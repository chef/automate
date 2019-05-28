package authorizer

import (
	"context"
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authz/common"
	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
)

type state struct {
	v1, v2, next middleware.AuthorizationHandler
}

func NewAuthorizer(v1, v2 middleware.AuthorizationHandler) middleware.SwitchingAuthorizationHandler {
	return &state{v1: v1, v2: v2, next: v1}
}

func (a *state) Handle(ctx context.Context,
	subjects []string, projects []string, req interface{}) (context.Context, error) {

	newCtx, err := a.next.Handle(ctx, subjects, projects, req)
	st := status.Convert(err)
	switch st.Code() {
	case codes.OK:
		return newCtx, nil
	case codes.FailedPrecondition:
		if a.fromStatus(st) {
			return a.Handle(ctx, subjects, projects, req)
		}
		fallthrough
	default: // any other error status
		return ctx, err
	}
}

func (a *state) IsAuthorized(ctx context.Context, subjects []string,
	resourceV1, actionV1, resourceV2, actionV2 string, projects []string,
) (middleware.AnnotatedAuthorizationResponse, error) {
	var (
		resp middleware.AuthorizationResponse
		err  error
	)
	switch a.next {
	case a.v1:
		resp, err = a.v1.IsAuthorized(ctx, subjects, resourceV1, actionV1, nil) // projects are not used  here
		if err == nil {
			return annotate(resp, subjects, resourceV1, actionV1), nil
		}
	case a.v2:
		resp, err = a.v2.IsAuthorized(ctx, subjects, resourceV2, actionV2, projects)
		if err == nil {
			return annotate(resp, subjects, resourceV2, actionV2), nil
		}
	}
	st := status.Convert(err)
	switch st.Code() {
	case codes.FailedPrecondition:
		if a.fromStatus(st) {
			return a.IsAuthorized(ctx, subjects, resourceV1, actionV1, resourceV2, actionV2, projects)
		}
		fallthrough
	default: // any other error status
		return nil, err
	}
}

func (a *state) FilterAuthorizedPairs(ctx context.Context, subjects []string,
	mapByResourceAndActionV1, mapByResourceAndActionV2 map[pairs.Pair][]string,
	methodsInfoV1, methodsInfoV2 map[string]pairs.Info,
) (*middleware.FilterPairsResponse, error) {
	var (
		resp []*pairs.Pair
		err  error
	)
	switch a.next {
	case a.v1:
		pairsV1 := pairs.GetKeys(mapByResourceAndActionV1)
		resp, err = a.v1.FilterAuthorizedPairs(ctx, subjects, pairsV1)
		if err == nil {
			return &middleware.FilterPairsResponse{
				Pairs:                  resp,
				MapByResourceAndAction: mapByResourceAndActionV1, // passed back as-is
				MethodsInfo:            methodsInfoV1,            // to simplify processing
			}, nil
		}
	case a.v2:
		pairsV2 := pairs.GetKeys(mapByResourceAndActionV2)
		resp, err = a.v2.FilterAuthorizedPairs(ctx, subjects, pairsV2)
		if err == nil {
			return &middleware.FilterPairsResponse{
				Pairs:                  resp,
				MapByResourceAndAction: mapByResourceAndActionV2,
				MethodsInfo:            methodsInfoV2,
			}, nil
		}
	}
	st := status.Convert(err)
	switch st.Code() {
	case codes.FailedPrecondition:
		if a.fromStatus(st) {
			return a.FilterAuthorizedPairs(ctx, subjects,
				mapByResourceAndActionV1, mapByResourceAndActionV2,
				methodsInfoV1, methodsInfoV2)
		}
		fallthrough
	default: // any other error status
		return nil, err
	}
}

type annotated struct {
	middleware.AuthorizationResponse
	err error
}

func (r *annotated) Err() error {
	if r.GetAuthorized() {
		return nil
	}
	return r.err
}

func annotate(resp middleware.AuthorizationResponse, subjects []string, resource, action string) middleware.AnnotatedAuthorizationResponse {
	return &annotated{AuthorizationResponse: resp,
		err: fmt.Errorf("subject %q is not authorized to %q resource %q",
			subjects, action, resource)}
}

func (a *state) fromStatus(st *status.Status) bool {
	for _, detail := range st.Details() {
		if _, ok := detail.(*common.ErrorShouldUseV1); ok {
			a.next = a.v1
			return true
		}
		if _, ok := detail.(*common.ErrorShouldUseV2); ok {
			a.next = a.v2
			return true
		}
	}
	return false
}
