package authorizer

import (
	"context"
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authz/common"
	"github.com/chef/automate/api/interservice/authz/v2"
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
	subjects []string, projects []string, req interface{}, minor v2.Version_VersionNumber) (context.Context, error) {

	// TODO drop
	fmt.Printf("HEY! here's the minor version in authorizer: %s\n\n", minor)
	newCtx, err := a.next.Handle(ctx, subjects, projects, req, minor)
	st := status.Convert(err)
	versionSwitch, updatedMinor := a.fromStatus(st)
	switch st.Code() {
	case codes.OK:
		return newCtx, nil
	case codes.FailedPrecondition:
		if versionSwitch {
			// TODO drop
			fmt.Printf("HEY! here's the updated minor version in authorizer: %s\n\n", updatedMinor)
			return a.Handle(ctx, subjects, projects, req, updatedMinor)
		}
		fallthrough
	default: // any other error status
		return ctx, err
	}
}

func (a *state) IsAuthorized(ctx context.Context, subjects []string,
	resourceV1, actionV1, resourceV2, actionV2 string,
) (middleware.AnnotatedAuthorizationResponse, error) {
	var (
		resp middleware.AuthorizationResponse
		err  error
	)
	switch a.next {
	case a.v1:
		resp, err = a.v1.IsAuthorized(ctx, subjects, resourceV1, actionV1)
		if err == nil {
			return annotate(resp, subjects, resourceV1, actionV1), nil
		}
	case a.v2:
		resp, err = a.v2.IsAuthorized(ctx, subjects, resourceV2, actionV2)
		if err == nil {
			return annotate(resp, subjects, resourceV2, actionV2), nil
		}
	}
	st := status.Convert(err)
	versionSwitch, _ := a.fromStatus(st)
	switch st.Code() {
	case codes.FailedPrecondition:
		if versionSwitch {
			return a.IsAuthorized(ctx, subjects, resourceV1, actionV1, resourceV2, actionV2)
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
				Pairs: resp,
				MapByResourceAndAction: mapByResourceAndActionV1, // passed back as-is
				MethodsInfo:            methodsInfoV1,            // to simplify processing
			}, nil
		}
	case a.v2:
		pairsV2 := pairs.GetKeys(mapByResourceAndActionV2)
		resp, err = a.v2.FilterAuthorizedPairs(ctx, subjects, pairsV2)
		if err == nil {
			return &middleware.FilterPairsResponse{
				Pairs: resp,
				MapByResourceAndAction: mapByResourceAndActionV2,
				MethodsInfo:            methodsInfoV2,
			}, nil
		}
	}
	st := status.Convert(err)
	versionSwitch, _ := a.fromStatus(st)
	switch st.Code() {
	case codes.FailedPrecondition:
		if versionSwitch {
			return a.FilterAuthorizedPairs(ctx, subjects,
				mapByResourceAndActionV1, mapByResourceAndActionV2,
				methodsInfoV1, methodsInfoV2)
		}
		fallthrough
	default: // any other error status
		return nil, err
	}
}

func (a *state) FilterAuthorizedProjects(ctx context.Context, subjects []string,
	mapByResourceAndActionV1, mapByResourceAndActionV2 map[pairs.Pair][]string,
	methodsInfoV1, methodsInfoV2 map[string]pairs.Info,
) (*middleware.FilterProjectsResponse, error) {
	var (
		resp []string
		err  error
	)
	switch a.next {
	case a.v1:
		pairsV1 := pairs.GetKeys(mapByResourceAndActionV1)
		resp, err = a.v1.FilterAuthorizedProjects(ctx, subjects, pairsV1)
		if err == nil {
			return &middleware.FilterProjectsResponse{
				Projects:               resp,
				MapByResourceAndAction: mapByResourceAndActionV1, // passed back as-is
				MethodsInfo:            methodsInfoV1,            // to simplify processing
			}, nil
		}
	case a.v2:
		pairsV2 := pairs.GetKeys(mapByResourceAndActionV2)
		resp, err := a.v2.FilterAuthorizedProjects(ctx, subjects, pairsV2)
		if err == nil {
			return &middleware.FilterProjectsResponse{
				Projects:               resp,
				MapByResourceAndAction: mapByResourceAndActionV2,
				MethodsInfo:            methodsInfoV2,
			}, nil
		}
	}
	st := status.Convert(err)
	versionSwitch, _ := a.fromStatus(st)
	switch st.Code() {
	case codes.FailedPrecondition:
		if versionSwitch {
			return a.FilterAuthorizedProjects(ctx, subjects,
				mapByResourceAndActionV1, mapByResourceAndActionV2,
				methodsInfoV1, methodsInfoV2)
		}
		fallthrough
	default: // any other error status
		return nil, err
	}
}

type annotated struct {
	r   middleware.AuthorizationResponse
	err error
}

func (r *annotated) Err() error {
	if r.r.GetAuthorized() {
		return nil
	}
	return r.err
}

func annotate(resp middleware.AuthorizationResponse, subjects []string, resource, action string) middleware.AnnotatedAuthorizationResponse {
	return &annotated{r: resp, err: fmt.Errorf("subject %q is not authorized to %q resource %q",
		subjects, action, resource)}
}

func (a *state) fromStatus(st *status.Status) (bool, v2.Version_VersionNumber) {
	for _, detail := range st.Details() {
		if _, ok := detail.(*common.ErrorShouldUseV1); ok {
			a.next = a.v1
			return true, v2.Version_V0
		}
		if _, ok := detail.(*common.ErrorShouldUseV2); ok {
			a.next = a.v2
			return true, v2.Version_V0
		}
		if _, ok := detail.(*common.ErrorShouldUseV2_1); ok {
			a.next = a.v2
			return true, v2.Version_V1
		}
	}
	return false, v2.Version_V0
}
