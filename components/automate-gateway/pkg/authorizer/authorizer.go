package authorizer

import (
	"context"
	"fmt"

	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
)

type state struct {
	v2 middleware.AuthorizationHandler
}

func NewAuthorizer(v2 middleware.AuthorizationHandler) middleware.SwitchingAuthorizationHandler {
	return &state{v2: v2}
}

func (a *state) Handle(ctx context.Context,
	subjects []string, projects []string, req interface{}) (context.Context, error) {
	return a.Handle(ctx, subjects, projects, req)
}

func (a *state) IsAuthorized(ctx context.Context, subjects []string,
	resourceV2, actionV2 string, projects []string,
) (middleware.AnnotatedAuthorizationResponse, error) {
	resp, err := a.v2.IsAuthorized(ctx, subjects, resourceV2, actionV2, projects)
	if err == nil {
		return annotate(resp, subjects, resourceV2, actionV2), nil
	}
	return nil, err
}

func (a *state) FilterAuthorizedPairs(ctx context.Context, subjects []string,
	mapByResourceAndActionV2 map[pairs.Pair][]string,
	methodsInfoV2 map[string]pairs.Info,
) (*middleware.FilterPairsResponse, error) {
	return a.FilterAuthorizedPairs(ctx, subjects, mapByResourceAndActionV2, methodsInfoV2)
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
