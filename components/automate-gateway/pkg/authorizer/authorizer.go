package authorizer

import (
	"context"
	"fmt"

	"github.com/chef/automate/components/automate-gateway/api/iam/v2/pairs"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
)

type state struct {
	authHandler middleware.AuthorizationHandler
}

func NewAuthorizer(authHandler middleware.AuthorizationHandler) middleware.SwitchingAuthorizationHandler {
	return &state{authHandler: authHandler}
}

func (a *state) Handle(ctx context.Context,
	subjects []string, projects []string, req interface{}) (context.Context, error) {
	return a.authHandler.Handle(ctx, subjects, projects, req)
}

func (a *state) IsAuthorized(ctx context.Context, subjects []string,
	resource, action string, projects []string,
) (middleware.AnnotatedAuthorizationResponse, error) {
	resp, err := a.authHandler.IsAuthorized(ctx, subjects, resource, action, projects)
	if err == nil {
		return annotate(resp, subjects, resource, action), nil
	}
	return nil, err
}

func (a *state) FilterAuthorizedPairs(ctx context.Context, subjects []string,
	mapByResourceAndAction map[pairs.Pair][]string,
	methodsInfo map[string]pairs.Info,
) (*middleware.FilterPairsResponse, error) {
	pairs := pairs.GetKeys(mapByResourceAndAction)
	resp, err := a.authHandler.FilterAuthorizedPairs(ctx, subjects, pairs)
	if err == nil {
		return &middleware.FilterPairsResponse{
			MapByResourceAndAction: mapByResourceAndAction,
			MethodsInfo:            methodsInfo,
			Pairs:                  resp,
		}, nil
	}
	return nil, err

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
