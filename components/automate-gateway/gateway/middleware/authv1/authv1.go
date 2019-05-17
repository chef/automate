package authv1

import (
	"context"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	"github.com/chef/automate/components/automate-gateway/api/authz/policy"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
)

type client struct {
	client authz.AuthorizationClient
}

func (c *client) Handle(ctx context.Context, subjects []string, _ []string,
	req interface{},
) (context.Context, error) {
	log := ctxlogrus.Extract(ctx)

	method, ok := grpc.Method(ctx)
	if !ok {
		return nil, status.Error(codes.Internal, "cannot retrieve method info")
	}

	polInfo := policy.InfoForMethod(method, req)
	if polInfo == nil {
		log.Warnf("no v1 policy annotation for method %s", method)
		return nil, status.Errorf(codes.Internal,
			"missing policy info for method %q", method)
	}

	action, resource := polInfo.Action, polInfo.Resource
	if action == "" || resource == "" {
		log.Warnf("no v1 policy annotation for method %s", method)
		return nil, status.Errorf(codes.Internal,
			"missing policy info for method %q", method)
	}

	// We have policyInfo here, use it to determine authorization:
	ctxlogrus.AddFields(ctx, logrus.Fields{
		"auth.subjects": subjects,
		"auth.resource": resource,
		"auth.action":   action,
		"iam.version":   "iam_v1",
	})
	// Note: if ANYTHING goes wrong, 403 is the error we return. This is done
	// on purpose, so our authz response doesn't leak information about what
	// is happening internally.
	resp, err := c.client.IsAuthorized(ctx, &authz.IsAuthorizedReq{
		Subjects: subjects,
		Resource: resource,
		Action:   action,
	})
	if err != nil {
		if status.Convert(err).Code() == codes.FailedPrecondition {
			return nil, err
		}
		log.WithError(err).Error("error authorizing request")
		return nil, status.Errorf(codes.PermissionDenied,
			"error authorizing action %q on resource %q for subjects %q: %s",
			action, resource, subjects, err.Error())
	}
	if !resp.GetAuthorized() {
		return nil, status.Errorf(codes.PermissionDenied,
			"unauthorized action %q on resource %q for subjects %q",
			action, resource, subjects)
	}

	projects := []string{auth_context.AllProjectsKey}
	return auth_context.NewContext(ctx, subjects, projects, resource, action, middleware.AuthV1.String()), nil
}

type resp struct {
	ctx        context.Context
	*authz.IsAuthorizedResp
}

func (r *resp) Ctx() context.Context {
	return r.ctx
}

func (c *client) IsAuthorized(ctx context.Context, subjects []string, resource, action string,
	_ []string, // projects aren't used
) (middleware.AuthorizationResponse, error) {
	r, err :=  c.client.IsAuthorized(ctx, &authz.IsAuthorizedReq{
		Subjects: subjects,
		Resource: resource,
		Action:   action,
	})
	return &resp{IsAuthorizedResp: r, ctx: ctx}, err
}

func (c *client) FilterAuthorizedPairs(ctx context.Context, subjects []string, inputPairs []*pairs.Pair,
) ([]*pairs.Pair, error) {
	pairsV1 := make([]*authz.Pair, len(inputPairs))
	for i, p := range inputPairs {
		pairsV1[i] = &authz.Pair{Resource: p.Resource, Action: p.Action}
	}

	resp, err := c.client.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
		Subjects: subjects,
		Pairs:    pairsV1,
	})
	if err != nil {
		return nil, err
	}
	respPairs := make([]*pairs.Pair, len(resp.Pairs))
	for i, p := range resp.Pairs {
		respPairs[i] = &pairs.Pair{Resource: p.Resource, Action: p.Action}
	}
	return respPairs, nil
}

func (c *client) FilterAuthorizedProjects(ctx context.Context, subjects []string, inputPairs []*pairs.Pair,
) ([]string, error) {
	pairsV1 := make([]*authz.Pair, len(inputPairs))
	for i, p := range inputPairs {
		pairsV1[i] = &authz.Pair{Resource: p.Resource, Action: p.Action}
	}

	// Need to make this call to allow auto-switching to v2 if needed
	_, err := c.client.FilterAuthorizedProjects(ctx, &authz.FilterAuthorizedPairsReq{
		Subjects: subjects,
		Pairs:    pairsV1,
	})
	if err != nil {
		return nil, err
	}
	return []string{"~v1~"}, nil // magic V1 identifier
}

func AuthorizationHandler(cl authz.AuthorizationClient) middleware.AuthorizationHandler {
	return &client{client: cl}
}
