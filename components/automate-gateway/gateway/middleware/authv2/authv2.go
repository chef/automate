package authv2

import (
	"context"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	authz "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	policy "github.com/chef/automate/components/automate-gateway/authz/policy_v2"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
)

type client struct {
	client authz.AuthorizationClient
}

// Note(sr): The Handle method is V2-only code. We can do anything here -- deal
// with incoming project headers, inject headers for downstream, etc.

func (c *client) Handle(ctx context.Context, subjects []string, projectsToFilter []string,
	req interface{},
) (context.Context, error) {
	log := ctxlogrus.Extract(ctx)
	method, ok := grpc.Method(ctx)
	if !ok {
		return nil, status.Error(codes.Internal, "cannot retrieve method info")
	}

	polInfo := policy.InfoForMethod(method, req)
	if polInfo == nil {
		log.Warnf("no v2 policy annotation for method %s", method)
		return nil, status.Errorf(codes.Internal,
			"missing policy info for method %q", method)
	}

	action, resource := polInfo.Action, polInfo.Resource
	if action == "" || resource == "" {
		log.Warnf("no v2 policy annotation for method %s", method)
		return nil, status.Errorf(codes.Internal,
			"missing policy info for method %q", method)
	}

	// We have policyInfo here, use it to determine authorization:
	ctxlogrus.AddFields(ctx, logrus.Fields{
		"auth.subjects": subjects,
		"auth.resource": resource,
		"auth.action":   action,
		"iam.version":   "iam_v2",
	})

	// Note: if ANYTHING goes wrong, 403 is the error we return. This is done
	// on purpose, so our authz response doesn't leak information about what
	// is happening internally.
	filteredResp, err := c.client.ProjectsAuthorized(ctx, &authz.ProjectsAuthorizedReq{
		Subjects:       subjects,
		Resource:       resource,
		Action:         action,
		ProjectsFilter: projectsToFilter,
	})
	if err != nil {
		if status.Convert(err).Code() == codes.FailedPrecondition {
			return nil, err
		}
		log.WithError(err).Error("error authorizing request")
		return nil, status.Errorf(codes.PermissionDenied,
			"error authorizing action %q on resource %q for subjects %q filtered by project list %q: %s",
			action, resource, subjects, projectsToFilter, err.Error())
	}
	if len(filteredResp.Projects) == 0 {
		return nil, status.Errorf(codes.PermissionDenied,
			"unauthorized: subjects %q has no project access for action %q on resource %q "+
				"(filtered by project list %q)",
			action, resource, subjects, projectsToFilter)
	}
	projects := filteredResp.Projects

	return auth_context.NewContext(ctx, subjects, projects, resource, action, middleware.AuthV2.String()), nil
}

func (c *client) IsAuthorized(ctx context.Context, subjects []string, resource, action string,
) (middleware.AuthorizationResponse, error) {
	return c.client.IsAuthorized(ctx, &authz.IsAuthorizedReq{
		Subjects: subjects,
		Resource: resource,
		Action:   action,
	})
}

func (c *client) FilterAuthorizedPairs(ctx context.Context, subjects []string, inputPairs []*pairs.Pair,
) ([]*pairs.Pair, error) {
	pairsV2 := make([]*authz.Pair, len(inputPairs))
	for i, p := range inputPairs {
		pairsV2[i] = &authz.Pair{Resource: p.Resource, Action: p.Action}
	}

	resp, err := c.client.FilterAuthorizedPairs(ctx, &authz.FilterAuthorizedPairsReq{
		Subjects: subjects,
		Pairs:    pairsV2,
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
	pairsV2 := make([]*authz.Pair, len(inputPairs))
	for i, p := range inputPairs {
		pairsV2[i] = &authz.Pair{Resource: p.Resource, Action: p.Action}
	}

	resp, err := c.client.FilterAuthorizedProjects(ctx, &authz.FilterAuthorizedPairsReq{
		Subjects: subjects,
		Pairs:    pairsV2,
	})
	if err != nil {
		return nil, err
	}
	respProjects := make([]string, len(resp.Projects))
	copy(respProjects, resp.Projects)
	return respProjects, nil
}

func AuthorizationHandler(cl authz.AuthorizationClient) middleware.AuthorizationHandler {
	return &client{client: cl}
}
