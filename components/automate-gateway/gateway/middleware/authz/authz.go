package authz

import (
	"context"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/external/iam/v2/pairs"
	"github.com/chef/automate/api/external/iam/v2/policy"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
)

type client struct {
	client authz.AuthorizationServiceClient
}

// Handle takes care of authorization in the gRPC middleware
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
		log.Warnf("no policy annotation for method %s", method)
		return nil, status.Errorf(codes.Internal,
			"missing policy info for method %q", method)
	}

	action, resource := polInfo.Action, polInfo.Resource
	if action == "" || resource == "" {
		log.Warnf("no policy annotation for method %s", method)
		return nil, status.Errorf(codes.Internal,
			"missing policy info for method %q", method)
	}

	// We have policyInfo here, use it to determine authorization:
	ctxlogrus.AddFields(ctx, logrus.Fields{
		"auth.subjects": subjects,
		"auth.resource": resource,
		"auth.action":   action,
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
			"error authorizing request")
	}
	if len(filteredResp.Projects) == 0 {
		log.Warnf("unauthorized: members %q cannot perform action %q on resource %q filtered by projects %q",
			subjects, action, resource, projectsToFilter)
		return nil, status.Errorf(codes.PermissionDenied,
			"unauthorized members")
	}
	projects := filteredResp.Projects

	return auth_context.NewContext(ctx, subjects, projects, resource, action), nil
}

// IsAuthorized takes care of HTTP authorization in the custom HTTP handlers
// It returns a context with the auth data including projects injected, a
// boolean indicating if authorization was successful (which means there's
// more than one project returned from the filtering), and an error.
func (c *client) IsAuthorized(ctx context.Context, subjects []string, resource, action string,
	projectsToFilter []string) (_ context.Context, authorized bool, _ error) {
	log := ctxlogrus.Extract(ctx)
	filteredResp, err := c.client.ProjectsAuthorized(ctx, &authz.ProjectsAuthorizedReq{
		Subjects:       subjects,
		Resource:       resource,
		Action:         action,
		ProjectsFilter: projectsToFilter,
	})
	if err != nil {
		if status.Convert(err).Code() == codes.FailedPrecondition {
			return nil, false, err
		}
		log.WithError(err).Error("error authorizing request")
		// something went wrong in some unexpected way -- we'll "fail shut", i.e., deny authorization
		return nil, false, errors.Errorf("error authorizing action %q on resource %q for members %q: %s",
			action, resource, subjects, err.Error())
	}
	projects := filteredResp.Projects

	return auth_context.NewContext(ctx, subjects, projects, resource, action),
		len(projects) != 0,
		nil
}

// FilterAuthorizedPairs drives authz introspection
// Q: There's no projects here! This can't be right!
// A(msorens): The short answer to that is introspection is not yet project-aware.
//
//	It is on the list of things to do... maybe.
//	For the current uses of introspection we have been able to get by without projects.
//	Should the user see this button? ... this page? .. this item in the nav bar?
//	Where introspection-plus-projects would be needed is, for example, on the Delete
//	button on individual rows on projects list, tokens list, teams list, etc. We had
//	actually wired that up using introspection-sans-projects and immediately ran into
//	performance issues: it is unacceptable to do a separate introspection network call
//	for each row in a table. So we took that out again for the time being.
//	You have probably already observed that for non-parameterized introspection it is
//	now highly optimized, doing a single IntrospectAll call once the front-end cache
//	is stale enough (I think it invalidates in 60 seconds or so).
//	The most likely strategy for introducing parameterized introspection back in the
//	mix is doing a single IntrospectSome passing all the ids on a given list at one
//	time. And then we need to figure out projects, too.
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

func AuthorizationHandler(cl authz.AuthorizationServiceClient) middleware.AuthorizationHandler {
	return &client{client: cl}
}
