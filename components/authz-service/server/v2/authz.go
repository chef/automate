package v2

import (
	"context"
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"

	api "github.com/chef/automate/api/interservice/authz/v2"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
)

// These do not have to be the same
// but seems reasonable to make them the same by convention.
type authzServer struct {
	log    logger.Logger
	engine engine.V2Authorizer
}

// NewAuthzServer returns a new IAM v2 Authz server.
func NewAuthzServer(l logger.Logger, e engine.V2Authorizer) (api.AuthorizationServer, error) {
	return &authzServer{
		log:    l,
		engine: e,
	}, nil
}

func (s *authzServer) IsAuthorized(
	ctx context.Context,
	req *api.IsAuthorizedReq) (*api.IsAuthorizedResp, error) {

	authorized, err := s.engine.V2IsAuthorized(ctx,
		engine.Subjects(req.Subjects),
		engine.Action(req.Action),
		engine.Resource(req.Resource))
	s.logQuery(req, authorized, err)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.IsAuthorizedResp{
		Authorized: authorized,
	}, nil
}

func (s *authzServer) ProjectsAuthorized(
	ctx context.Context,
	req *api.ProjectsAuthorizedReq) (*api.ProjectsAuthorizedResp, error) {

	projectsAuthorized, err := s.engine.V2ProjectsAuthorized(ctx,
		engine.Subjects(req.Subjects),
		engine.Action(req.Action),
		engine.Resource(req.Resource),
		engine.ProjectList(req.ProjectsFilter...))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}
	// Generally we return the engine's response verbatim
	// but there are two cases that need to be intercepted and adjusted.
	if stringutils.SliceContains(projectsAuthorized, constants.AllProjectsID) {
		if len(req.ProjectsFilter) == 0 {
			// Engine allows all and we requested all, so signify it as all.
			// This must be different than the requested notion of all,
			// an empty array, because an empty array coming back from the engine means none!
			projectsAuthorized = []string{constants.AllProjectsExternalID}
		} else {
			// Engine allows all--but we want that to mean just the *requested* ones.
			projectsAuthorized = req.ProjectsFilter
		}
	}

	return &api.ProjectsAuthorizedResp{
		Projects: projectsAuthorized,
	}, nil
}

func (s *authzServer) FilterAuthorizedPairs(
	ctx context.Context,
	req *api.FilterAuthorizedPairsReq) (*api.FilterAuthorizedPairsResp, error) {
	resp, err := s.engine.V2FilterAuthorizedPairs(ctx,
		engine.Subjects(req.Subjects),
		toEnginePairs(req.Pairs))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.FilterAuthorizedPairsResp{
		Pairs: toPBPairs(resp),
	}, nil
}

func (s *authzServer) FilterAuthorizedProjects(
	ctx context.Context,
	req *api.FilterAuthorizedPairsReq) (*api.FilterAuthorizedProjectsResp, error) {
	resp, err := s.engine.V2FilterAuthorizedProjects(ctx,
		engine.Subjects(req.Subjects),
		toEnginePairs(req.Pairs))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.FilterAuthorizedProjectsResp{
		Projects: resp,
	}, nil
}

func (s *authzServer) logQuery(req *api.IsAuthorizedReq, authorized bool, err error) {
	result := fmt.Sprintf("%t", authorized)
	if err != nil {
		result = err.Error()
	}
	s.log.WithFields(logger.KV{
		"result":   result,
		"subject":  req.Subjects,
		"action":   req.Action,
		"resource": req.Resource,
	}).Info("Authorization Query")
}

func toPBPairs(pairs []engine.Pair) []*api.Pair {
	ps := make([]*api.Pair, len(pairs))
	for i, p := range pairs {
		ps[i] = &api.Pair{Resource: string(p.Resource), Action: string(p.Action)}
	}
	return ps
}

func toEnginePairs(pairs []*api.Pair) []engine.Pair {
	ps := make([]engine.Pair, len(pairs))
	for i, p := range pairs {
		ps[i] = engine.Pair{Resource: engine.Resource(p.Resource), Action: engine.Action(p.Action)}
	}
	return ps
}
