package v2

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/authz/v2"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/storage/v2/postgres"
	"github.com/chef/automate/lib/projectassignment"
)

// These do not have to be the same
// but seems reasonable to make them the same by convention.
type authzServer struct {
	log      logger.Logger
	engine   engine.V2Authorizer
	projects api.ProjectsServer
	store    storage.Storage
}

// NewPostgresAuthzServer returns a new IAM v2 Authz server.
func NewPostgresAuthzServer(l logger.Logger, e engine.V2Authorizer, p api.ProjectsServer) (api.AuthorizationServer, error) {
	s := postgres.GetInstance()
	if s == nil {
		return nil, errors.New("postgres v2 singleton not yet initialized for authz server")
	}
	return NewAuthzServer(l, e, p, s)
}

func NewAuthzServer(l logger.Logger, e engine.V2Authorizer, p api.ProjectsServer, s storage.Storage) (api.AuthorizationServer, error) {
	return &authzServer{
		log:      l,
		engine:   e,
		projects: p,
		store:    s,
	}, nil
}

func (s *authzServer) ProjectsAuthorized(
	ctx context.Context,
	req *api.ProjectsAuthorizedReq) (*api.ProjectsAuthorizedResp, error) {
	var authorizedProjects []string

	requestedProjects := req.ProjectsFilter

	allProjectsRequested := len(req.ProjectsFilter) == 0
	var allProjects []string
	var err error
	if allProjectsRequested {
		allProjects, err = s.getAllProjects(ctx)
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}
		requestedProjects = allProjects
	}

	engineResp, err := s.engine.V2ProjectsAuthorized(ctx,
		engine.Subjects(req.Subjects),
		engine.Action(req.Action),
		engine.Resource(req.Resource),
		engine.ProjectList(requestedProjects...))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	authorizedProjects = engineResp
	if allProjectsRequested {
		authorizedProjects = handleAllProjectsResponse(engineResp, allProjects)
	}

	s.logProjectQuery(req, authorizedProjects)
	return &api.ProjectsAuthorizedResp{
		Projects: authorizedProjects,
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
	req *api.FilterAuthorizedProjectsReq) (*api.FilterAuthorizedProjectsResp, error) {

	// Introspection needs unfiltered access.
	ctx = auth_context.ContextWithoutProjects(ctx)

	resp, err := s.engine.V2FilterAuthorizedProjects(ctx, engine.Subjects(req.Subjects))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	var projectIDs []string
	if stringutils.SliceContains(resp, constants.AllProjectsID) {
		list, err := s.projects.ListProjects(ctx, &api.ListProjectsReq{})
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}
		for _, project := range list.Projects {
			projectIDs = append(projectIDs, project.Id)
		}

		// all projects also includes unassigned
		projectIDs = append(projectIDs, constants.UnassignedProjectID)
	} else {
		projectIDs = resp
	}

	return &api.FilterAuthorizedProjectsResp{
		Projects: projectIDs,
	}, nil
}

// TODO (tc) Until projects are transactionally cleaned up from all data everywhere
// on project delete, this might fail when it shouldn't (aka the project no longer exists
// that is being removed from some object), since the project won't be found or authorized
// for iam:projects:assign. We should make cleaning up projects on project delete from IAM
// objects throughout the system transactional.
//
// ValidateProjectAssignment assesses if a set of subjects is authorized to reassign a set
// of projects. It will return nil for the error if successful.
// If unsuccessful it will return an error when:
//
// 1) One or more of the projects passed do not actual exist (returns NotFound).
// 2) The set of subjects was not authorized on one or more of the projects (returns PermissionsDenied).
//
// ValidateProjectAssignment is only used externally to authz-service to avoid circular dependency issues.
// When checking project assignment permissions internally, the relevant bits of this function are used
// directly in the v2/postgres.go code.
func (s *authzServer) ValidateProjectAssignment(
	ctx context.Context,
	req *api.ValidateProjectAssignmentReq) (*api.ValidateProjectAssignmentResp, error) {

	newProjects := req.NewProjects
	if len(newProjects) == 0 {
		newProjects = []string{}
	}
	oldProjects := req.OldProjects
	if len(oldProjects) == 0 {
		oldProjects = []string{}
	}

	if len(newProjects) != 0 {
		err := s.store.EnsureNoProjectsMissing(ctx, newProjects)
		if err != nil {
			if _, ok := err.(*projectassignment.ProjectsMissingError); ok {
				return nil, status.Error(codes.NotFound, err.Error())
			}
			return nil, status.Error(codes.Internal, err.Error())
		}
	}

	err := projectassignment.AuthorizeProjectAssignment(ctx, s.engine, req.Subjects,
		oldProjects, newProjects, req.IsUpdateRequest)
	if err != nil {
		if _, ok := err.(*projectassignment.ProjectsUnauthorizedForAssignmentErr); ok {
			return nil, status.Error(codes.PermissionDenied, err.Error())
		}
		if _, ok := err.(*projectassignment.InvalidCreateRequest); ok {
			return nil, status.Error(codes.InvalidArgument, err.Error())
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.ValidateProjectAssignmentResp{}, nil
}

func (s *authzServer) getAllProjects(ctx context.Context) ([]string, error) {
	// Need unfiltered access to the projects list.
	ctx = auth_context.ContextWithoutProjects(ctx)

	// we make this extra call to cover the case when the following are true:
	// - no project filter has been provided
	// - one statement allows All Projects for the given resource/action
	// - at least one statement denies 1 or more projects for the same resource/action
	// in order to return a complete list of projects exclusive of the denied projects,
	// we must provide the engine with the list of all projects instead of an empty list
	list, err := s.projects.ListProjects(ctx, &api.ListProjectsReq{})
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}
	projectIDs := make([]string, len(list.Projects)+1)
	for i, project := range list.Projects {
		projectIDs[i] = project.Id
	}
	// ListProjects never returns UnassignedProjectID
	// because we do not want it to show up in the ListProjects view;
	// vital to have it here though, so add it back!
	projectIDs[len(list.Projects)] = constants.UnassignedProjectID

	return projectIDs, nil
}

// handleAllProjectsResponse sets the response to * if all projects returned
func handleAllProjectsResponse(authorizedProjects, allProjects []string) []string {
	// though incoming requests signify All Projects with an empty array
	// we cannot return an empty array here because when the engine returns an empty array
	// it means No Projects Allowed.
	// so instead, here we set All Projects as *, to be passed on to the domain services
	// this is more explicit and avoids the issue of golang coercing empty arrays into nil
	if len(authorizedProjects) == len(allProjects) {
		authorizedProjects = []string{constants.AllProjectsExternalID}
	}
	return authorizedProjects
}

func (s *authzServer) logProjectQuery(req *api.ProjectsAuthorizedReq, authorizedProjects []string) {
	s.log.WithFields(logger.KV{
		"result":   authorizedProjects,
		"subject":  req.Subjects,
		"action":   req.Action,
		"resource": req.Resource,
		"projects": req.ProjectsFilter,
	}).Info("Projects Authorized Query")
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
