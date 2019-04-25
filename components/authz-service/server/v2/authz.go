package v2

import (
	"context"
	"fmt"
	"strings"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"

	"github.com/chef/automate/api/interservice/authz/common"
	api "github.com/chef/automate/api/interservice/authz/v2"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
)

// These do not have to be the same
// but seems reasonable to make them the same by convention.
type authzServer struct {
	log      logger.Logger
	engine   engine.V2Authorizer
	vSwitch  *VersionSwitch
	projects api.ProjectsServer
}

// NewAuthzServer returns a new IAM v2 Authz server.
func NewAuthzServer(l logger.Logger, e engine.V2Authorizer, v *VersionSwitch, p api.ProjectsServer) (api.AuthorizationServer, error) {
	return &authzServer{
		log:      l,
		engine:   e,
		vSwitch:  v,
		projects: p,
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
	// we check the version set in the channel on policy server
	// in order to determine whether or not to filter projects for the request
	version := s.vSwitch.Version
	var projects []string
	if isBeta2p1(version) {
		// we make this extra call to cover the case when the following are true:
		// - no project filter has been provided
		// - one statement allows All Projects for the given resource/action
		// - at least one statement denies 1 or more projects for the same resource/action
		// in order to return a complete list of projects exclusive of the denied projects,
		// we must provide ProjectsAuthorized with the list of all projects instead of an empty list
		if len(req.ProjectsFilter) == 0 {
			pairs := []*api.Pair{
				&api.Pair{
					Resource: req.Resource,
					Action:   req.Action,
				},
			}
			// this call checks for all allowed projects (without considering deny)
			filterReq := api.FilterAuthorizedPairsReq{
				Subjects: req.Subjects,
				Pairs:    pairs,
			}
			allowed, err := s.FilterAuthorizedProjects(ctx, &filterReq)
			if err != nil {
				return nil, status.Error(codes.Internal, err.Error())
			}

			projects = allowed.Projects
		} else {
			projects = req.ProjectsFilter
		}
	} else {
		// if IAM version is set to v2.0
		// we override the requested projects because no filter should be applied on v2
		projects = []string{}
	}

	// this call returns allowed projects that are not overridden by deny
	projectsAuthorized, err := s.engine.V2ProjectsAuthorized(ctx,
		engine.Subjects(req.Subjects),
		engine.Action(req.Action),
		engine.Resource(req.Resource),
		engine.ProjectList(projects...))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	if !isBeta2p1(version) && len(projectsAuthorized) > 0 {
		// if IAM version is set to v2.0
		// as long as at least one project is authorized
		// we override the filtered projects because no filter should be applied on v2.0
		projectsAuthorized = []string{constants.AllProjectsExternalID}
	}

	if stringutils.SliceContains(projectsAuthorized, constants.AllProjectsID) {
		// Though incoming requests signify All Projects with an empty array
		// we cannot return an empty array here because when the engine returns an empty array
		// it means No Projects Allowed
		// so instead, here we set All Projects as *, to be passed on to the domain services
		// this is more explicit and avoids the issue of golang coercing empty arrays into nil
		projectsAuthorized = []string{constants.AllProjectsExternalID}
	}

	// TODO bd: parse result and send gateway different errors based on version

	s.logProjectQuery(req, projectsAuthorized)
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

type VersionSwitch struct {
	Version api.Version
}

func (v *VersionSwitch) Interceptor(ctx context.Context,
	req interface{},
	info *grpc.UnaryServerInfo,
	handler grpc.UnaryHandler) (interface{}, error) {
	// For Authorization calls (IsAuthorized, FilterAuthorizedPairs), we decline
	// cross-overs.
	//
	// Note: v2 policy related calls have their own service, so, for example, the
	// endpoint for retrieving whether IAMv1 or v2 is used, GetPolicyVersion, is
	// "/chef.automate.domain.authz.v2.Policies/GetPolicyVersion", and thus
	// exempt from this version check.

	// These methods skip the check, though they are in the relevant service
	// definition:
	switch info.FullMethod {
	case "/chef.automate.domain.authz.Authorization/GetVersion":
		return handler(ctx, req)
	}

	v1Req := strings.HasPrefix(info.FullMethod, "/chef.automate.domain.authz.Authorization/")
	v2Req := strings.HasPrefix(info.FullMethod, "/chef.automate.domain.authz.v2.Authorization/")

	if v.Version.Major == api.Version_V2 && v1Req {
		st := status.New(codes.FailedPrecondition, "authz-service set to v2")
		st, err := st.WithDetails(&common.ErrorShouldUseV2{})
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to add details to err: %v", err)
		}
		return nil, st.Err()
	}
	if v.Version.Major == api.Version_V1 && v2Req {
		st := status.New(codes.FailedPrecondition, "authz-service set to v1")
		st, err := st.WithDetails(&common.ErrorShouldUseV1{})
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to add details to err: %v", err)
		}
		return nil, st.Err()
	}
	return handler(ctx, req)
}

func NewSwitch(c chan api.Version) *VersionSwitch {
	x := VersionSwitch{
		Version: api.Version{
			Major: api.Version_V1,
			Minor: api.Version_V0,
		},
	}
	go func() {
		for {
			select {
			case v := <-c:
				x.Version = v
			}
		}
	}()
	return &x
}

func isBeta2p1(version api.Version) bool {
	return version.Major == api.Version_V2 && version.Minor == api.Version_V1
}
