package v2

import (
	"context"

	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"

	api "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/authz-service/engine"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/storage/v2/memstore"
	"github.com/chef/automate/components/authz-service/storage/v2/postgres"
	event "github.com/chef/automate/components/event-service/server"
	log "github.com/sirupsen/logrus"
)

// state the server state for projects
type state struct {
	log                  logger.Logger
	store                storage.Storage
	engine               engine.ProjectRulesRetriever
	projectUpdateManager ProjectUpdateManager
}

// NewMemstoreProjectsServer returns an instance of api.ProjectsServer
func NewMemstoreProjectsServer(
	ctx context.Context,
	l logger.Logger,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
) (api.ProjectsServer, error) {

	return NewProjectsServer(ctx, l, memstore.New(), e, eventServiceClient)
}

// NewPostgresProjectsServer instantiates a ProjectsServer using a PG store
func NewPostgresProjectsServer(
	ctx context.Context,
	l logger.Logger,
	migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
) (api.ProjectsServer, error) {

	s, err := postgres.New(ctx, l, migrationsConfig, dataMigrationsConfig)
	if err != nil {
		return nil, errors.Wrap(err, "failed to initialize v2 store state")
	}
	return NewProjectsServer(ctx, l, s, e, eventServiceClient)
}

func NewProjectsServer(
	ctx context.Context,
	l logger.Logger,
	s storage.Storage,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
) (api.ProjectsServer, error) {

	return &state{
		log:                  l,
		store:                s,
		engine:               e,
		projectUpdateManager: NewProjectUpdateManager(eventServiceClient),
	}, nil
}

func (s *state) GetProject(ctx context.Context,
	req *api.GetProjectReq) (*api.GetProjectResp, error) {
	p, err := s.store.GetProject(ctx, req.Id)
	if err != nil {
		if err == storage_errors.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "could not find project with ID %q", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error retrieving project with ID %q: %s", req.Id, err.Error())
	}

	apiProject, err := fromStorageProject(p)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting project with ID %q: %s", p.ID, err.Error())
	}
	return &api.GetProjectResp{Project: apiProject}, nil
}

func (s *state) CreateProject(ctx context.Context,
	req *api.CreateProjectReq) (*api.CreateProjectResp, error) {
	p, err := storage.NewProject(req.Id, req.Name, storage.Custom)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"creating project with ID %q: %s", req.Id, err.Error())
	}
	resp, err := s.store.CreateProject(ctx, &p)
	if err != nil {
		if err == storage_errors.ErrConflict {
			return nil, status.Errorf(codes.AlreadyExists, "project with ID %q already exists", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error retrieving project with ID %q: %s", req.Id, err.Error())
	}

	apiProject, err := fromStorageProject(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting project with ID %q: %s", resp.ID, err.Error())
	}
	return &api.CreateProjectResp{Project: apiProject}, nil
}

func (s *state) UpdateProject(ctx context.Context,
	req *api.UpdateProjectReq) (*api.UpdateProjectResp, error) {
	p, err := storage.NewProject(req.Id, req.Name, storage.Custom)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"updating project with ID %q: %s", req.Id, err.Error())
	}

	resp, err := s.store.UpdateProject(ctx, &p)
	if err != nil {
		if err == storage_errors.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "project with ID %q not found", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error retrieving project with ID %q: %s", req.Id, err.Error())
	}

	apiProject, err := fromStorageProject(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting project with ID %q: %s", resp.ID, err.Error())
	}

	err = s.projectUpdateManager.Start()
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error starting project update %q: %s", resp.ID, err.Error())
	}

	return &api.UpdateProjectResp{Project: apiProject}, nil
}

func (s *state) ListProjects(ctx context.Context,
	_ *api.ListProjectsReq) (*api.ListProjectsResp, error) {
	ps, err := s.store.ListProjects(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "error retrieving projects: %s", err.Error())
	}
	systemProjects := storage.DefaultProjectIDs()

	resp := api.ListProjectsResp{
		Projects: make([]*api.Project, 0, len(ps)),
	}
	for _, p := range ps {
		apiProject, err := fromStorageProject(p)
		if err != nil {
			return nil, status.Errorf(codes.Internal,
				"error converting project with ID %q: %s", p.ID, err.Error())
		}
		// exclude "all projects" meta-project from the API
		if !stringutils.SliceContains(systemProjects, apiProject.Id) {
			resp.Projects = append(resp.Projects, apiProject)
		}
	}

	return &resp, nil
}

func (s *state) DeleteProject(ctx context.Context,
	req *api.DeleteProjectReq) (*api.DeleteProjectResp, error) {
	err := s.store.DeleteProject(ctx, req.Id)
	switch err {
	case nil:
		return &api.DeleteProjectResp{}, nil
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no project with ID %q found", req.Id)
	default: // some other error
		return nil, status.Errorf(codes.Internal,
			"error deleting project with ID %q: %s", req.Id, err.Error())
	}
}

func (s *state) ListProjectRules(ctx context.Context,
	req *api.ListProjectRulesReq) (*api.ProjectCollectionRulesResp, error) {

	ruleMap, err := s.engine.ListProjectMappings(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	projectRules := make(map[string]*api.ProjectRules, len(ruleMap))
	for projectID, rules := range ruleMap {
		pr := rulesToProjectRules(rules)
		projectRules[projectID] = &api.ProjectRules{
			Rules: pr,
		}
	}
	return &api.ProjectCollectionRulesResp{
		ProjectRules: projectRules,
	}, nil
}

func (s *state) GetProjectRules(ctx context.Context,
	req *api.GetProjectRulesReq) (*api.GetProjectRulesResp, error) {

	if req.ProjectId == "" {
		return nil, status.Error(codes.InvalidArgument, "GetProjectRules requires a ProjectID")
	}

	rules, err := s.engine.RulesForProject(ctx, req.ProjectId)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "could not retrieve rules for project %q: %s",
			req.ProjectId, err.Error())
	}
	if len(rules) == 0 {
		return nil, status.Errorf(codes.NotFound,
			"could not find project mapping rules for project %s", req.ProjectId)
	}

	return &api.GetProjectRulesResp{
		RulesForProject: &api.ProjectRules{
			Rules: rulesToProjectRules(rules),
		},
	}, nil
}

func (s *state) HandleEvent(ctx context.Context,
	req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	log.Debugf("authz is handling your event %s", req.EventID)

	response := &automate_event.EventResponse{}
	if req.Type.Name == event.ProjectRulesUpdateStatus {
		err := s.projectUpdateManager.ProcessStatusMessage(req)
		if err != nil {
			return response, err
		}
	} else if req.Type.Name == event.ProjectRulesUpdateFailed {
		err := s.projectUpdateManager.ProcessFailMessage(req)
		if err != nil {
			return response, err
		}
	}

	return response, nil
}

// we want to reserve the option to return an error in this conversion
// eventually, so for now, we stop the linter from complaining:
// nolint: unparam
func fromStorageProject(p *storage.Project) (*api.Project, error) {
	return &api.Project{
		Id:       p.ID,
		Name:     p.Name,
		Type:     typeFromInternal(p.Type),
		Projects: p.Projects,
	}, nil
}

func rulesToProjectRules(rules []engine.Rule) []*api.ProjectRule {
	pr := make([]*api.Condition, len(rules))
	for j, r := range rules {
		pr[j] = &api.Condition{
			Type:   r.Type,
			Values: r.Values,
		}
	}
	rule := &api.ProjectRule{
		Conditions: pr,
	}
	return []*api.ProjectRule{rule}
}
