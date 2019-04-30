package v2

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"

	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	log "github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	v2_constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/storage/v2/memstore"
	"github.com/chef/automate/components/authz-service/storage/v2/postgres"
	event "github.com/chef/automate/components/event-service/server"
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
		} else if err == storage_errors.ErrMaxProjectsExceeded {
			return nil, status.Errorf(codes.FailedPrecondition,
				"max of %d projects allowed while IAM v2 Beta", v2_constants.MaxProjects)
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

func (s *state) ProjectUpdateStatus(ctx context.Context,
	req *api.ProjectUpdateStatusReq) (*api.ProjectUpdateStatusResp, error) {
	time, err := ptypes.TimestampProto(s.projectUpdateManager.EstimatedTimeCompelete())
	if err != nil {
		log.Errorf("Could not convert EstimatedTimeCompelete to protobuf Timestamp %v", err)
		time = &tspb.Timestamp{}
	}
	return &api.ProjectUpdateStatusResp{
		State:                  s.projectUpdateManager.State(),
		PercentageComplete:     float32(s.projectUpdateManager.PercentageComplete()),
		EstimatedTimeCompelete: time,
	}, nil
}

func (s *state) ProjectUpdateCancel(ctx context.Context,
	req *api.ProjectUpdateStatusReq) (*api.ProjectUpdateCancelResp, error) {
	err := s.projectUpdateManager.Cancel()
	if err != nil {
		log.Errorf("Could not cancel project update: %v", err.Error())
	}
	return &api.ProjectUpdateCancelResp{}, nil
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

	projects := make(map[string]*api.ProjectRules, len(ruleMap))
	for projectID, rules := range ruleMap {
		projectRules, err := rulesToProjectRules(rules)
		if err != nil {
			return &api.ProjectCollectionRulesResp{}, err
		}
		projects[projectID] = &api.ProjectRules{
			Rules: projectRules,
		}
	}
	return &api.ProjectCollectionRulesResp{
		ProjectRules: projects,
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

	projectRules, err := rulesToProjectRules(rules)
	if err != nil {
		return &api.GetProjectRulesResp{}, err
	}

	return &api.GetProjectRulesResp{
		RulesForProject: &api.ProjectRules{
			Rules: projectRules,
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

// TODO: Currently there is only collection of conditions in a project, which are called 'Rule'.
// We need to update this data structure to have projects have collection of rules. The rules have a
// collection of conditions. The conditions have a 'type' and 'values' (like rules currently do).
// The rules should have a 'type' either 'node' or 'event'.
func rulesToProjectRules(rules []engine.Rule) ([]*api.ProjectRule, error) {
	conditions := make([]*api.Condition, len(rules))
	for j, rule := range rules {
		conditionType, exists := api.ProjectRuleConditionTypes_value[rule.Type]
		if !exists {
			return []*api.ProjectRule{}, errors.New(fmt.Sprintf("Condition type %s is not supported", rule.Type))
		}
		conditions[j] = &api.Condition{
			Type:   api.ProjectRuleConditionTypes(conditionType),
			Values: rule.Values,
		}
	}
	// TODO: The ProjectRule Type needs to be added to the database
	rule := &api.ProjectRule{
		Conditions: conditions,
		Type:       api.ProjectRuleTypes_NODE,
	}
	return []*api.ProjectRule{rule}, nil
}
