package v2

import (
	"context"
	"fmt"
	"sync"

	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"

	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	log "github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/authz-service/config"
	constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
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
	projectUpdateManager *ProjectUpdateManager
}

// NewMemstoreProjectsServer returns an instance of api.ProjectsServer
func NewMemstoreProjectsServer(
	ctx context.Context,
	l logger.Logger,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager,
) (api.ProjectsServer, error) {

	return NewProjectsServer(ctx, l, memstore.New(), e, eventServiceClient, configManager)
}

// NewPostgresProjectsServer instantiates a ProjectsServer using a PG store
func NewPostgresProjectsServer(
	ctx context.Context,
	l logger.Logger,
	migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager,
) (api.ProjectsServer, error) {

	s, err := postgres.New(ctx, l, migrationsConfig, dataMigrationsConfig)
	if err != nil {
		return nil, errors.Wrap(err, "failed to initialize v2 store state")
	}
	return NewProjectsServer(ctx, l, s, e, eventServiceClient, configManager)
}

func NewProjectsServer(
	ctx context.Context,
	l logger.Logger,
	s storage.Storage,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager,
) (api.ProjectsServer, error) {

	return &state{
		log:                  l,
		store:                s,
		engine:               e,
		projectUpdateManager: NewProjectUpdateManager(eventServiceClient, configManager),
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
				"max of %d projects allowed while IAM v2 Beta", constants_v2.MaxProjects)
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
	time, err := ptypes.TimestampProto(s.projectUpdateManager.EstimatedTimeComplete())
	if err != nil {
		log.Errorf("Could not convert EstimatedTimeComplete to protobuf Timestamp %v", err)
		time = &tspb.Timestamp{}
	}
	return &api.ProjectUpdateStatusResp{
		State:                 s.projectUpdateManager.State(),
		PercentageComplete:    float32(s.projectUpdateManager.PercentageComplete()),
		EstimatedTimeComplete: time,
		Failed:                s.projectUpdateManager.Failed(),
		FailureMessage:        s.projectUpdateManager.FailureMessage(),
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

func (s *state) ListProjects(
	ctx context.Context, _ *api.ListProjectsReq) (*api.ListProjectsResp, error) {
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

		// exclude all meta-projects from the API
		if !stringutils.SliceContains(systemProjects, apiProject.Id) {
			resp.Projects = append(resp.Projects, apiProject)
		}
	}

	return &resp, nil
}

func (s *state) ListProjectsForIntrospection(
	ctx context.Context, req *api.ListProjectsReq) (*api.ListProjectsResp, error) {

	// Introspection needs unfiltered access.
	ctx = auth_context.ContextWithoutProjects(ctx)

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
		// Exclude all meta-projects from the API except "unassigned"
		if !stringutils.SliceContains(systemProjects, apiProject.Id) ||
			apiProject.Id == constants_v2.UnassignedProjectID {
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
		err := s.projectUpdateManager.ProcessStatusEvent(req)
		if err != nil {
			return response, err
		}
	} else if req.Type.Name == event.ProjectRulesUpdateFailed {
		err := s.projectUpdateManager.ProcessFailEvent(req)
		if err != nil {
			return response, err
		}
	}

	return response, nil
}

func (s *state) CreateRule(ctx context.Context, req *api.CreateRuleReq) (*api.CreateRuleResp, error) {
	r, err := s.prepareStorageRule(req.Id, req.ProjectId, req.Name, req.Type, req.Conditions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "error processing request: %s", err.Error())
	}

	resp, err := s.store.CreateRule(ctx, r)
	if err != nil {
		if err == storage_errors.ErrConflict {
			return nil, status.Errorf(codes.AlreadyExists, "rule with ID %q already exists", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error creating rule with ID %q: %s", req.Id, err.Error())
	}

	apiRule, err := fromStorageRule(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting rule with ID %q: %s", resp.ID, err.Error())
	}
	return &api.CreateRuleResp{Rule: apiRule}, nil
}

func (s *state) UpdateRule(ctx context.Context, req *api.UpdateRuleReq) (*api.UpdateRuleResp, error) {
	r, err := s.prepareStorageRule(req.Id, req.ProjectId, req.Name, req.Type, req.Conditions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "error processing request: %s", err.Error())
	}

	resp, err := s.store.UpdateRule(ctx, r)
	if err != nil {
		if err == storage_errors.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "rule with ID %q not found", req.Id)
		}
		if err == storage_errors.ErrChangeProjectForRule {
			return nil, status.Errorf(codes.FailedPrecondition,
				"cannot change project_id for existing rule with ID %q ", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error creating rule with ID %q: %s", req.Id, err.Error())
	}

	apiRule, err := fromStorageRule(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting rule with ID %q: %s", resp.ID, err.Error())
	}
	return &api.UpdateRuleResp{Rule: apiRule}, nil
}

func (s *state) GetRule(ctx context.Context, req *api.GetRuleReq) (*api.GetRuleResp, error) {
	resp, err := s.store.GetRule(ctx, req.Id)
	if err != nil {
		if err == storage_errors.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "could not find rule with ID %q", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error retrieving rule with ID %q: %s", req.Id, err.Error())
	}

	apiRule, err := fromStorageRule(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting rule with ID %q: %s", resp.ID, err.Error())
	}
	return &api.GetRuleResp{Rule: apiRule}, nil
}

func (s *state) ListRules(ctx context.Context, req *api.ListRulesReq) (*api.ListRulesResp, error) {
	resp, err := s.store.ListRules(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "error retrieving rules: %s", err.Error())
	}

	rules := make([]*api.ProjectRule, len(resp))
	for i, rule := range resp {
		apiRule, err := fromStorageRule(rule)
		if err != nil {
			return nil, status.Errorf(codes.Internal,
				"error converting rule with ID %q: %s", rule.ID, err.Error())
		}
		rules[i] = apiRule
	}

	return &api.ListRulesResp{Rules: rules}, nil
}

func (s *state) ListRulesForProject(ctx context.Context, req *api.ListRulesForProjectReq) (*api.ListRulesForProjectResp, error) {
	resp, err := s.store.ListRulesForProject(ctx, req.Id)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "error retrieving rules: %s", err.Error())
	}

	rules := make([]*api.ProjectRule, len(resp))
	for i, rule := range resp {
		apiRule, err := fromStorageRule(rule)
		if err != nil {
			return nil, status.Errorf(codes.Internal,
				"error converting rule with ID %q: %s", rule.ID, err.Error())
		}
		rules[i] = apiRule
	}

	return &api.ListRulesForProjectResp{Rules: rules}, nil
}

func (s *state) DeleteRule(ctx context.Context, req *api.DeleteRuleReq) (*api.DeleteRuleResp, error) {
	err := s.store.DeleteRule(ctx, req.Id)
	switch err {
	case nil:
		return &api.DeleteRuleResp{}, nil
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "could not find rule with ID %q", req.Id)
	default: // any other error
		return nil, status.Errorf(codes.Internal,
			"error deleting rule with ID %q: %s", req.Id, err.Error())
	}
}

func storageConditions(ruleType storage.RuleType, apiConditions []*api.Condition) ([]storage.Condition, error) {
	cs := make([]storage.Condition, len(apiConditions))
	for i, c := range apiConditions {
		var err error
		cs[i], err = storageCondition(ruleType, c)
		if err != nil {
			return nil, err
		}
	}
	return cs, nil
}

func storageCondition(ruleType storage.RuleType, apiCondition *api.Condition) (storage.Condition, error) {
	condAttr, err := fromAPIProjectRuleConditionTypes(apiCondition.Type)
	if err != nil {
		return storage.Condition{}, err
	}

	condOp, err := fromAPIProjectRuleConditionOperators(apiCondition.Operator)
	if err != nil {
		return storage.Condition{}, err
	}

	return storage.NewCondition(ruleType, apiCondition.Values, condAttr, condOp)
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

func fromStorageRule(r *storage.Rule) (*api.ProjectRule, error) {
	cs, err := fromStorageConditions(r.Conditions)
	if err != nil {
		return nil, err
	}
	t, err := fromStorageRuleType(r.Type)
	if err != nil {
		return nil, err
	}
	return &api.ProjectRule{
		Id:         r.ID,
		Name:       r.Name,
		Type:       t,
		ProjectId:  r.ProjectID,
		Conditions: cs,
	}, nil
}

func fromStorageConditions(cs []storage.Condition) ([]*api.Condition, error) {
	apiConditions := make([]*api.Condition, len(cs))
	for i, c := range cs {
		d, err := fromStorageCondition(c)
		if err != nil {
			return nil, err
		}
		apiConditions[i] = d
	}
	return apiConditions, nil
}

func fromStorageCondition(c storage.Condition) (*api.Condition, error) {
	t, err := fromStorageConditionType(c.Attribute)
	if err != nil {
		return nil, err
	}

	o, err := fromStorageConditionOperator(c.Operator)
	if err != nil {
		return nil, err
	}

	return &api.Condition{
		Type:     t,
		Values:   c.Value,
		Operator: o,
	}, nil
}

var storageToAPIConditionAttributes = map[storage.ConditionAttribute]api.ProjectRuleConditionTypes{
	storage.ChefRole:     api.ProjectRuleConditionTypes_ROLES,
	storage.ChefServer:   api.ProjectRuleConditionTypes_CHEF_SERVERS,
	storage.ChefTag:      api.ProjectRuleConditionTypes_CHEF_TAGS,
	storage.Environment:  api.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
	storage.Organization: api.ProjectRuleConditionTypes_CHEF_ORGS,
	storage.PolicyGroup:  api.ProjectRuleConditionTypes_POLICY_GROUP,
	storage.PolicyName:   api.ProjectRuleConditionTypes_POLICY_NAME,
}

var apiToStorageConditionAttributes = map[api.ProjectRuleConditionTypes]storage.ConditionAttribute{}
var onceReverseConditionAttributesMapping sync.Once

func fromStorageConditionType(t storage.ConditionAttribute) (api.ProjectRuleConditionTypes, error) {
	if s, ok := storageToAPIConditionAttributes[t]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition type %q", t.String())
}

func fromAPIProjectRuleConditionTypes(t api.ProjectRuleConditionTypes) (storage.ConditionAttribute, error) {
	onceReverseConditionAttributesMapping.Do(func() {
		for k, v := range storageToAPIConditionAttributes {
			apiToStorageConditionAttributes[v] = k
		}
	})

	if s, ok := apiToStorageConditionAttributes[t]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition type %q", t.String())
}

var storageToAPIConditionOperators = map[storage.ConditionOperator]api.ProjectRuleConditionOperators{
	storage.MemberOf: api.ProjectRuleConditionOperators_MEMBER_OF,
	storage.Equals:   api.ProjectRuleConditionOperators_EQUALS,
}

var apiToStorageConditionOperators = map[api.ProjectRuleConditionOperators]storage.ConditionOperator{}
var onceReverseConditionOperatorsMapping sync.Once

func fromStorageConditionOperator(t storage.ConditionOperator) (api.ProjectRuleConditionOperators, error) {
	if s, ok := storageToAPIConditionOperators[t]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition operator %q", t.String())
}

func fromAPIProjectRuleConditionOperators(t api.ProjectRuleConditionOperators) (storage.ConditionOperator, error) {
	onceReverseConditionOperatorsMapping.Do(func() {
		for k, v := range storageToAPIConditionOperators {
			apiToStorageConditionOperators[v] = k
		}
	})

	if s, ok := apiToStorageConditionOperators[t]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition operator %q", t.String())
}

func fromStorageRuleType(t storage.RuleType) (api.ProjectRuleTypes, error) {
	switch t {
	case storage.Node:
		return api.ProjectRuleTypes_NODE, nil
	case storage.Event:
		return api.ProjectRuleTypes_EVENT, nil
	default:
		return 0, fmt.Errorf("unknown rule type: %v", t)
	}
}

func fromAPIType(t api.ProjectRuleTypes) (storage.RuleType, error) {
	switch t {
	case api.ProjectRuleTypes_NODE:
		return storage.Node, nil
	case api.ProjectRuleTypes_EVENT:
		return storage.Event, nil
	default:
		return 0, fmt.Errorf("unknown rule type %s", t.String())
	}
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

func (s *state) prepareStorageRule(inID, projectID, name string,
	inType api.ProjectRuleTypes, inConditions []*api.Condition) (*storage.Rule, error) {

	ruleType, err := fromAPIType(inType)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"creating rule with ID %q: %s", inID, err.Error())
	}
	conditions, err := storageConditions(ruleType, inConditions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"creating rule with ID %q: %s", inID, err.Error())
	}
	r, err := storage.NewRule(inID, projectID, name, ruleType, conditions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"creating rule with ID %q: %s", inID, err.Error())
	}
	return &r, nil
}
