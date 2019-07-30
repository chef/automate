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

// ProjectState holds the server state for projects
type ProjectState struct {
	log                  logger.Logger
	store                storage.Storage
	engine               engine.ProjectRulesRetriever
	ProjectUpdateManager ProjectUpdateMgr
	policyRefresher      PolicyRefresher
	applyRuleMux         sync.Mutex
}

// NewMemstoreProjectsServer returns an instance of api.ProjectsServer
func NewMemstoreProjectsServer(
	ctx context.Context,
	l logger.Logger,
	e engine.ProjectRulesRetriever,
	eventServiceClient automate_event.EventServiceClient,
	configManager *config.Manager,
	pr PolicyRefresher,
) (api.ProjectsServer, error) {

	projectUpdateManager := NewProjectUpdateManager(eventServiceClient, configManager)
	return NewProjectsServer(ctx, l, memstore.New(), e, projectUpdateManager, pr)
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
	pr PolicyRefresher,
) (api.ProjectsServer, error) {

	s, err := postgres.New(ctx, l, migrationsConfig, dataMigrationsConfig)
	if err != nil {
		return nil, errors.Wrap(err, "failed to initialize v2 store state")
	}
	projectUpdateManager := NewProjectUpdateManager(eventServiceClient, configManager)
	return NewProjectsServer(ctx, l, s, e, projectUpdateManager, pr)
}

func NewProjectsServer(
	ctx context.Context,
	l logger.Logger,
	s storage.Storage,
	e engine.ProjectRulesRetriever,
	projectUpdateManager ProjectUpdateMgr,
	pr PolicyRefresher,
) (api.ProjectsServer, error) {

	return &ProjectState{
		log:                  l,
		store:                s,
		engine:               e,
		ProjectUpdateManager: projectUpdateManager,
		policyRefresher:      pr,
	}, nil
}

func (s *ProjectState) GetProject(ctx context.Context,
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

func (s *ProjectState) CreateProject(ctx context.Context,
	req *api.CreateProjectReq) (*api.CreateProjectResp, error) {
	p, err := storage.NewProject(req.Id, req.Name, storage.Custom)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"creating project with ID %q: %s", req.Id, err.Error())
	}
	resp, err := s.store.CreateProject(ctx, &p)
	switch err {
	case nil: // continue
	case storage_errors.ErrConflict:
		return nil, status.Errorf(codes.AlreadyExists, "project with ID %q already exists", req.Id)
	case storage_errors.ErrMaxProjectsExceeded:
		return nil, status.Errorf(codes.FailedPrecondition,
			"max of %d projects allowed while IAM v2 Beta", constants_v2.MaxProjects)
	default:
		switch err.(type) {
		case *storage_errors.ForeignKeyError:
			return nil, status.Error(codes.InvalidArgument, err.Error())
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

func (s *ProjectState) UpdateProject(ctx context.Context,
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

	return &api.UpdateProjectResp{Project: apiProject}, nil
}

func (s *ProjectState) ApplyRulesStart(
	ctx context.Context, _ *api.ApplyRulesStartReq) (*api.ApplyRulesStartResp, error) {
	// NOTE (tc): Only one call to ApplyRulesStart can happen at a time.
	// This should be good enough to prevent race conditions for single node,
	// in conjunction with the table locking that happens in store.ApplyStagedRules.
	// Can still get into a weird state if we panic, but a re-apply will fix things up.
	// We will be refactoring for multi-node using workflow tooling in the near future.
	s.applyRuleMux.Lock()
	defer s.applyRuleMux.Unlock()

	switch s.ProjectUpdateManager.State() {
	case config.NotRunningState:
		break
	case config.RunningState:
		return nil, status.Error(codes.FailedPrecondition,
			"cannot apply rules: apply already in progress")
	default:
		return nil, status.Error(codes.Internal,
			"failed to parse state of rule apply")
	}

	s.log.Info("apply project rules: START")
	err := s.store.ApplyStagedRules(ctx)
	if err != nil {
		s.log.Warnf("error applying staged projects: %s", err.Error())
		return nil, status.Errorf(codes.Internal,
			"error applying staged projects: %s", err.Error())
	}

	// TODO (tc): If we panic between here and manager Start, we will be in a state where the rules
	// have been updated in the database but Refresh has not been kicked off.
	// We will be refactoring with workflow to make this safer soon.
	err = s.policyRefresher.Refresh(ctx)
	if err != nil {
		s.log.Warnf("error refreshing policy cache. the rules were updated but the apply was not started, please try again.")
		return nil, status.Errorf(codes.Internal,
			"error refreshing policy cache: %s", err.Error())
	}

	err = s.ProjectUpdateManager.Start()
	if err != nil {
		s.log.Warnf("error starting project update. the rules and cache were updated but the apply was not started, please try again.")
		return nil, status.Errorf(codes.Internal,
			"error starting project update: %s", err.Error())
	}

	return &api.ApplyRulesStartResp{}, nil
}

func (s *ProjectState) ApplyRulesCancel(
	context.Context, *api.ApplyRulesCancelReq) (*api.ApplyRulesCancelResp, error) {
	s.log.Info("apply project rules: CANCEL")
	err := s.ProjectUpdateManager.Cancel()
	if err != nil {
		s.log.Errorf("Could not cancel project update: %v", err.Error())
	}
	return &api.ApplyRulesCancelResp{}, nil
}

func (s *ProjectState) ApplyRulesStatus(
	context.Context, *api.ApplyRulesStatusReq) (*api.ApplyRulesStatusResp, error) {
	time, err := ptypes.TimestampProto(s.ProjectUpdateManager.EstimatedTimeComplete())
	if err != nil {
		s.log.Errorf("Could not convert EstimatedTimeComplete to protobuf Timestamp %v", err)
		time = &tspb.Timestamp{}
	}
	return &api.ApplyRulesStatusResp{
		State:                 s.ProjectUpdateManager.State(),
		PercentageComplete:    float32(s.ProjectUpdateManager.PercentageComplete()),
		EstimatedTimeComplete: time,
		Failed:                s.ProjectUpdateManager.Failed(),
		FailureMessage:        s.ProjectUpdateManager.FailureMessage(),
	}, nil
}

func (s *ProjectState) ListProjects(
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

func (s *ProjectState) ListProjectsForIntrospection(
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

func (s *ProjectState) DeleteProject(ctx context.Context,
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

func (s *ProjectState) ListRulesForAllProjects(ctx context.Context,
	req *api.ListRulesForAllProjectsReq) (*api.ListRulesForAllProjectsResp, error) {

	ruleMap, err := s.engine.ListProjectMappings(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	projects := make(map[string]*api.ProjectRules, len(ruleMap))
	for projectID, rules := range ruleMap {
		apiRules := make([]*api.ProjectRule, len(rules))
		for i, rule := range rules {
			r, err := fromStorageRule(&rule)
			if err != nil {
				return &api.ListRulesForAllProjectsResp{}, err
			}
			apiRules[i] = r
		}

		projects[projectID] = &api.ProjectRules{
			Rules: apiRules,
		}
	}
	return &api.ListRulesForAllProjectsResp{
		ProjectRules: projects,
	}, nil
}

func (s *ProjectState) HandleEvent(ctx context.Context,
	req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	s.log.Debugf("authz is handling your event %s", req.EventID)

	response := &automate_event.EventResponse{}
	if req.Type.Name == event.ProjectRulesUpdateStatus {
		err := s.ProjectUpdateManager.ProcessStatusEvent(req)
		if err != nil {
			return response, err
		}
	} else if req.Type.Name == event.ProjectRulesUpdateFailed {
		err := s.ProjectUpdateManager.ProcessFailEvent(req)
		if err != nil {
			return response, err
		}
	}

	return response, nil
}

func (s *ProjectState) CreateRule(ctx context.Context, req *api.CreateRuleReq) (*api.CreateRuleResp, error) {
	r, err := s.prepareStorageRule(req.Id, req.ProjectId, req.Name, req.Type, req.Conditions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "error processing request: %s", err.Error())
	}

	resp, err := s.store.CreateRule(ctx, r)
	switch err {
	case nil: // continue
	case storage_errors.ErrConflict:
		return nil, status.Errorf(codes.AlreadyExists, "rule with ID %q already exists", req.Id)
	default:
		switch err.(type) {
		case *storage_errors.ForeignKeyError:
			return nil, status.Error(codes.InvalidArgument, err.Error())
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

func (s *ProjectState) UpdateRule(ctx context.Context, req *api.UpdateRuleReq) (*api.UpdateRuleResp, error) {
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
			"error updating rule with ID %q: %s", req.Id, err.Error())
	}

	apiRule, err := fromStorageRule(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting rule with ID %q: %s", resp.ID, err.Error())
	}
	return &api.UpdateRuleResp{Rule: apiRule}, nil
}

func (s *ProjectState) GetRule(ctx context.Context, req *api.GetRuleReq) (*api.GetRuleResp, error) {
	resp, err := s.store.GetStagedOrAppliedRule(ctx, req.Id)
	if err != nil {
		if err == storage_errors.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "could not find rule with ID %q", req.Id)
		}
		return nil, status.Errorf(codes.Internal,
			"error retrieving rule with ID %q: %s", req.Id, err.Error())
	}
	if resp.Deleted {
		return nil, status.Errorf(codes.NotFound, "rule with ID %q marked for deletion", req.Id)
	}

	apiRule, err := fromStorageRule(resp)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"error converting rule with ID %q: %s", resp.ID, err.Error())
	}
	return &api.GetRuleResp{Rule: apiRule}, nil
}

func (s *ProjectState) ListRules(ctx context.Context, req *api.ListRulesReq) (*api.ListRulesResp, error) {
	if req.IncludeStaged {
		return s.listRulesWithFunction(ctx, req, s.store.ListStagedAndAppliedRules)
	}
	return s.listRulesWithFunction(ctx, req, s.store.ListRules)
}

func (s *ProjectState) listRulesWithFunction(ctx context.Context, req *api.ListRulesReq, list func(context.Context) ([]*storage.Rule, error)) (*api.ListRulesResp, error) {
	resp, err := list(ctx)
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

func (s *ProjectState) ListRulesForProject(ctx context.Context, req *api.ListRulesForProjectReq) (*api.ListRulesForProjectResp, error) {
	resp, statusResp, err := s.store.ListRulesForProject(ctx, req.Id)
	if err != nil {
		if err == storage_errors.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "could not find project with ID %q", req.Id)
		}
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

	return &api.ListRulesForProjectResp{
		Rules:  rules,
		Status: statusResp.String(),
	}, nil
}

func (s *ProjectState) DeleteRule(ctx context.Context, req *api.DeleteRuleReq) (*api.DeleteRuleResp, error) {
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

func storageConditions(apiConditions []*api.Condition) ([]storage.Condition, error) {
	cs := make([]storage.Condition, len(apiConditions))
	for i, c := range apiConditions {
		var err error
		cs[i], err = storageCondition(c)
		if err != nil {
			return nil, err
		}
	}
	return cs, nil
}

func storageCondition(apiCondition *api.Condition) (storage.Condition, error) {
	condAttr, err := fromAPIProjectRuleConditionAttributes(apiCondition.Attribute)
	if err != nil {
		return storage.Condition{}, err
	}

	condOp, err := fromAPIProjectRuleConditionOperators(apiCondition.Operator)
	if err != nil {
		return storage.Condition{}, err
	}

	return storage.NewCondition(apiCondition.Values, condAttr, condOp)
}

// we want to reserve the option to return an error in this conversion
// eventually, so for now, we stop the linter from complaining:
// nolint: unparam
func fromStorageProject(p *storage.Project) (*api.Project, error) {
	return &api.Project{
		Id:       p.ID,
		Name:     p.Name,
		Type:     typeFromInternal(p.Type),
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
		Deleted:    r.Deleted,
		Status:     r.Status,
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
	a, err := fromStorageConditionAttribute(c.Attribute)
	if err != nil {
		return nil, err
	}

	o, err := fromStorageConditionOperator(c.Operator)
	if err != nil {
		return nil, err
	}

	return &api.Condition{
		Attribute: a,
		Values:    c.Value,
		Operator:  o,
	}, nil
}

var storageToAPIConditionAttributes = map[storage.ConditionAttribute]api.ProjectRuleConditionAttributes{
	storage.ChefRole:     api.ProjectRuleConditionAttributes_ROLES,
	storage.ChefServer:   api.ProjectRuleConditionAttributes_CHEF_SERVERS,
	storage.ChefTag:      api.ProjectRuleConditionAttributes_CHEF_TAGS,
	storage.Environment:  api.ProjectRuleConditionAttributes_CHEF_ENVIRONMENTS,
	storage.Organization: api.ProjectRuleConditionAttributes_CHEF_ORGS,
	storage.PolicyGroup:  api.ProjectRuleConditionAttributes_POLICY_GROUP,
	storage.PolicyName:   api.ProjectRuleConditionAttributes_POLICY_NAME,
}

var apiToStorageConditionAttributes = map[api.ProjectRuleConditionAttributes]storage.ConditionAttribute{}
var onceReverseConditionAttributesMapping sync.Once

func fromStorageConditionAttribute(a storage.ConditionAttribute) (api.ProjectRuleConditionAttributes, error) {
	if s, ok := storageToAPIConditionAttributes[a]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition attribute %q", a.String())
}

func fromAPIProjectRuleConditionAttributes(a api.ProjectRuleConditionAttributes) (storage.ConditionAttribute, error) {
	onceReverseConditionAttributesMapping.Do(func() {
		for k, v := range storageToAPIConditionAttributes {
			apiToStorageConditionAttributes[v] = k
		}
	})

	if s, ok := apiToStorageConditionAttributes[a]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition attribute %q", a.String())
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

func (s *ProjectState) prepareStorageRule(inID, projectID, name string,
	inType api.ProjectRuleTypes, inConditions []*api.Condition) (*storage.Rule, error) {

	ruleType, err := fromAPIType(inType)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument,
			"creating rule with ID %q: %s", inID, err.Error())
	}
	conditions, err := storageConditions(inConditions)
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
