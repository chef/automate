package v2

import (
	"context"
	"sync"

	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	project_domain_purge "github.com/chef/automate/lib/authz"
)

const (
	PurgeProjectWorkflowName = "PurgeProject"
	// TODO does this need to be different from project manager?
	PurgeProjectInstanceName       = "SingletonV1"
	MoveProjectToGraveyardTaskName = "authz/MoveProjectToGraveyard"
)

type ProjectPurgerStatus interface {
	// Failed() bool
	// Cancelled() bool
	// FailureMessage() string
	// PercentageComplete() float64
	// EstimatedTimeComplete() time.Time
	// State() ProjectUpdateState
	// Stage() ProjectUpdateStage
}

type ProjectPurger interface {
	Cancel() error
	Start() error
	Status() (ProjectPurgerStatus, error)
}

var ProjectPurgeDomainServices = []string{
	"teams",
	// "tokens",
}

// This workflow executor follows the pattern described in project_update_manager.go
// in detail with one exception: it has a final serial task to execute after after all
// the parallel tasks finish.
//
// Serial workflow 1: move project to graveyard
//
// Parallel workflows 2: purge project from domain services
//
// Serial workflow 3: delete project from graveyard
func NewWorkflowExecutor() (*patterns.ChainWorkflowExecutor, error) {
	workflowMap := make(map[string]cereal.WorkflowExecutor)
	lock := sync.Mutex{}

	// TODO should this be cancelable?
	moveProjectToGraveyardExecutor :=
		patterns.NewSingleTaskWorkflowExecutor(MoveProjectToGraveyardTaskName, true)

	// TODO

	return nil, nil
}

type MoveProjectToGraveyardTaskExecutor struct {
	store           storage.Storage
	policyRefresher PolicyRefresher
	log             logger.Logger
}

type MoveProjectToGraveyardResult struct {
}

type MoveProjectToGraveyardParams struct {
}

func (s *MoveProjectToGraveyardTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	s.log.Info("apply project rules starting")

	if err := s.store.MoveProjectToGraveyard(ctx); err != nil {
		s.log.Warnf("error applying staged projects: %s", err.Error())
		return nil, status.Errorf(codes.Internal,
			"error applying staged projects: %s", err.Error())
	}

	// TODO: If the domain services are reading out of the cache below, that is a problem for multinode.
	//       this does not force a refresh on all the nodes. We should read this information from the
	//       database.
	if err := s.policyRefresher.Refresh(ctx); err != nil {
		s.log.Warnf("error refreshing policy cache. the rules were updated but the apply was not started, please try again.")
		return nil, status.Errorf(codes.Internal,
			"error refreshing policy cache: %s", err.Error())
	}

	return MoveProjectToGraveyardResult{}, nil
}

type CerealProjectPurger struct {
	manager        *cereal.Manager
	workflowName   string
	instanceName   string
	domainServices []string
}

func RegisterCerealProjectUpdateManager(manager *cereal.Manager, log logger.Logger, s storage.Storage) (ProjectPurger, error) {
	domainServicesWorkflowExecutor, err := NewWorkflowExecutor()
	if err != nil {
		return nil, err
	}

	if err := manager.RegisterWorkflowExecutor(MoveProjectToGraveyardTaskName, domainServicesWorkflowExecutor); err != nil {
		return nil, err
	}

	moveProjectToGraveyardTaskExecutor := &MoveProjectToGraveyardTaskExecutor{
		store: s,
		log:   log,
	}
	if err := manager.RegisterTaskExecutor(ApplyStagedRulesTaskName, applyStagedRulesTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {

	}

	purgeManager := &CerealProjectPurger{
		manager:        manager,
		workflowName:   PurgeProjectWorkflowName,
		instanceName:   PurgeProjectInstanceName,
		domainServices: ProjectPurgeDomainServices,
	}

	return purgeManager, nil
}

func (m *CerealProjectPurger) Cancel() error {
	err := m.manager.CancelWorkflow(context.Background(), m.workflowName, m.instanceName)
	if err == cereal.ErrWorkflowInstanceNotFound {
		return nil
	}
	return err
}

func (m *CerealProjectPurger) Start() error {
	params := map[string]interface{}{}

	for _, svc := range m.domainServices {
		params[svc] = project_domain_purge.DomainProjectPurgeWorkflowParameters{
			ProjectUpdateID: createProjectUpdateID(),
			ProjectID:       "project1", // TODO how to pass this in?
		}
	}
	domainSvcUpdateWorkflowParams, err := patterns.ToParallelWorkflowParameters(m.domainServices, params)
	if err != nil {
		return err
	}
	return patterns.EnqueueChainWorkflow(context.TODO(), m.manager, m.workflowName, m.instanceName, []interface{}{
		MoveProjectToGraveyardParams{},
		domainSvcUpdateWorkflowParams,
	})
}
