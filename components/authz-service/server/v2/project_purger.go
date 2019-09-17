package v2

import (
	"context"
	"sync"

	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/authz/project_purge_workflow"
)

const (
	PurgeProjectWorkflowName       = "PurgeProject"
	MoveProjectToGraveyardTaskName = "authz/MoveProjectToGraveyard"
)

type ProjectPurger interface {
	Start(string) error
	Status(string) (ProjectPurgerStatus, error)
}

type ProjectPurgerStatus struct {
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

	moveProjectToGraveyardExecutor :=
		patterns.NewSingleTaskWorkflowExecutor(MoveProjectToGraveyardTaskName, false)

	domainSvcExecutor := patterns.NewParallelWorkflowExecutor(func(key string) (cereal.WorkflowExecutor, bool) {
		lock.Lock()
		defer lock.Unlock()
		if workflowExecutor, ok := workflowMap[key]; ok {
			return workflowExecutor, true
		}
		logrus.Infof("creating workflow executor for %q", key)
		workflowExecutor := project_purge_workflow.NewWorkflowExecutorForDomainService(key)
		workflowMap[key] = workflowExecutor
		return workflowExecutor, true
	})

	// TODO remove project from graveyard
	return nil, nil
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

func (m *CerealProjectPurger) Start(projectID string) error {
	params := map[string]interface{}{}

	for _, svc := range m.domainServices {
		params[svc] = project_domain_purge.DomainProjectPurgeWorkflowParameters{
			ProjectID: projectID,
		}
	}
	domainSvcUpdateWorkflowParams, err := patterns.ToParallelWorkflowParameters(m.domainServices, params)
	if err != nil {
		return err
	}
	if err := patterns.EnqueueChainWorkflow(context.TODO(), m.manager, m.workflowName, projectID, []interface{}{
		MoveProjectToGraveyardParams{
			ProjectID: projectID,
		},
		domainSvcUpdateWorkflowParams,
	}); err != nil {
		return err
	}

	// TODO wait for first serial workflow to return
}

type MoveProjectToGraveyardTaskExecutor struct {
	store storage.Storage
	log   logger.Logger
}

type MoveProjectToGraveyardResult struct {
}

type MoveProjectToGraveyardParams struct {
	ProjectID string
}

func (s *MoveProjectToGraveyardTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := MoveProjectToGraveyardParams{}
	if err := w.GetParameters(&params); err != nil {
		// TODO wrap
		return nil, err
	}

	s.log.Infof("stargin project purge for id %q", params.ProjectID)

	// if err := s.store.MoveProjectToGraveyard(ctx); err != nil {
	// 	s.log.Warnf("error applying staged projects: %s", err.Error())
	// 	return nil, status.Errorf(codes.Internal,
	// 		"error applying staged projects: %s", err.Error())
	// }

	return MoveProjectToGraveyardResult{}, nil
}
