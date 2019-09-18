package v2

import (
	"context"
	"sync"

	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/authz/project_purge"
)

const (
	PurgeProjectWorkflowName           = "authz/PurgeProject"
	MoveProjectToGraveyardTaskName     = "authz/MoveProjectToGraveyard"
	DeleteProjectFromDomainTaskName    = "authz/DeleteProjectFromDomain"
	DeleteProjectFromGraveyardTaskName = "authz/DeleteProjectFromGraveyard"
)

type ProjectPurger interface {
	Start(string) error
	// Status(string) (ProjectPurgerStatus, error)
}

// type ProjectPurgerStatus struct {
// }

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
func NewProjectPurgerWorkflowExecutor() (*patterns.ChainWorkflowExecutor, error) {
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
		workflowExecutor := project_purge.NewWorkflowExecutorForDomainService(key)
		workflowMap[key] = workflowExecutor
		return workflowExecutor, true
	})

	deleteProjectFromGraveyardExecutor :=
		patterns.NewSingleTaskWorkflowExecutor(DeleteProjectFromGraveyardTaskName, false)

	return patterns.NewChainWorkflowExecutor(moveProjectToGraveyardExecutor, domainSvcExecutor, deleteProjectFromGraveyardExecutor)
}

type CerealProjectPurger struct {
	manager        *cereal.Manager
	workflowName   string
	instanceName   string
	domainServices []string
}

func RegisterCerealProjectPurger(manager *cereal.Manager, log logger.Logger, s storage.Storage) (ProjectPurger, error) {
	domainServicesWorkflowExecutor, err := NewProjectPurgerWorkflowExecutor()
	if err != nil {
		return nil, err
	}

	if err := manager.RegisterWorkflowExecutor(PurgeProjectWorkflowName, domainServicesWorkflowExecutor); err != nil {
		return nil, err
	}

	moveProjectToGraveyardTaskExecutor := &MoveProjectToGraveyardTaskExecutor{
		store: s,
		log:   log,
	}
	if err := manager.RegisterTaskExecutor(MoveProjectToGraveyardTaskName, moveProjectToGraveyardTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {
	}

	deleteProjectFromGraveyardTaskExecutor := &DeleteProjectFromGraveyardTaskExecutor{
		store: s,
		log:   log,
	}
	if err := manager.RegisterTaskExecutor(DeleteProjectFromGraveyardTaskName, deleteProjectFromGraveyardTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {
	}

	purgeManager := &CerealProjectPurger{
		manager:        manager,
		workflowName:   PurgeProjectWorkflowName,
		domainServices: ProjectPurgeDomainServices,
	}

	return purgeManager, nil
}

func (m *CerealProjectPurger) Start(projectID string) error {
	params := map[string]interface{}{}

	for _, svc := range m.domainServices {
		params[svc] = project_purge.DomainProjectPurgeWorkflowParameters{
			ProjectID: projectID,
		}
	}

	domainSvcUpdateWorkflowParams, err := patterns.ToParallelWorkflowParameters(m.domainServices, params)
	if err != nil {
		return errors.Wrap(err, "marshall parallel workflow params")
	}
	if err := patterns.EnqueueChainWorkflow(context.TODO(), m.manager, m.workflowName, projectID, []interface{}{
		MoveProjectToGraveyardParams{
			ProjectID: projectID,
		},
		domainSvcUpdateWorkflowParams,
		DeleteProjectFromGraveyardParams{
			ProjectID: projectID,
		},
	}); err != nil {
		return errors.Wrap(err, "enqueue chain workflow for project purge")
	}

	return nil
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
	if err := task.GetParameters(&params); err != nil {
		return nil, errors.Wrap(err, "unmarshall move to graveyard params")
	}

	s.log.Infof("starting project move to graveyard for id %q", params.ProjectID)

	// err := s.store.MoveProjectToGraveyard(ctx, req.Id)
	// switch err {
	// case nil:
	// 	return &api.DeleteProjectResp{}, nil
	// case storage_errors.ErrNotFound:
	// 	return nil, status.Errorf(codes.NotFound, "no project with ID %q found", req.Id)
	// default: // some other error
	// 	return nil, status.Errorf(codes.Internal,
	// 		"error deleting project with ID %q: %s", req.Id, err.Error())
	// }

	return MoveProjectToGraveyardResult{}, nil
}

type DeleteProjectFromGraveyardTaskExecutor struct {
	store storage.Storage
	log   logger.Logger
}

type DeleteProjectFromGraveyardResult struct {
}

type DeleteProjectFromGraveyardParams struct {
	ProjectID string
}

func (s *DeleteProjectFromGraveyardTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := DeleteProjectFromGraveyardParams{}
	if err := task.GetParameters(&params); err != nil {
		// TODO wrap
		return nil, errors.Wrap(err, "unmarshall delete from graveyard params")
	}

	s.log.Infof("starting project delete from graveyard for id %q", params.ProjectID)

	// err := s.store.MoveProjectToGraveyard(ctx, req.Id)
	// switch err {
	// case nil:
	// 	return &api.DeleteProjectResp{}, nil
	// case storage_errors.ErrNotFound:
	// 	return nil, status.Errorf(codes.NotFound, "no project with ID %q found", req.Id)
	// default: // some other error
	// 	return nil, status.Errorf(codes.Internal,
	// 		"error deleting project with ID %q: %s", req.Id, err.Error())
	// }

	return DeleteProjectFromGraveyardResult{}, nil
}
