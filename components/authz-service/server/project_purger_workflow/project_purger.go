package project_purger_workflow

import (
	"context"
	"sync"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/authz/project_purge"
)

var (
	purgeProjectWorkflowName           = cereal.NewWorkflowName("authz/PurgeProject")
	moveProjectToGraveyardTaskName     = cereal.NewTaskName("authz/MoveProjectToGraveyard")
	deleteProjectFromGraveyardTaskName = cereal.NewTaskName("authz/DeleteProjectFromGraveyard")
)

// ProjectPurger is a wrapper around a set of cereal workflows that is used to purge
// a project from authz and other dependent domains in a transactional way.
type ProjectPurger interface {
	Start(string) error
	GraveyardingCompleted(string) (bool, error)
}

// Map of every domain service that performs project purging.
// Any future domain that has non-rule based project data
// needs to be added here and RegisterTaskExecutors with the same string.
var projectPurgeDomainServices = []string{
	"teams",
	"authn",
}

type cerealProjectPurger struct {
	manager        *cereal.Manager
	workflowName   cereal.WorkflowName
	domainServices []string
}

// RegisterCerealProjectPurger creates a new ProjectPurger with the default domains.
func RegisterCerealProjectPurger(manager *cereal.Manager, log logger.Logger, s storage.Storage) (ProjectPurger, error) {
	return RegisterCerealProjectPurgerWithDomainServices(manager, log, s, projectPurgeDomainServices)
}

// RegisterCerealProjectPurgerWithDomainServices creates a new ProjectPurger with domains specified (only used for testing).
func RegisterCerealProjectPurgerWithDomainServices(manager *cereal.Manager, log logger.Logger, s storage.Storage, domainServices []string) (ProjectPurger, error) {
	moveProjectToGraveyardTaskExecutor := &moveProjectToGraveyardTaskExecutor{
		store: s,
		log:   log,
	}
	if err := manager.RegisterTaskExecutor(moveProjectToGraveyardTaskName, moveProjectToGraveyardTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {
	}

	domainServicesWorkflowExecutor, err := NewProjectPurgerWorkflowExecutor()
	if err != nil {
		return nil, err
	}
	if err := manager.RegisterWorkflowExecutor(purgeProjectWorkflowName, domainServicesWorkflowExecutor); err != nil {
		return nil, err
	}

	deleteProjectFromGraveyardTaskExecutor := &deleteProjectFromGraveyardTaskExecutor{
		store: s,
		log:   log,
	}
	if err := manager.RegisterTaskExecutor(deleteProjectFromGraveyardTaskName, deleteProjectFromGraveyardTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {
	}

	purgeManager := &cerealProjectPurger{
		manager:        manager,
		workflowName:   purgeProjectWorkflowName,
		domainServices: domainServices,
	}

	return purgeManager, nil
}

// Start kicks off the purging of a project. It sets up the cereal workflows needed
// and enqueues them all.
func (m *cerealProjectPurger) Start(projectID string) error {
	params := map[string]interface{}{}

	for _, svc := range m.domainServices {
		params[svc] = project_purge.DomainProjectPurgeWorkflowParameters{
			ProjectID: projectID,
		}
	}

	domainSvcUpdateWorkflowParams, err := patterns.ToParallelWorkflowParameters(m.domainServices, params)
	if err != nil {
		return errors.Wrap(err, "marshall parallel workflow params for domain project purge")
	}
	if err := patterns.EnqueueChainWorkflow(context.TODO(), m.manager, m.workflowName, projectID, []interface{}{
		moveProjectToGraveyardParams{
			ProjectID: projectID,
		},
		domainSvcUpdateWorkflowParams,
		deleteProjectFromGraveyardWorkflowExecutorParams{
			ProjectID: projectID,
		},
	}); err != nil {
		return errors.Wrap(err, "enqueue chain workflow for project purge")
	}

	return nil
}

// GraveyardingCompleted returns whether the project has successfully been moved to the graveyard.
// If there was an error that could not be solved by a retry, it will be returned with the correct GRPC code.
func (m *cerealProjectPurger) GraveyardingCompleted(projectID string) (bool, error) {
	instance, err := patterns.GetChainWorkflowInstance(context.TODO(), m.manager, m.workflowName, projectID)
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return true, nil
		}
		return false, errors.Wrap(err, "failed to fetch instance")
	}

	subinstance, err := instance.GetSubWorkflow(0)
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return false, nil
		}
		return false, errors.Wrap(err, "failed to fetch subinstance")
	}

	if subinstance.IsRunning() {
		return false, nil
	}
	result := moveProjectToGraveyardResult{}
	err = subinstance.GetResult(&result)

	if err != nil {
		switch err.Error() {
		case storage.ErrNotFound.Error():
			return true, status.Errorf(codes.NotFound, "no project with ID %q found", projectID)
		default:
			return true, status.Errorf(codes.Internal,
				"error graveyarding project with ID %q", projectID)
		}
	}
	return true, nil
}

// NewProjectPurgerWorkflowExecutor mostly follows the pattern described in project_update_manager.go but here is a detailed description:
//
// +--------------------------------------------------------------------+ProjectPurger+----------------------------------------------------------------------------------+
// |                                                                                                                                                                     |
// |    ProjectPurger is a wrapper around a ChainWorkflowExecutor that contains the subworkflows required to purge projects.                                             |
// |                                                                                                                                                                     |
// |                                                        Executor: ParallelWorkflowExecutor contains multiple                                                         |
// | Executor: moveProjectToGraveyardWorkflowExecutor         Executors: domainProjectPurgeWorkflowExecutor         Executor: deleteProjectFromGraveyardWorkflowExecutor |
// |                                                            for each domain (authn, tokens) in parallel.                                                             |
// | +---------------------------------------------+        +---------------------------------------------+         +---------------------------------------------+      |
// | | Contains a single task per project:         |        | There is a                                  |         | Contains a single task per project:         |      |
// | | moveProjectToGraveyardTaskExecutor          |        | domainProjectPurgeTask for each             |         | deleteProjectFromGraveyardTaskExecutor      |      |
// | |                                             |        | domain that needs to delete projects that   |         |                                             |      |
// | | If this task fails, the subsequent          |        | run in parallel under the parent            |         | Removes the now completely purged task from |      |
// | | workflows will be aborted. For example,     |        | ParallelWorkflowExecutor. Each child        |         | the project graveyard.                      |      |
// | | if the database 500s or 404s. The initial   |        | runs a single task per project purge        |         |                                             |      |
// | | API request waits for this task to complete.|        | that removes the project from its domain.   |         | Similar to the previous workflow, it will   |      |
// | | In the case of an error, it will return     | +--->  | ProjectPurger workflow will wait until      | +--->   | retry with exponential backoff and logging. |      |
// | | either a 404 or a 500, and abort            |        | every domain is done before moving onto     |         |                                             |      |
// | | subsequent workflows. Except in the case    |        | the last workflow. We don't expect failures |         | We never expect errors here unless the      |      |
// | | of the task getting lost, which will retry  |        | here unless the system is unhealthy and     |         | system is unhealthy.                        |      |
// | | the idempotent db call to move the project  |        | now the project is in the graveyard so we   |         |                                             |      |
// | | to the graveyard                            |        | retry with exponential backoff and logging. |         |                                             |      |
// | |                                             |        |                                             |         |                                             |      |
// | |                                             |        |                                             |         |                                             |      |
// | |                                             |        |                                             |         |                                             |      |
// | |                                             |        |                                             |         |                                             |      |
// | +---------------------------------------------+        +---------------------------------------------+         +---------------------------------------------+      |
// |                                                                                                                                                                     |
// +---------------------------------------------------------------------------------------------------------------------------------------------------------------------+
func NewProjectPurgerWorkflowExecutor() (*patterns.ChainWorkflowExecutor, error) {
	workflowMap := make(map[string]cereal.WorkflowExecutor)
	lock := sync.Mutex{}

	moveProjectToGraveyardExecutor := NewMoveProjectToGraveyardWorkflowExecutor()

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
		NewDeleteProjectFromGraveyardWorkflowExecutor()

	return patterns.NewChainWorkflowExecutor(
		moveProjectToGraveyardExecutor, domainSvcExecutor, deleteProjectFromGraveyardExecutor)
}
