package v2

import (
	"context"
	"sync"
	"time"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/authz/project_purge"
)

const (
	PurgeProjectWorkflowName                = "authz/PurgeProject"
	MoveProjectToGraveyardTaskName          = "authz/MoveProjectToGraveyard"
	DeleteProjectFromDomainTaskName         = "authz/DeleteProjectFromDomain"
	DeleteProjectFromGraveyardTaskName      = "authz/DeleteProjectFromGraveyard"
	StartDeleteProjectFromGraveyardTaskName = "authz/StartDeleteProjectFromGraveyard"
)

type ProjectPurger interface {
	Start(string) error
	GraveyardingCompleted(string) (bool, error)
}

var ProjectPurgeDomainServices = []string{
	"teams",
	// "authn",
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
		NewMoveProjectToGraveyardWorkflowExecutor()

	return patterns.NewChainWorkflowExecutor(moveProjectToGraveyardExecutor, domainSvcExecutor, deleteProjectFromGraveyardExecutor)
}

type CerealProjectPurger struct {
	manager        *cereal.Manager
	workflowName   string
	domainServices []string
}

func RegisterCerealProjectPurger(manager *cereal.Manager, log logger.Logger, s storage.Storage) (ProjectPurger, error) {
	return RegisterCerealProjectPurgerWithDomainServices(manager, log, s, ProjectPurgeDomainServices)
}

func RegisterCerealProjectPurgerWithDomainServices(manager *cereal.Manager, log logger.Logger, s storage.Storage, domainServices []string) (ProjectPurger, error) {
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
	if err := manager.RegisterTaskExecutor(StartDeleteProjectFromGraveyardTaskName, deleteProjectFromGraveyardTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {
	}

	purgeManager := &CerealProjectPurger{
		manager:        manager,
		workflowName:   PurgeProjectWorkflowName,
		domainServices: domainServices,
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
		MoveProjectToGraveyardWorkflowExecutorParams{
			ProjectID: projectID,
		},
	}); err != nil {
		return errors.Wrap(err, "enqueue chain workflow for project purge")
	}

	return nil
}

func (m *CerealProjectPurger) GraveyardingCompleted(projectID string) (bool, error) {
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
	result := MoveProjectToGraveyardResult{}
	if err := subinstance.GetResult(&result); err != nil {
		return true, status.Errorf(codes.Internal,
			"unmarshall move to graveyard result for project with ID ID %q: %s", projectID, err.Error())
	}
	switch result.Code {
	case codes.OK:
		return true, nil
	case codes.NotFound:
		return true, status.Errorf(codes.NotFound, "no project with ID %q found", projectID)
	default:
		return true, status.Errorf(codes.Internal,
			"error graveyarding project with ID %q", projectID)
	}
}

// MoveProjectToGraveyardWorkflowExecutor is based on the SingleTaskWorkflowExecutor
// with infinite retry after exponential backoff.
type MoveProjectToGraveyardWorkflowExecutor struct {
}

type MoveProjectToGraveyardWorkflowExecutorParams struct {
	ProjectID string
}

type MoveProjectToGraveyardWorkflowExecutorPayload struct {
	ConsecutiveJobCheckFailures int
}

func NewMoveProjectToGraveyardWorkflowExecutor() *MoveProjectToGraveyardWorkflowExecutor {
	return &MoveProjectToGraveyardWorkflowExecutor{}
}

func (s *MoveProjectToGraveyardWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	// TODO remove

	var params MoveProjectToGraveyardWorkflowExecutorParams
	err := w.GetParameters(&params)
	if err != nil {
		logrus.WithError(err).Error("failed to get move to graveyard parameters")
		return w.Fail(err)
	}

	logrus.Warnf("STARTING MOVE PROJECT TO GRAVEYARD WITH ID %q", params.ProjectID)

	if err := w.EnqueueTask(StartDeleteProjectFromGraveyardTaskName, MoveProjectToGraveyardParams{ProjectID: params.ProjectID}); err != nil {
		logrus.WithError(err).Errorf("failed to enqueue move to graveyard task %s", StartDeleteProjectFromGraveyardTaskName)
		return w.Fail(err)
	}
	return w.Continue(nil)
}

func (s *MoveProjectToGraveyardWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	payload := MoveProjectToGraveyardWorkflowExecutorPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Error("Failed to deserialize delete project from graveyard payload")
		return w.Fail(err)
	}

	params := MoveProjectToGraveyardWorkflowExecutorParams{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	switch ev.TaskName {
	case StartDeleteProjectFromGraveyardTaskName:
		if errToLog := ev.Result.Err(); errToLog != nil {
			// TODO Log errToLog
			payload.ConsecutiveJobCheckFailures++
			if err := w.EnqueueTask(
				StartDeleteProjectFromGraveyardTaskName, MoveProjectToGraveyardParams{ProjectID: params.ProjectID},
				cereal.StartAfter(s.nextCheck(payload.ConsecutiveJobCheckFailures))); err != nil {
				return w.Fail(err)
			}
			return w.Continue(payload)
		}
		return w.Complete()
	default:
		return w.Fail(errors.Errorf("Unknown task type %q", ev.TaskName))
	}
}

func (m *MoveProjectToGraveyardWorkflowExecutor) nextCheck(numberOfFailures int) time.Time {
	return project_purge.ExponentialNextCheck(numberOfFailures)
}

func (s *MoveProjectToGraveyardWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Fail(errors.New("force cancel move project to graveyard"))

}

type MoveProjectToGraveyardTaskExecutor struct {
	store storage.Storage
	log   logger.Logger
}

type MoveProjectToGraveyardResult struct {
	Code codes.Code
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

	err := s.store.DeleteProject(ctx, params.ProjectID)
	switch err {
	case nil:
		return &MoveProjectToGraveyardResult{Code: codes.OK}, nil
	case storage_errors.ErrNotFound:
		return &MoveProjectToGraveyardResult{Code: codes.NotFound}, nil
	default:
		return &MoveProjectToGraveyardResult{Code: codes.Internal}, nil
	}
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
		return nil, errors.Wrap(err, "unmarshall delete from graveyard params")
	}

	s.log.Infof("starting project delete from graveyard for id %q", params.ProjectID)

	return DeleteProjectFromGraveyardResult{}, s.store.RemoveProjectFromGraveyard(ctx, params.ProjectID)
}
