package v2

import (
	"context"
	"fmt"
	"math"
	"sync"
	"time"

	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/logger"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"

	project_update_tags "github.com/chef/automate/lib/authz"
)

const (
	ProjectUpdateRunningState    = "running"
	ProjectUpdateNotRunningState = "not_running"
	ProjectUpdateUnknownState    = "unknown"

	ProjectUpdateWorkflowName = "ProjectUpdate"
	ProjectUpdateInstanceName = "SingletonV1"
	ApplyStagedRulesTaskName  = "authz/ApplyStagedRules"
)

var ProjectUpdateDomainServices = []string{
	"ingest",
	"compliance",
}

type ProjectUpdateStatus interface {
	Failed() bool
	FailureMessage() string
	PercentageComplete() float64
	EstimatedTimeComplete() time.Time
	State() string
}

type ProjectUpdateMgr interface {
	Cancel() error
	Start() error
	Status() (ProjectUpdateStatus, error)
}

func createProjectUpdateID() string {
	uuid, err := uuid.NewV4()
	if err != nil {
		return "project-update-id"
	}

	return uuid.String()
}

func NewWorkflowExecutor() (*patterns.ChainWorkflowExecutor, error) {
	workflowMap := make(map[string]cereal.WorkflowExecutor)
	lock := sync.Mutex{}

	applyStagedRulesExecutor := patterns.NewSingleTaskWorkflowExecutor(ApplyStagedRulesTaskName, true)
	// The code below allows us to easily add to the ProjectUpdateDomainServices list.
	// By not preconfiguring our workflow with an exact set of subworkflow executors,
	// we can generate the workflow executor for the correct domain service easily.
	// The logic only depends on the name. This means that an instance of authz that
	// didn't know about a domain service can still drive its workflow.
	domainSvcExecutor := patterns.NewParallelWorkflowExecutor(func(key string) (cereal.WorkflowExecutor, bool) {
		lock.Lock()
		defer lock.Unlock()
		if workflowExecutor, ok := workflowMap[key]; ok {
			return workflowExecutor, true
		}
		logrus.Infof("creating workflow executor for %q", key)
		workflowExecutor := project_update_tags.NewWorkflowExecutorForDomainService(key)
		workflowMap[key] = workflowExecutor
		return workflowExecutor, true
	})
	return patterns.NewChainWorkflowExecutor(applyStagedRulesExecutor, domainSvcExecutor)
}

type ApplyStagedRulesTaskExecutor struct {
	store           storage.Storage
	policyRefresher PolicyRefresher
	log             logger.Logger
}

type ApplyStagedRulesResult struct {
}

type ApplyStagedRulesParams struct {
}

func (s *ApplyStagedRulesTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	s.log.Info("apply project rules starting")

	if err := s.store.ApplyStagedRules(ctx); err != nil {
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

	return ApplyStagedRulesResult{}, nil
}

type CerealProjectUpdateManager struct {
	manager        *cereal.Manager
	workflowName   string
	instanceName   string
	domainServices []string
}

func RegisterCerealProjectUpdateManager(manager *cereal.Manager, log logger.Logger, s storage.Storage, pr PolicyRefresher) (ProjectUpdateMgr, error) {
	domainServicesWorkflowExecutor, err := NewWorkflowExecutor()
	if err != nil {
		return nil, err
	}

	if err := manager.RegisterWorkflowExecutor(ProjectUpdateWorkflowName, domainServicesWorkflowExecutor); err != nil {
		return nil, err
	}

	applyStagedRuelsTaskExecutor := &ApplyStagedRulesTaskExecutor{
		store:           s,
		policyRefresher: pr,
		log:             log,
	}
	if err := manager.RegisterTaskExecutor(ApplyStagedRulesTaskName, applyStagedRuelsTaskExecutor,
		cereal.TaskExecutorOpts{Workers: 1}); err != nil {

	}

	updateManager := &CerealProjectUpdateManager{
		manager:        manager,
		workflowName:   ProjectUpdateWorkflowName,
		instanceName:   ProjectUpdateInstanceName,
		domainServices: ProjectUpdateDomainServices,
	}

	return updateManager, nil
}

func (m *CerealProjectUpdateManager) Cancel() error {
	err := m.manager.CancelWorkflow(context.Background(), m.workflowName, m.instanceName)
	if err == cereal.ErrWorkflowInstanceNotFound {
		return nil
	}
	return err
}

func (m *CerealProjectUpdateManager) Start() error {
	params := map[string]interface{}{}

	for _, svc := range m.domainServices {
		params[svc] = project_update_tags.DomainProjectUpdateWorkflowParameters{
			ProjectUpdateID: createProjectUpdateID(),
		}
	}
	domainSvcUpdateWorkflowParams, err := patterns.ToParallelWorkfowParameters(m.domainServices, params)
	if err != nil {
		return err
	}
	return patterns.EnqueueChainWorkflow(context.TODO(), m.manager, m.workflowName, m.instanceName, []interface{}{
		ApplyStagedRulesParams{},
		domainSvcUpdateWorkflowParams,
	})
}

type workflowInstance struct {
	chain *patterns.ChainWorkflowInstance
}

func (w *workflowInstance) Failed() bool {
	return w.FailureMessage() != ""
}

func (w *workflowInstance) FailureMessage() string {
	if err := w.chain.Err(); err != nil {
		return err.Error()
	}

	if instance, err := w.GetApplyStagedRulesInstance(); err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return ""
		}
		return err.Error()
	} else {
		if err := instance.Err(); err != nil {
			return err.Error()
		}
	}

	if updateDomainServicesInstance, err := w.GetUpdateDomainServicesInstance(); err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return ""
		}
		return err.Error()
	} else {
		if err := updateDomainServicesInstance.Err(); err != nil {
			return err.Error()
		}

		errMsg := ""
		for _, subworkflowKey := range updateDomainServicesInstance.ListSubWorkflows() {
			if subworkflowInstance, err := updateDomainServicesInstance.GetSubWorkflow(subworkflowKey); err != nil {
				if err == cereal.ErrWorkflowInstanceNotFound {
					continue
				}
				errMsg = fmt.Sprintf("%s; %s: %s", errMsg, subworkflowKey, err.Error())
			} else {
				if subworkflowInstance.Err() != nil {
					errMsg = fmt.Sprintf("%s; %s: %s", errMsg, subworkflowKey, subworkflowInstance.Err().Error())
				}
			}
		}
		return errMsg
	}
}

func (w *workflowInstance) PercentageComplete() float64 {
	if !w.IsRunning() {
		return 1.0
	}

	percentComplete := 0.0
	domainServicesUpdateInstance, err := w.GetUpdateDomainServicesInstance()
	if err == cereal.ErrWorkflowInstanceNotFound {
		return percentComplete
	}

	domainServices := domainServicesUpdateInstance.ListSubWorkflows()
	if len(domainServices) == 0 {
		return 1.0
	}

	for _, d := range domainServicesUpdateInstance.ListSubWorkflows() {
		subWorkflow, err := domainServicesUpdateInstance.GetSubWorkflow(d)
		if err != nil {
			logrus.WithError(err).Errorf("failed to get subworkflow for %q", d)
			continue
		}
		payload := project_update_tags.DomainProjectUpdateWorkflowPayload{}
		if subWorkflow.IsRunning() {
			if err := subWorkflow.GetPayload(&payload); err != nil {
				logrus.WithError(err).Errorf("failed to get payload for %q", d)
				continue
			}
			percentComplete = percentComplete + (float64(payload.MergedJobStatus.PercentageComplete) / float64(len(domainServices)))
		} else {
			percentComplete = percentComplete + 1.0/float64(len(domainServices))
		}
	}

	return math.Min(percentComplete, 1.0)
}

func (w *workflowInstance) EstimatedTimeComplete() time.Time {
	if !w.IsRunning() {
		return time.Now()
	}
	domainServicesUpdateInstance, err := w.GetUpdateDomainServicesInstance()
	if err == cereal.ErrWorkflowInstanceNotFound {
		return time.Now()
	}

	longestEstimatedTimeComplete := time.Time{}
	for _, d := range domainServicesUpdateInstance.ListSubWorkflows() {
		subWorkflow, err := domainServicesUpdateInstance.GetSubWorkflow(d)
		if err != nil {
			logrus.WithError(err).Errorf("failed to get subworkflow for %q", d)
			continue
		}
		payload := project_update_tags.DomainProjectUpdateWorkflowPayload{}
		if subWorkflow.IsRunning() {
			if err := subWorkflow.GetPayload(&payload); err != nil {
				logrus.WithError(err).Errorf("failed to get payload for %q", d)
				continue
			}
			estimatedTime := time.Unix(payload.MergedJobStatus.EstimatedEndTimeInSec, 0)
			if estimatedTime.After(longestEstimatedTimeComplete) {
				longestEstimatedTimeComplete = estimatedTime
			}
		}
	}

	return longestEstimatedTimeComplete
}

func (w *workflowInstance) State() string {
	if w.IsRunning() {
		return ProjectUpdateRunningState
	}
	return ProjectUpdateNotRunningState
}

func (w *workflowInstance) IsRunning() bool {
	return w.chain.IsRunning()
}

func (w *workflowInstance) GetApplyStagedRulesInstance() (cereal.ImmutableWorkflowInstance, error) {
	return w.chain.GetSubWorkflow(0)
}

func (w *workflowInstance) GetUpdateDomainServicesInstance() (*patterns.ParallelWorkflowInstance, error) {
	instance, err := w.chain.GetSubWorkflow(1)
	if err != nil {
		return nil, err
	}
	return patterns.ToParallelWorkflowInstance(instance)
}

func (m *CerealProjectUpdateManager) getWorkflowInstance(ctx context.Context) (*workflowInstance, error) {
	chainInstance, err := patterns.GetChainWorkflowInstance(ctx, m.manager, m.workflowName, m.instanceName)
	if err != nil {
		return nil, err
	}
	return &workflowInstance{chain: chainInstance}, nil
}

type emptyProjectUpdateStatus struct{}

func (*emptyProjectUpdateStatus) Failed() bool {
	return false
}
func (*emptyProjectUpdateStatus) FailureMessage() string {
	return ""
}

func (*emptyProjectUpdateStatus) PercentageComplete() float64 {
	return 1.0
}

func (*emptyProjectUpdateStatus) EstimatedTimeComplete() time.Time {
	return time.Time{}
}

func (*emptyProjectUpdateStatus) State() string {
	return ProjectUpdateNotRunningState
}

func (m *CerealProjectUpdateManager) Status() (ProjectUpdateStatus, error) {
	projectUpdateInstance, err := m.getWorkflowInstance(context.TODO())
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return &emptyProjectUpdateStatus{}, nil
		}
		return nil, err
	}
	return projectUpdateInstance, nil
}
