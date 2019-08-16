package v2

import (
	"context"
	"fmt"
	"sync"
	"time"

	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/multiworkflow"
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
)

var ProjectUpdateDomainServices = []string{
	"ingest",
	"compliance",
}

const (
	minutesWithoutCheckingInFailure = 5
	sleepTimeBetweenStatusChecksSec = 5
)

type ProjectUpdateMgr interface {
	Cancel() error
	Start() error
	Failed() bool
	FailureMessage() string
	PercentageComplete() float64
	EstimatedTimeComplete() time.Time
	State() string
}

func createProjectUpdateID() string {
	uuid, err := uuid.NewV4()
	if err != nil {
		return "project-update-id"
	}

	return uuid.String()
}

func NewWorkflowExecutor(s storage.Storage, pr PolicyRefresher) *multiworkflow.MultiWorkflow {
	workflowMap := make(map[string]cereal.WorkflowExecutor)
	lock := sync.Mutex{}

	applyStagedRulesExecutor := patterns.NewSingleTaskWorkflowExecutor("authz/ApplyStagedRules", true)
	// The code below allows us to easily add to the ProjectUpdateDomainServices list.
	// By not preconfiguring our workflow with an exact set of subworkflow executors,
	// we can generate the workflow executor for the correct domain service easily.
	// The logic only depends on the name. This means that an instance of authz that
	// didn't know about a domain service can still drive its workflow.
	domainSvcExecutor := multiworkflow.NewMultiWorkflowExecutor(func(key string) (cereal.WorkflowExecutor, bool) {
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
	// return chainworkflow.NewChainWorkflowExecutor(NewApplyStagedRulesExecutor(), domainSvcExecutor)
	return domainSvcExecutor
}

type ApplyStagedRulesTaskExecutor struct {
	store           storage.Storage
	policyRefresher PolicyRefresher
	log             logger.Logger
}

type ApplyStagedRulesResult struct {
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

func RegisterCerealProjectUpdateManager(manager *cereal.Manager, s storage.Storage, pr PolicyRefresher) (ProjectUpdateMgr, error) {
	domainServicesWorkflowExecutor := NewWorkflowExecutor(s, pr)

	if err := manager.RegisterWorkflowExecutor(ProjectUpdateWorkflowName, domainServicesWorkflowExecutor); err != nil {
		return nil, err
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
	return multiworkflow.EnqueueWorkflow(context.Background(), m.manager, m.workflowName, m.instanceName, m.domainServices, params)
}

func (m *CerealProjectUpdateManager) Failed() bool {
	instance, err := multiworkflow.GetWorkflowInstance(context.Background(), m.manager, m.workflowName, m.instanceName)
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return false
		}
		logrus.WithError(err).Error("failed to get workflow instance")
		return true
	}

	if instance.Err() != nil {
		return true
	}

	var payload *multiworkflow.MultiWorkflowPayload
	if instance.IsRunning() {
		logrus.Info("Running")
		var err error
		payload, err = instance.GetPayload()
		if err != nil {
			logrus.WithError(err).Error("failed to get workflow instance payload")
			return true
		}
	} else {
		logrus.Info("Not running")
		var err error
		payload, err = instance.GetResult()
		if err != nil {
			logrus.WithError(err).Error("failed to get workflow instance payload")
			return true
		}
	}

	// OnStart has not run yet
	if payload == nil || payload.State == nil {
		return false
	}

	for _, d := range m.domainServices {
		subWorkflow, err := instance.GetSubWorkflow(d)
		if err != nil {
			logrus.Warnf("subworkflow %q not found", d)
			continue
		}

		if subWorkflow.Err() != nil {
			return true
		}
	}

	return false
}

func (m *CerealProjectUpdateManager) FailureMessage() string {
	instance, err := multiworkflow.GetWorkflowInstance(context.Background(), m.manager, m.workflowName, m.instanceName)
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return ""
		}
		logrus.WithError(err).Error("failed to get workflow instance")
		return err.Error()
	}

	if instance.Err() != nil {
		return instance.Err().Error()
	}

	var payload *multiworkflow.MultiWorkflowPayload
	if instance.IsRunning() {
		var err error
		payload, err = instance.GetPayload()
		if err != nil {
			logrus.WithError(err).Error("failed to get workflow instance payload")
			return err.Error()
		}
	} else {
		var err error
		payload, err = instance.GetResult()
		if err != nil {
			logrus.WithError(err).Error("failed to get workflow instance payload")
			return err.Error()
		}
	}

	// OnStart has not run yet
	if payload == nil || payload.State == nil {
		return ""
	}

	errMsg := ""
	for _, d := range m.domainServices {
		subWorkflow, err := instance.GetSubWorkflow(d)
		if err != nil {
			logrus.Warnf("subworkflow %q not found", d)
			continue
		}

		if subWorkflow.Err() != nil {
			errMsg = fmt.Sprintf("%s; %s: %s", errMsg, d, subWorkflow.Err().Error())
		}
	}

	return errMsg
}

func (m *CerealProjectUpdateManager) PercentageComplete() float64 {
	instance, err := multiworkflow.GetWorkflowInstance(context.Background(), m.manager, m.workflowName, m.instanceName)
	if err != nil {
		return 1.0
	}

	if !instance.IsRunning() {
		return 1.0
	}

	percentComplete := 0.0
	for _, d := range m.domainServices {
		subWorkflow, err := instance.GetSubWorkflow(d)
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
			percentComplete = percentComplete + (float64(payload.MergedJobStatus.PercentageComplete) / float64(len(m.domainServices)))
		} else {
			percentComplete = percentComplete + 1.0/float64(len(m.domainServices))
		}
	}

	return percentComplete
}

func (m *CerealProjectUpdateManager) EstimatedTimeComplete() time.Time {
	instance, err := multiworkflow.GetWorkflowInstance(context.Background(), m.manager, m.workflowName, m.instanceName)
	if err != nil || !instance.IsRunning() {
		return time.Now()
	}
	longestEstimatedTimeComplete := time.Time{}
	for _, d := range m.domainServices {
		subWorkflow, err := instance.GetSubWorkflow(d)
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

func (m *CerealProjectUpdateManager) State() string {
	instance, err := m.manager.GetWorkflowInstanceByName(context.Background(), m.instanceName, m.workflowName)
	if err == cereal.ErrWorkflowInstanceNotFound {
		return ProjectUpdateNotRunningState
	}
	if err != nil {
		logrus.WithError(err).Error("failed to get workflow instance")
		return ProjectUpdateUnknownState
	}
	if instance.IsRunning() {
		return ProjectUpdateRunningState
	}

	return ProjectUpdateNotRunningState
}
