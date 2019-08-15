package v2

import (
	"context"
	"fmt"
	"time"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/multiworkflow"

	"github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"

	project_update_tags "github.com/chef/automate/lib/authz"
)

const (
	ProjectUpdateRunningState    = "running"
	ProjectUpdateNotRunningState = "not_running"
	ProjectUpdateUnknownState    = "unknown"

	ProjectUpdateWorkflowName = "ProjectUpdate"
	ProjectUpdateInstanceName = "Singleton"
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

func NewWorkflowExecutor(domainServices []string) *multiworkflow.MultiWorkflow {
	workflowMap := make(map[string]cereal.WorkflowExecutor, len(domainServices))
	for _, d := range domainServices {
		workflowMap[d] = project_update_tags.NewWorkflowExecutorForDomainService(d)
	}
	return multiworkflow.NewMultiWorkflowExecutor(workflowMap)
}

type CerealProjectUpdateManager struct {
	manager        *cereal.Manager
	workflowName   string
	instanceName   string
	domainServices []string
}

func RegisterCerealProjectUpdateManager(manager *cereal.Manager) (ProjectUpdateMgr, error) {
	domainServicesWorkflowExecutor := NewWorkflowExecutor(ProjectUpdateDomainServices)

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
	return multiworkflow.EnqueueWorkflow(context.Background(), m.manager, m.workflowName, m.instanceName, params)
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
		var err error
		payload, err = instance.GetPayload()
		if err != nil {
			logrus.WithError(err).Error("failed to get workflow instance payload")
			return true
		}
	} else {
		var err error
		payload, err = instance.GetResult()
		if err != nil {
			logrus.WithError(err).Error("failed to get workflow instance payload")
			return true
		}
	}

	// OnStart has not run yet
	if payload == nil {
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
	if payload == nil {
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
