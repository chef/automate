package project_purge

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

const (
	startPurgeProjectTaskName = "StartPurgeProjects"
	maxBackoffDuration        = time.Minute
)

type DomainProjectPurgeWorkflowExecutor struct {
	startPurgeProjectTaskName string
}

type DomainProjectPurgeWorkflowParameters struct {
	ProjectID string
}

type DomainProjectPurgeWorkflowPayload struct {
	ConsecutiveJobCheckFailures int
}

func (m *DomainProjectPurgeWorkflowExecutor) OnStart(
	w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {

	logrus.Info("OnStart for domain")
	params := DomainProjectPurgeWorkflowParameters{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	logrus.Debugf("Started DomainProjectPurgeWorkflow for %s",
		params.ProjectID)
	taskParams := DomainProjectPurgeTaskParams{ProjectID: params.ProjectID}
	if err := w.EnqueueTask(m.startPurgeProjectTaskName, taskParams); err != nil {
		return w.Fail(err)
	}
	return w.Continue(DomainProjectPurgeWorkflowPayload{})
}

func (m *DomainProjectPurgeWorkflowExecutor) OnTaskComplete(
	w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {

	payload := DomainProjectPurgeWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Error("Failed to deserialize payload")
		return w.Fail(err)
	}

	params := DomainProjectPurgeWorkflowParameters{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	switch ev.TaskName {
	case m.startPurgeProjectTaskName:
		if errToLog := ev.Result.Err(); errToLog != nil {
			logrus.WithError(errToLog).Error("failed to purge project, retrying")
			payload.ConsecutiveJobCheckFailures++
			if err := w.EnqueueTask(
				m.startPurgeProjectTaskName, DomainProjectPurgeTaskParams{ProjectID: params.ProjectID},
				cereal.StartAfter(m.nextCheck(payload.ConsecutiveJobCheckFailures))); err != nil {
				return w.Fail(err)
			}
			return w.Continue(payload)
		}
		return w.Complete()
	default:
		return w.Fail(errors.Errorf("Unknown task type %q", ev.TaskName))
	}
}

func (m *DomainProjectPurgeWorkflowExecutor) OnCancel(
	w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Fail(errors.New("force cancel project purge"))
}

func (m *DomainProjectPurgeWorkflowExecutor) nextCheck(numberOfFailures int) time.Time {
	return ExponentialNextCheck(numberOfFailures)
}

type DomainProjectPurgeTask struct {
	purgeClient PurgeClient
}

type DomainProjectPurgeTaskParams struct {
	ProjectID string
}

func (m *DomainProjectPurgeTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {
	params := DomainProjectPurgeTaskParams{}
	if err := task.GetParameters(&params); err != nil {
		return nil, errors.Wrap(err, "unmarshall purge project params")
	}

	err := m.startProjectTagUpdater(ctx, params.ProjectID)
	if err != nil {
		return nil, err
	}
	return nil, nil
}

func (m *DomainProjectPurgeTask) startProjectTagUpdater(ctx context.Context, projectID string) error {
	logrus.Info("starting project purges")

	err := m.purgeClient.PurgeProject(ctx, projectID)
	if err != nil {
		return errors.Wrap(err, "failed to launch purge project client")
	}

	return nil
}

func StartProjectPurgeTaskName(svcName string) string {
	return fmt.Sprintf("%s/%s", svcName, startPurgeProjectTaskName)
}

func NewWorkflowExecutorForDomainService(domainService string) *DomainProjectPurgeWorkflowExecutor {
	return &DomainProjectPurgeWorkflowExecutor{
		startPurgeProjectTaskName: StartProjectPurgeTaskName(domainService),
	}
}

func RegisterTaskExecutors(manager *cereal.Manager, domainService string, domainPurgeClient PurgeClient) error {
	startPurgeProjectTaskName := StartProjectPurgeTaskName(domainService)

	taskExecutorOpts := cereal.TaskExecutorOpts{
		Workers: 1,
	}

	startPurgeProjectTask := &DomainProjectPurgeTask{
		purgeClient: domainPurgeClient,
	}
	if err := manager.RegisterTaskExecutor(startPurgeProjectTaskName, startPurgeProjectTask,
		taskExecutorOpts); err != nil {
		return err
	}

	return nil
}
