package authz

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/lib/cereal"
)

const (
	cancelUpdateProjectTagsTaskName = "CancelUpdateProjectTags"
	startProjectTagUpdaterTaskName  = "StartProjectTagUpdater"
	projectTagUpdaterStatusTaskName = "ProjectTagUpdaterStatus"
)

type DomainProjectUpdateWorkflowExecutor struct {
	producer     string
	esClient     EsClient
	PollInterval time.Duration

	cancelUpdateProjectTagsTaskName string
	startProjectTagUpdaterTaskName  string
	projectTagUpdaterStatusTaskName string
}

type DomainProjectUpdateWorkflowParameters struct {
	ProjectUpdateID string
}

type DomainProjectUpdateWorkflowPayload struct {
	ProjectUpdateID             string
	ESJobID                     []string
	ConsecutiveJobCheckFailures int
	MergedJobStatus             JobStatus
}

func (m *DomainProjectUpdateWorkflowExecutor) OnStart(
	w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {

	params := DomainProjectUpdateWorkflowParameters{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	logrus.Debugf("Started DomainProjectUpdateWorkflow for %s",
		params.ProjectUpdateID)
	w.EnqueueTask(m.startProjectTagUpdaterTaskName, nil) // nolint: errcheck
	return w.Continue(DomainProjectUpdateWorkflowPayload{
		ProjectUpdateID: params.ProjectUpdateID,
	})
}

func (m *DomainProjectUpdateWorkflowExecutor) OnTaskComplete(
	w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {

	payload := DomainProjectUpdateWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Errorf("Failed to launch %s", m.cancelUpdateProjectTagsTaskName)
		return w.Fail(err)
	}

	switch ev.TaskName {
	case m.cancelUpdateProjectTagsTaskName:
		return w.Complete()
	case m.startProjectTagUpdaterTaskName:
		esJobIDs := []string{}
		if err := ev.Result.Get(&esJobIDs); err != nil {
			logrus.WithError(err).Errorf(
				"Failed to deserialize %s result", m.startProjectTagUpdaterTaskName)
			return w.Fail(err)
		}
		if len(esJobIDs) <= 0 {
			err := errors.New("0 ES jobs started")
			logrus.WithError(err).Errorf(
				"Failed to deserialize %s result", m.startProjectTagUpdaterTaskName)
			return w.Fail(err)
		}
		w.EnqueueTask(m.projectTagUpdaterStatusTaskName, esJobIDs,
			cereal.StartAfter(m.nextCheck())) // nolint: errcheck
		return w.Continue(payload)
	case m.projectTagUpdaterStatusTaskName:
		if err := ev.Result.Err(); err != nil {
			payload.ConsecutiveJobCheckFailures++
			if payload.ConsecutiveJobCheckFailures > maxNumberOfConsecutiveFails {
				return w.Fail(err)
			}
			w.EnqueueTask(m.projectTagUpdaterStatusTaskName, payload.ESJobID,
				cereal.StartAfter(m.nextCheck())) // nolint: errcheck
			return w.Continue(payload)
		}
		mergedJobStatus := JobStatus{}
		if err := ev.Result.Get(&mergedJobStatus); err != nil {
			// TODO (jaym): Should we cancel here?
			return w.Fail(err)
		}
		payload.ConsecutiveJobCheckFailures = 0
		payload.MergedJobStatus = mergedJobStatus
		if mergedJobStatus.Completed {
			return w.Complete(cereal.WithResult(payload))
		}
		w.EnqueueTask(m.projectTagUpdaterStatusTaskName, payload.ESJobID,
			cereal.StartAfter(m.nextCheck())) // nolint: errcheck
		return w.Continue(payload)
	default:
		return w.Fail(errors.New("Unknown task type"))
	}

}

func (m *DomainProjectUpdateWorkflowExecutor) OnCancel(
	w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	payload := DomainProjectUpdateWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Errorf("Failed to launch %s", m.cancelUpdateProjectTagsTaskName)
		return w.Fail(err)
	}

	w.EnqueueTask(m.cancelUpdateProjectTagsTaskName, payload) // nolint: errcheck
	return w.Continue(payload)
}

func (m *DomainProjectUpdateWorkflowExecutor) nextCheck() time.Time {
	return time.Now().Add(m.PollInterval)
}

type CancelUpdateProjectTagsTask struct {
	esClient EsClient
}

func (m *CancelUpdateProjectTagsTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	logrus.Info("CancelUpdateProjectTagsTask running")
	params := DomainProjectUpdateWorkflowPayload{}
	if err := task.GetParameters(&params); err != nil {
		logrus.WithError(err).Error("Could not deserialize CancelUpdateProjectTagsTask params")
		return nil, err
	}

	for _, jobID := range params.ESJobID {
		logrus.Infof("Canceling ES Job %s", jobID)
		if err := m.esClient.JobCancel(ctx, jobID); err != nil {
			logrus.WithError(err).Errorf("Failed to cancel ES Job %s", jobID)
			return nil, err
		}
	}

	return nil, nil
}

type StartProjectTagUpdaterTask struct {
	esClient            EsClient
	authzProjectsClient iam_v2.ProjectsClient
}

func (m *StartProjectTagUpdaterTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {
	return m.startProjectTagUpdater(ctx)
}

func (m *StartProjectTagUpdaterTask) startProjectTagUpdater(ctx context.Context) ([]string, error) {
	logrus.Info("starting project updater")

	projectCollectionRulesResp, err := m.authzProjectsClient.ListRulesForAllProjects(ctx,
		&iam_v2.ListRulesForAllProjectsReq{})
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to get authz project rules")
	}

	esJobIDs, err := m.esClient.UpdateProjectTags(ctx, projectCollectionRulesResp.ProjectRules)
	if err != nil {
		return []string{}, errors.Wrap(err, "Failed to start Elasticsearch Node project tags update")
	}

	logrus.Infof("Started Project rule update with job IDs: %v", esJobIDs)

	return esJobIDs, nil
}

type ProjectTagUpdaterStatusTask struct {
	esClient EsClient
}

func (m *ProjectTagUpdaterStatusTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	esJobIDs := []string{}
	if err := task.GetParameters(&esJobIDs); err != nil {
		logrus.WithError(err).Error("Could not deserialize ProjectTagUpdaterStatusTask params")
		return nil, err
	}

	jobStatuses, err := m.collectJobStatus(ctx, esJobIDs)
	if err != nil {
		return nil, errors.Errorf("Failed to check the running job: %v", err)
	}

	mergedJobStatus := FindSlowestJobStatus(jobStatuses)
	return mergedJobStatus, nil
}

func (m *ProjectTagUpdaterStatusTask) collectJobStatus(ctx context.Context, esJobIDs []string) ([]JobStatus, error) {
	jobStatuses := make([]JobStatus, len(esJobIDs))
	for index, esJobID := range esJobIDs {
		jobStatus, err := m.esClient.JobStatus(ctx, esJobID)
		if err != nil {
			return jobStatuses, err
		}

		jobStatuses[index] = jobStatus
	}

	return jobStatuses, nil
}

type WorkflowProjectUpdateManager struct {
	c            *cereal.Manager
	workflowName string
	instanceName string
}

func (manager *WorkflowProjectUpdateManager) Cancel(projectUpdateID string) {
	err := manager.c.CancelWorkflow(context.Background(), manager.workflowName,
		manager.instanceName)
	if err != nil {
		if err != cereal.ErrWorkflowInstanceNotFound {
			logrus.WithError(err).Error("failed to cancel workflow")
		}
	}
}

func (manager *WorkflowProjectUpdateManager) Start(projectUpdateID string) {
	err := manager.c.EnqueueWorkflow(context.Background(), manager.workflowName,
		manager.instanceName, DomainProjectUpdateWorkflowParameters{
			ProjectUpdateID: projectUpdateID,
		})
	if err != nil {
		logrus.WithError(err).Error("failed to enqueue project update workflow")
	}
}

func (manager *WorkflowProjectUpdateManager) PercentageComplete() float32 {
	jobStatus, running, err := manager.getJobStatus()
	if err != nil || !running || jobStatus == nil {
		return 1
	}
	return jobStatus.PercentageComplete
}

func (manager *WorkflowProjectUpdateManager) EstimatedTimeComplete() time.Time {
	jobStatus, running, err := manager.getJobStatus()
	if err != nil || !running || jobStatus == nil {
		return time.Time{}
	}
	return time.Unix(jobStatus.EstimatedEndTimeInSec, 0)
}

func (manager *WorkflowProjectUpdateManager) State() string {
	_, running, err := manager.getJobStatus()

	if err != nil {
		return "unknown"
	}

	if running {
		return RunningState
	} else {
		return NotRunningState
	}
}

func (manager *WorkflowProjectUpdateManager) getJobStatus() (jobStatus *JobStatus, running bool, err error) {
	a, err := manager.c.GetWorkflowInstanceByName(context.Background(),
		manager.instanceName, manager.workflowName)
	if err != nil {
		if err == cereal.ErrWorkflowInstanceNotFound {
			return nil, false, nil
		}
		logrus.WithError(err).Error("Failed to get state")
		return nil, false, err
	}

	if a.IsRunning() {
		jobStatus := JobStatus{}
		if err := a.GetPayload(&jobStatus); err != nil {
			return nil, true, nil
		}

		return &jobStatus, true, nil
	} else {
		return nil, false, nil
	}
}

func RegisterWorkflow(manager *cereal.Manager, instanceName string, workflowName string,
	esClient EsClient, authzProjectsClient iam_v2.ProjectsClient) (*WorkflowProjectUpdateManager, error) {

	workflowExecutor := &DomainProjectUpdateWorkflowExecutor{
		esClient:     esClient,
		PollInterval: 10 * time.Second,

		cancelUpdateProjectTagsTaskName: fmt.Sprintf("%s/%s", workflowName, cancelUpdateProjectTagsTaskName),
		startProjectTagUpdaterTaskName:  fmt.Sprintf("%s/%s", workflowName, startProjectTagUpdaterTaskName),
		projectTagUpdaterStatusTaskName: fmt.Sprintf("%s/%s", workflowName, projectTagUpdaterStatusTaskName),
	}
	if err := manager.RegisterWorkflowExecutor(workflowName, workflowExecutor); err != nil {
		return nil, err
	}

	taskExecutorOpts := cereal.TaskExecutorOpts{
		Workers: 1,
	}

	cancelTaskExecutor := &CancelUpdateProjectTagsTask{
		esClient: esClient,
	}
	if err := manager.RegisterTaskExecutor(workflowExecutor.cancelUpdateProjectTagsTaskName, cancelTaskExecutor,
		taskExecutorOpts); err != nil {
		return nil, err
	}

	startTagsUpdaterTask := &StartProjectTagUpdaterTask{
		esClient:            esClient,
		authzProjectsClient: authzProjectsClient,
	}
	if err := manager.RegisterTaskExecutor(workflowExecutor.startProjectTagUpdaterTaskName, startTagsUpdaterTask,
		taskExecutorOpts); err != nil {
		return nil, err
	}

	statusTask := &ProjectTagUpdaterStatusTask{
		esClient: esClient,
	}
	if err := manager.RegisterTaskExecutor(workflowExecutor.projectTagUpdaterStatusTaskName, statusTask,
		taskExecutorOpts); err != nil {
		return nil, err
	}

	return &WorkflowProjectUpdateManager{
		c:            manager,
		workflowName: workflowName,
		instanceName: instanceName,
	}, nil
}
