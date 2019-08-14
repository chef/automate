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

type statusParameters struct {
	EsJobIDs []string
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
		w.EnqueueTask(m.projectTagUpdaterStatusTaskName, statusParameters{esJobIDs}, cereal.StartAfter(m.nextCheck())) // nolint: errcheck
		return w.Continue(payload)
	case m.projectTagUpdaterStatusTaskName:
		if err := ev.Result.Err(); err != nil {
			payload.ConsecutiveJobCheckFailures++
			if payload.ConsecutiveJobCheckFailures > maxNumberOfConsecutiveFails {
				return w.Fail(err)
			}
			w.EnqueueTask(m.projectTagUpdaterStatusTaskName, statusParameters{payload.ESJobID}, cereal.StartAfter(m.nextCheck())) // nolint: errcheck
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
		w.EnqueueTask(m.projectTagUpdaterStatusTaskName, statusParameters{payload.ESJobID}, cereal.StartAfter(m.nextCheck())) // nolint: errcheck
		return w.Continue(payload)
	default:
		return w.Fail(errors.Errorf("Unknown task type %q", ev.TaskName))
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

	params := statusParameters{}
	if err := task.GetParameters(&params); err != nil {
		logrus.WithError(err).Error("Could not deserialize ProjectTagUpdaterStatusTask params")
		return nil, err
	}

	jobStatuses, err := m.collectJobStatus(ctx, params.EsJobIDs)
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

func NewWorkflowExecutorForDomainService(domainService string) *DomainProjectUpdateWorkflowExecutor {
	return &DomainProjectUpdateWorkflowExecutor{
		PollInterval: 10 * time.Second,

		cancelUpdateProjectTagsTaskName: fmt.Sprintf("%s/%s", domainService, cancelUpdateProjectTagsTaskName),
		startProjectTagUpdaterTaskName:  fmt.Sprintf("%s/%s", domainService, startProjectTagUpdaterTaskName),
		projectTagUpdaterStatusTaskName: fmt.Sprintf("%s/%s", domainService, projectTagUpdaterStatusTaskName),
	}
}

func RegisterTaskExecutors(manager *cereal.Manager, domainService string, esClient EsClient, authzProjectsClient iam_v2.ProjectsClient) error {
	cancelUpdateProjectTagsTaskName := fmt.Sprintf("%s/%s", domainService, cancelUpdateProjectTagsTaskName)
	startProjectTagUpdaterTaskName := fmt.Sprintf("%s/%s", domainService, startProjectTagUpdaterTaskName)
	projectTagUpdaterStatusTaskName := fmt.Sprintf("%s/%s", domainService, projectTagUpdaterStatusTaskName)

	taskExecutorOpts := cereal.TaskExecutorOpts{
		Workers: 1,
	}

	cancelTaskExecutor := &CancelUpdateProjectTagsTask{
		esClient: esClient,
	}
	if err := manager.RegisterTaskExecutor(cancelUpdateProjectTagsTaskName, cancelTaskExecutor,
		taskExecutorOpts); err != nil {
		return err
	}

	startTagsUpdaterTask := &StartProjectTagUpdaterTask{
		esClient:            esClient,
		authzProjectsClient: authzProjectsClient,
	}
	if err := manager.RegisterTaskExecutor(startProjectTagUpdaterTaskName, startTagsUpdaterTask,
		taskExecutorOpts); err != nil {
		return err
	}

	statusTask := &ProjectTagUpdaterStatusTask{
		esClient: esClient,
	}
	if err := manager.RegisterTaskExecutor(projectTagUpdaterStatusTaskName, statusTask,
		taskExecutorOpts); err != nil {
		return err
	}

	return nil
}
