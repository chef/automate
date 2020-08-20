package authz

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/cereal"
)

const (
	cancelUpdateProjectTagsTaskName = "CancelUpdateProjectTags"
	startProjectTagUpdaterTaskName  = "StartProjectTagUpdater"
	projectTagUpdaterStatusTaskName = "ProjectTagUpdaterStatus"
)

var ErrNoJobIDs = errors.New("0 Job IDs returned")

type DomainProjectUpdateWorkflowExecutor struct {
	PollInterval time.Duration

	cancelUpdateProjectTagsTaskName cereal.TaskName
	startProjectTagUpdaterTaskName  cereal.TaskName
	projectTagUpdaterStatusTaskName cereal.TaskName
}

type DomainProjectUpdateWorkflowParameters struct {
	ProjectUpdateID string
}

type DomainProjectUpdateWorkflowPayload struct {
	ProjectUpdateID             string
	JobIDs                      []string
	ConsecutiveJobCheckFailures int
	MergedJobStatus             JobStatus
	Canceled                    bool
}

type StartResult struct {
	IsComplete bool
	JobIDs     []string
}

type StatusParameters struct {
	JobIDs []string
}

func (m *DomainProjectUpdateWorkflowExecutor) OnStart(
	w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {

	logrus.Info("OnStart for domain")
	params := DomainProjectUpdateWorkflowParameters{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	logrus.Debugf("Started DomainProjectUpdateWorkflow for %s",
		params.ProjectUpdateID)
	if err := w.EnqueueTask(m.startProjectTagUpdaterTaskName, nil); err != nil {
		return w.Fail(err)
	}
	return w.Continue(DomainProjectUpdateWorkflowPayload{
		ProjectUpdateID: params.ProjectUpdateID,
	})
}

func (m *DomainProjectUpdateWorkflowExecutor) OnTaskComplete(
	w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {

	payload := DomainProjectUpdateWorkflowPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Error("Failed to deserialize payload")
		return w.Fail(err)
	}

	switch ev.TaskName {
	case m.cancelUpdateProjectTagsTaskName:
		if err := ev.Result.Err(); err != nil {
			return w.Fail(err)
		}
		return w.Complete(cereal.WithResult(payload))
	case m.startProjectTagUpdaterTaskName:
		result := StartResult{}
		if err := ev.Result.Get(&result); err != nil {
			logrus.WithError(err).Errorf(
				"Failed to deserialize %s result", m.startProjectTagUpdaterTaskName)
			return w.Fail(err)
		}
		if result.IsComplete {
			payload.MergedJobStatus = JobStatus{
				Completed:          true,
				PercentageComplete: 1.0,
			}
			return w.Complete(cereal.WithResult(payload))
		}
		if len(result.JobIDs) <= 0 {
			err := ErrNoJobIDs
			logrus.WithError(err).Errorf(
				"Failed to deserialize %s result", m.startProjectTagUpdaterTaskName)
			return w.Fail(err)
		}
		payload.JobIDs = result.JobIDs
		if err := w.EnqueueTask(
			m.projectTagUpdaterStatusTaskName,
			StatusParameters{result.JobIDs}, cereal.StartAfter(m.nextCheck())); err != nil {
			return w.Fail(err)
		}
		return w.Continue(payload)
	case m.projectTagUpdaterStatusTaskName:
		if err := ev.Result.Err(); err != nil {
			payload.ConsecutiveJobCheckFailures++
			if payload.ConsecutiveJobCheckFailures > maxNumberOfConsecutiveFails {
				return w.Fail(err)
			}
			if err := w.EnqueueTask(
				m.projectTagUpdaterStatusTaskName,
				StatusParameters{payload.JobIDs},
				cereal.StartAfter(m.nextCheck())); err != nil {
				return w.Fail(err)
			}
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
		if err := w.EnqueueTask(
			m.projectTagUpdaterStatusTaskName,
			StatusParameters{payload.JobIDs},
			cereal.StartAfter(m.nextCheck())); err != nil {
			return w.Fail(err)
		}
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

	payload.Canceled = true

	if err := w.EnqueueTask(m.cancelUpdateProjectTagsTaskName, payload); err != nil {
		return w.Fail(err)
	}
	return w.Continue(payload)
}

func (m *DomainProjectUpdateWorkflowExecutor) nextCheck() time.Time {
	return time.Now().Add(m.PollInterval)
}

type ESCancelUpdateProjectTagsTask struct {
	esClient EsClient
}

func (m *ESCancelUpdateProjectTagsTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	logrus.Info("ESCancelUpdateProjectTagsTask running")
	params := DomainProjectUpdateWorkflowPayload{}
	if err := task.GetParameters(&params); err != nil {
		logrus.WithError(err).Error("Could not deserialize ESCancelUpdateProjectTagsTask params")
		return nil, err
	}

	for _, jobID := range params.JobIDs {
		logrus.Infof("Canceling ES Job %s", jobID)
		if err := m.esClient.JobCancel(ctx, jobID); err != nil {
			logrus.WithError(err).Errorf("Failed to cancel ES Job %s", jobID)
			return nil, err
		}
	}

	return nil, nil
}

type ESStartProjectTagUpdaterTask struct {
	esClient                   EsClient
	authzProjectsServiceClient authz.ProjectsServiceClient
}

func (m *ESStartProjectTagUpdaterTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {
	jobIDs, err := m.startProjectTagUpdater(ctx)
	if err != nil {
		return nil, err
	}
	return StartResult{
		IsComplete: false,
		JobIDs:     jobIDs,
	}, nil
}

func (m *ESStartProjectTagUpdaterTask) startProjectTagUpdater(ctx context.Context) ([]string, error) {
	logrus.Info("starting es project updater")

	projectCollectionRulesResp, err := m.authzProjectsServiceClient.ListRulesForAllProjects(ctx,
		&authz.ListRulesForAllProjectsReq{})
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

type ESProjectTagUpdaterStatusTask struct {
	esClient EsClient
}

func (m *ESProjectTagUpdaterStatusTask) Run(
	ctx context.Context, task cereal.Task) (interface{}, error) {

	params := StatusParameters{}
	if err := task.GetParameters(&params); err != nil {
		logrus.WithError(err).Error("Could not deserialize ESProjectTagUpdaterStatusTask params")
		return nil, err
	}

	jobStatuses, err := m.collectJobStatus(ctx, params.JobIDs)
	if err != nil {
		return nil, errors.Errorf("Failed to check the running job: %v", err)
	}

	mergedJobStatus := FindSlowestJobStatus(jobStatuses)
	return mergedJobStatus, nil
}

func (m *ESProjectTagUpdaterStatusTask) collectJobStatus(ctx context.Context, esJobIDs []string) ([]JobStatus, error) {
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

func StartProjectTagUpdaterTaskName(svcName string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", svcName, startProjectTagUpdaterTaskName))
}

func ProjectTagUpdaterStatusTaskName(svcName string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", svcName, projectTagUpdaterStatusTaskName))
}

func CancelUpdateProjectTagsTaskName(svcName string) cereal.TaskName {
	return cereal.NewTaskName(fmt.Sprintf("%s/%s", svcName, cancelUpdateProjectTagsTaskName))
}

func NewWorkflowExecutorForDomainService(domainService string) *DomainProjectUpdateWorkflowExecutor {
	return &DomainProjectUpdateWorkflowExecutor{
		PollInterval: 10 * time.Second,

		cancelUpdateProjectTagsTaskName: CancelUpdateProjectTagsTaskName(domainService),
		startProjectTagUpdaterTaskName:  StartProjectTagUpdaterTaskName(domainService),
		projectTagUpdaterStatusTaskName: ProjectTagUpdaterStatusTaskName(domainService),
	}
}

func RegisterTaskExecutors(manager *cereal.Manager, domainService string, esClient EsClient, authzProjectsServiceClient authz.ProjectsServiceClient) error {
	cancelUpdateProjectTagsTaskName := CancelUpdateProjectTagsTaskName(domainService)
	startProjectTagUpdaterTaskName := StartProjectTagUpdaterTaskName(domainService)
	projectTagUpdaterStatusTaskName := ProjectTagUpdaterStatusTaskName(domainService)

	taskExecutorOpts := cereal.TaskExecutorOpts{
		Workers: 1,
	}

	cancelTaskExecutor := &ESCancelUpdateProjectTagsTask{
		esClient: esClient,
	}
	if err := manager.RegisterTaskExecutor(cancelUpdateProjectTagsTaskName, cancelTaskExecutor,
		taskExecutorOpts); err != nil {
		return err
	}

	startTagsUpdaterTask := &ESStartProjectTagUpdaterTask{
		esClient:                   esClient,
		authzProjectsServiceClient: authzProjectsServiceClient,
	}
	if err := manager.RegisterTaskExecutor(startProjectTagUpdaterTaskName, startTagsUpdaterTask,
		taskExecutorOpts); err != nil {
		return err
	}

	statusTask := &ESProjectTagUpdaterStatusTask{
		esClient: esClient,
	}
	if err := manager.RegisterTaskExecutor(projectTagUpdaterStatusTaskName, statusTask,
		taskExecutorOpts); err != nil {
		return err
	}

	return nil
}
