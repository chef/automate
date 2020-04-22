package project_purger_workflow

import (
	"context"
	"time"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/authz/project_purge"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type deleteProjectFromGraveyardWorkflowExecutor struct {
}

type deleteProjectFromGraveyardWorkflowExecutorParams struct {
	ProjectID string
}

type deleteProjectFromGraveyardWorkflowExecutorPayload struct {
	ConsecutiveJobCheckFailures int
}

// NewDeleteProjectFromGraveyardWorkflowExecutor returns a workflow executor that
// is based on the SingleTaskWorkflowExecutor with infinite retry after exponential backoff.
func NewDeleteProjectFromGraveyardWorkflowExecutor() cereal.WorkflowExecutor {
	return &deleteProjectFromGraveyardWorkflowExecutor{}
}

func (s *deleteProjectFromGraveyardWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	var params deleteProjectFromGraveyardWorkflowExecutorParams
	err := w.GetParameters(&params)
	if err != nil {
		logrus.WithError(err).Error("failed to get delete project from graveyard parameters")
		return w.Fail(err)
	}

	if err := w.EnqueueTask(deleteProjectFromGraveyardTaskName, deleteProjectFromGraveyardParams{ProjectID: params.ProjectID}); err != nil {
		logrus.WithError(err).Errorf("failed to enqueue delete project from graveyard task %s", deleteProjectFromGraveyardTaskName)
		return w.Fail(err)
	}
	return w.Continue(nil)
}

func (s *deleteProjectFromGraveyardWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	payload := deleteProjectFromGraveyardWorkflowExecutorPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Error("Failed to deserialize delete project from graveyard payload")
		return w.Fail(err)
	}

	params := deleteProjectFromGraveyardWorkflowExecutorParams{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	switch ev.TaskName {
	case deleteProjectFromGraveyardTaskName:
		if errToLog := ev.Result.Err(); errToLog != nil {
			logrus.WithError(errToLog).Error("failed to delete project from graveyard, retrying")
			payload.ConsecutiveJobCheckFailures++
			if err := w.EnqueueTask(
				deleteProjectFromGraveyardTaskName, deleteProjectFromGraveyardParams{ProjectID: params.ProjectID},
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

func (m *deleteProjectFromGraveyardWorkflowExecutor) nextCheck(numberOfFailures int) time.Time {
	return project_purge.ExponentialNextCheck(numberOfFailures)
}

func (s *deleteProjectFromGraveyardWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Fail(errors.New("force cancel delete project from graveyard"))

}

type deleteProjectFromGraveyardTaskExecutor struct {
	store storage.Storage
	log   logger.Logger
}

type deleteProjectFromGraveyardResult struct {
}

type deleteProjectFromGraveyardParams struct {
	ProjectID string
}

func (s *deleteProjectFromGraveyardTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := deleteProjectFromGraveyardParams{}
	if err := task.GetParameters(&params); err != nil {
		return nil, errors.Wrap(err, "unmarshall delete from graveyard params")
	}

	s.log.Infof("starting project delete from graveyard for id %q", params.ProjectID)

	return deleteProjectFromGraveyardResult{}, s.store.RemoveProjectFromGraveyard(ctx, params.ProjectID)
}
