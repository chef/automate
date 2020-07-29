package project_purger_workflow

import (
	"context"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type moveProjectToGraveyardWorkflowExecutor struct {
}

type moveProjectToGraveyardWorkflowExecutorParams struct {
	ProjectID string
}

type moveProjectToGraveyardWorkflowExecutorPayload struct {
}

// NewMoveProjectToGraveyardWorkflowExecutor returns a workflow executor that
// will retry on ErrTaskLost but fail on any other error.
func NewMoveProjectToGraveyardWorkflowExecutor() cereal.WorkflowExecutor {
	return &moveProjectToGraveyardWorkflowExecutor{}
}

func (s *moveProjectToGraveyardWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	var params moveProjectToGraveyardWorkflowExecutorParams
	err := w.GetParameters(&params)
	if err != nil {
		logrus.WithError(err).Error("failed to get move to graveyard parameters")
		return w.Fail(err)
	}

	if err := w.EnqueueTask(moveProjectToGraveyardTaskName, moveProjectToGraveyardParams{ProjectID: params.ProjectID}); err != nil {
		logrus.WithError(err).Errorf("failed to enqueue move to graveyard task %s", moveProjectToGraveyardTaskName)
		return w.Fail(err)
	}
	return w.Continue(nil)
}

// OnTaskComplete instantly retries on ErrTaskLost to handle the case where the project move to graveyard
// was completed in postgres but things crashed before the success could be reported. All other errors are
// real database errors (like NotFound) that should be bubbled up.
func (s *moveProjectToGraveyardWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	payload := moveProjectToGraveyardWorkflowExecutorPayload{}
	if err := w.GetPayload(&payload); err != nil {
		logrus.WithError(err).Error("Failed to deserialize move project to graveyard payload")
		return w.Fail(err)
	}

	params := moveProjectToGraveyardWorkflowExecutorParams{}
	if err := w.GetParameters(&params); err != nil {
		return w.Fail(err)
	}

	switch ev.TaskName {
	case moveProjectToGraveyardTaskName:
		if taskErr := ev.Result.Err(); taskErr != nil {
			// if we lost the task, run the move to graveyard again
			if taskErr == cereal.ErrTaskLost {
				logrus.WithError(taskErr).Error("the task was lost, retrying")
				if err := w.EnqueueTask(
					moveProjectToGraveyardTaskName, moveProjectToGraveyardParams{ProjectID: params.ProjectID}); err != nil {
					return w.Fail(err)
				}
			}
			// return database failure
			return w.Fail(taskErr)
		}
		return w.Complete()
	default:
		return w.Fail(errors.Errorf("Unknown task type %q", ev.TaskName))
	}
}

func (s *moveProjectToGraveyardWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Fail(errors.New("force cancel move project to graveyard"))
}

type moveProjectToGraveyardTaskExecutor struct {
	store storage.Storage
	log   logger.Logger
}

type moveProjectToGraveyardResult struct {
}

type moveProjectToGraveyardParams struct {
	ProjectID string
}

func (s *moveProjectToGraveyardTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := moveProjectToGraveyardParams{}
	if err := task.GetParameters(&params); err != nil {
		return nil, errors.Wrap(err, "unmarshall move to graveyard params")
	}

	s.log.Infof("starting project move to graveyard for id %q", params.ProjectID)

	err := s.store.DeleteProject(ctx, params.ProjectID)
	if err != nil {
		// could be storage_errors.ErrNotFound which is the only one we will special case upstream
		return &moveProjectToGraveyardResult{}, err
	}
	return &moveProjectToGraveyardResult{}, nil
}
