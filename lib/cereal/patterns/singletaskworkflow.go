package patterns

import (
	"context"
	"encoding/json"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

// A SingleTaskWorkflowExecutor handles running a workflow composed of
// a single task. The workflow enqueues the task and startup. The
// workflow fails if the task fails.
type SingleTaskWorkflowExecutor struct {
	taskName    string
	allowCancel bool
}

func NewSingleTaskWorkflowExecutor(taskName string, allowCancel bool) *SingleTaskWorkflowExecutor {
	return &SingleTaskWorkflowExecutor{
		taskName:    taskName,
		allowCancel: allowCancel,
	}
}

func (s *SingleTaskWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	var params json.RawMessage
	err := w.GetParameters(&params)
	if err != nil {
		logrus.WithError(err).Error("failed to get parameters")
		return w.Fail(err)
	}

	if err := w.EnqueueTask(s.taskName, params); err != nil {
		logrus.WithError(err).Errorf("failed to enqueue task %s", s.taskName)
		return w.Fail(err)
	}
	return w.Continue(nil)
}

func (s *SingleTaskWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	var result json.RawMessage
	if err := ev.Result.Err(); err != nil {
		logrus.WithError(err).Error("task failed")
		return w.Fail(err)
	}
	if err := ev.Result.Get(&result); err != nil {
		logrus.WithError(err).Error("failed to get task result")
		return w.Fail(err)
	}
	return w.Complete(cereal.WithResult(result))
}

func (s *SingleTaskWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	if s.allowCancel {
		return w.Fail(context.Canceled)
	} else {
		return w.Continue(nil)
	}
}
