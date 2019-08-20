package patterns

import (
	"encoding/json"

	"github.com/chef/automate/lib/cereal"
	"github.com/sirupsen/logrus"
)

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
		w.Complete()
	}

	if err := w.EnqueueTask(s.taskName, params); err != nil {
		return w.Fail(err)
	}
	return w.Continue(nil)
}

func (s *SingleTaskWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	var result json.RawMessage
	if err := ev.Result.Err(); err != nil {
		return w.Fail(err)
	}
	if err := ev.Result.Get(&result); err != nil {
		return w.Fail(err)
	}
	return w.Complete(cereal.WithResult(result))
}

func (s *SingleTaskWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	if s.allowCancel {
		return w.Complete()
	} else {
		return w.Continue(nil)
	}
}
