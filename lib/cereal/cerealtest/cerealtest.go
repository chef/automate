package cerealtest

import (
	"github.com/chef/automate/lib/cereal"
)

type workflowExecutorWrapper struct {
	onStart        func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision
	onTaskComplete func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision
	onCancel       func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision
}

func NewWorkflowExecutor(
	onStart func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision,
	onTaskComplete func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision,
	onCancel func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision,
) cereal.WorkflowExecutor {
	return &workflowExecutorWrapper{
		onStart:        onStart,
		onTaskComplete: onTaskComplete,
		onCancel:       onCancel,
	}
}

func (wrapper *workflowExecutorWrapper) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	return wrapper.onStart(w, ev)
}

func (wrapper *workflowExecutorWrapper) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {
	return wrapper.onTaskComplete(w, ev)
}

func (wrapper *workflowExecutorWrapper) OnCancel(w cereal.WorkflowInstance,
	ev cereal.CancelEvent) cereal.Decision {
	return wrapper.onCancel(w, ev)
}
