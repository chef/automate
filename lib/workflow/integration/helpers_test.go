package integration_test

import (
	"context"
	"time"

	"github.com/chef/automate/lib/workflow"
	"github.com/stretchr/testify/suite"
)

type taskExecutor struct {
	Name     string
	Executor workflow.TaskExecutor
}
type workflowExecutor struct {
	Name     string
	Executor workflow.WorkflowExecutor
}

type workflowManagerOpt struct {
	TaskExecutors     []taskExecutor
	WorkflowExecutors []workflowExecutor
	NoStart           bool
}
type workflowManagerOptFunc func(*workflowManagerOpt)

func WithWorkflowExecutor(name string, executor workflow.WorkflowExecutor) workflowManagerOptFunc {
	return func(o *workflowManagerOpt) {
		o.WorkflowExecutors = append(o.WorkflowExecutors, workflowExecutor{
			Name:     name,
			Executor: executor,
		})
	}
}

func WithTaskExecutor(name string, executor workflow.TaskExecutor) workflowManagerOptFunc {
	return func(o *workflowManagerOpt) {
		o.TaskExecutors = append(o.TaskExecutors, taskExecutor{
			Name:     name,
			Executor: executor,
		})
	}
}

func WithNoStart() workflowManagerOptFunc {
	return func(o *workflowManagerOpt) {
		o.NoStart = true
	}
}

type taskExecutorWrapper struct {
	f func(context.Context, workflow.Task) (interface{}, error)
}

func (w *taskExecutorWrapper) Run(ctx context.Context, task workflow.Task) (interface{}, error) {
	return w.f(ctx, task)
}

func WithTaskExecutorF(name string, f func(context.Context, workflow.Task) (interface{}, error)) workflowManagerOptFunc {
	return WithTaskExecutor(name, &taskExecutorWrapper{f})
}

type workflowExecutorWrapper struct {
	onStart        func(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision
	onTaskComplete func(w workflow.WorkflowInstance, ev workflow.TaskCompleteEvent) workflow.Decision
	onCancel       func(w workflow.WorkflowInstance, ev workflow.CancelEvent) workflow.Decision
}

func (wrapper *workflowExecutorWrapper) OnStart(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision {
	return wrapper.onStart(w, ev)
}

func (wrapper *workflowExecutorWrapper) OnTaskComplete(w workflow.WorkflowInstance,
	ev workflow.TaskCompleteEvent) workflow.Decision {
	return wrapper.onTaskComplete(w, ev)
}

func (wrapper *workflowExecutorWrapper) OnCancel(w workflow.WorkflowInstance,
	ev workflow.CancelEvent) workflow.Decision {
	return wrapper.onCancel(w, ev)
}

func randName(name string) string {
	now := time.Now()
	return name + now.String()
}

type WorkflowTestSuite struct {
	suite.Suite
	newManager func(...workflowManagerOptFunc) *workflow.WorkflowManager
}
