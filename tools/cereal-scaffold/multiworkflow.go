package main

import (
	"context"

	"github.com/chef/automate/lib/cereal"
)

type CountingWorkflowExecutor struct {
}

type CountingWorkflowParameters struct {
	NumTasks int
	StartAt  int
}

type CountingWorkflowResult struct {
	Value int
}

func (*CountingWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	w.EnqueueTask("CountByN", nil)
}

func (*CountingWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
}

func (*CountingWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
}

type CountByNTaskExecutor struct {
}

type CountByNTaskParameters struct {
	Value int
}

type CountByNTaskResult struct {
	Value int
}

func (*CountByNTaskExecutor) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := CountByNTaskParameters{}
	if err := task.GetParameters(&params); err != nil {
		return nil, err
	}

	return CountByNTaskResult{
		Value: params.Value,
	}, nil
}
