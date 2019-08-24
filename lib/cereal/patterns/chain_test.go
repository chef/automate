package patterns

import (
	"encoding/json"
	"errors"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/cerealtest"
)

type TestWorkflowExecutor struct {
}

type TestWorkflowParams struct {
	Value string
}

type TestWorkflowPayload struct {
	PayloadValue string
}

type TestTaskParams struct {
	TaskValue string
}

// TestChainSingleOnStart tests the OnStart function when there is 1
// executor in the chain
func TestChainSingleOnStart(t *testing.T) {
	t.Run("fails when parameters invalid", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				require.Fail(t, "Should not get here")
				return w.Continue(nil)
			},
			nil,
			nil,
		))
		require.NoError(t, err)
		// The chain only has one executor, but we're passing 2 parameters
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrIncorrectParameters)
	})

	t.Run("can parse parameters and continue", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		executor, err := NewChainWorkflowExecutor(cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				require.Equal(t, workflow1Params, params)
				w.EnqueueTask("testTask1", nil)
				w.EnqueueTask("testTask2", nil)
				return w.Continue(TestWorkflowPayload{
					PayloadValue: params.Value,
				})
			},
			nil,
			nil,
		))
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		continuing := instance.AssertContinuing()
		continuing.AssertTaskEnqueued("testTask1").AssertCount(1)
		continuing.AssertTaskEnqueued("testTask2").AssertCount(1)

		payload := ChainWorkflowPayload{}
		continuing.GetPayload(&payload)
		// We're not finished
		require.False(t, payload.Finished())
		// There is 1 executing subworkflow
		require.Len(t, payload.State, 1)
		require.False(t, payload.State[0].IsFinished)

		require.Equal(t, 0, payload.State[0].CompletedTasks)
		// We enqueued 2 tasks
		require.Equal(t, 2, payload.State[0].EnqueuedTasks)
		// There was no error
		require.Empty(t, payload.State[0].Err)
		// There is no result yet
		require.Equal(t, "null", string(payload.State[0].Result))
		// We copied the subworkflow parameters into the payload
		subWorkflowPayload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Payload, &subWorkflowPayload))
		require.Equal(t, workflow1Params.Value, subWorkflowPayload.PayloadValue)

	})

	t.Run("can complete", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		executor, err := NewChainWorkflowExecutor(cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				// This enqueued task is ignored as the workflow is ending
				w.EnqueueTask("testTask1", nil)
				return w.Complete(cereal.WithResult(TestWorkflowPayload{
					PayloadValue: workflow1Params.Value,
				}))
			},
			nil,
			nil,
		))
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		// We're finished
		require.True(t, payload.Finished())
		// There is 1 executing subworkflow
		require.Len(t, payload.State, 1)
		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		// There was no error
		require.Empty(t, payload.State[0].Err)
		subWorkflowPayload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &subWorkflowPayload))
		require.Equal(t, workflow1Params.Value, subWorkflowPayload.PayloadValue)
	})

	t.Run("can fail", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		executor, err := NewChainWorkflowExecutor(cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				// This enqueued task is ignored as the workflow is ending
				w.EnqueueTask("testTask1", nil)
				return w.Fail(errors.New("fail"))
			},
			nil,
			nil,
		))
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		// We're finished
		require.True(t, payload.Finished())
		// There is 1 executing subworkflow
		require.Len(t, payload.State, 1)
		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		// There was no error
		require.Equal(t, "fail", payload.State[0].Err)
	})
}

func TestChainMultipleOnStart(t *testing.T) {
	t.Run("fails when parameters invalid", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
				nil,
			))
		require.NoError(t, err)
		// The chain only has one executor, but we're passing 2 parameters
		params, err := ToChainWorkflowParameters([]interface{}{nil})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrIncorrectParameters)
	})

	t.Run("can parse parameters and continue", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2",
		}
		subworkflowExecutor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				require.Equal(t, workflow1Params, params)
				w.EnqueueTask("testTask1", nil)
				w.EnqueueTask("testTask2", nil)
				return w.Continue(TestWorkflowPayload{
					PayloadValue: params.Value,
				})
			},
			nil,
			nil,
		)
		executor, err := NewChainWorkflowExecutor(subworkflowExecutor, subworkflowExecutor)
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params, workflow2Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		continuing := instance.AssertContinuing()
		continuing.AssertTaskEnqueued("testTask1").AssertCount(1)
		continuing.AssertTaskEnqueued("testTask2").AssertCount(1)

		payload := ChainWorkflowPayload{}
		continuing.GetPayload(&payload)
		// We're not finished
		require.False(t, payload.Finished())
		// There is 1 executing subworkflow
		require.Len(t, payload.State, 1)
		require.False(t, payload.State[0].IsFinished)

		require.Equal(t, 0, payload.State[0].CompletedTasks)
		// We enqueued 2 tasks
		require.Equal(t, 2, payload.State[0].EnqueuedTasks)
		// There was no error
		require.Empty(t, payload.State[0].Err)
		// There is no result yet
		require.Equal(t, "null", string(payload.State[0].Result))
		// We copied the subworkflow parameters into the payload
		subWorkflowPayload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Payload, &subWorkflowPayload))
		require.Equal(t, workflow1Params.Value, subWorkflowPayload.PayloadValue)
	})

	t.Run("can complete first", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2",
		}
		workflow2TaskParams := TestTaskParams{
			TaskValue: "taskworkflow2",
		}
		subworkflow1Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				// This should be ignored as this workflow is completing
				w.EnqueueTask("testTask1", nil)
				return w.Complete(cereal.WithResult(TestWorkflowPayload{
					PayloadValue: params.Value,
				}))
			},
			nil,
			nil,
		)
		subworkflow2Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				w.EnqueueTask("testTask1", workflow2TaskParams)
				return w.Continue(TestWorkflowPayload{
					PayloadValue: params.Value,
				})
			},
			nil,
			nil,
		)
		executor, err := NewChainWorkflowExecutor(subworkflow1Executor, subworkflow2Executor)
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params, workflow2Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		continuing := instance.AssertContinuing()

		payload := ChainWorkflowPayload{}
		continuing.GetPayload(&payload)
		tasks := continuing.AssertTaskEnqueued("testTask1").AssertCount(1)
		taskMetadata := ChainWorkflowTaskParam{}
		tasks.Tasks[0].GetParameters(&taskMetadata)
		require.Equal(t, int64(1), taskMetadata.XXX_ChainWorkflowIdx)
		taskParms := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&taskParms)
		require.Equal(t, workflow2TaskParams, taskParms)

		// We're not finished because the second workflow should start
		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Empty(t, payload.State[0].Err)
		subWorkflow1Payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &subWorkflow1Payload))
		require.Equal(t, workflow1Params.Value, subWorkflow1Payload.PayloadValue)

		require.False(t, payload.State[1].IsFinished)
		require.Equal(t, 1, payload.State[1].EnqueuedTasks)
		require.Equal(t, 0, payload.State[1].CompletedTasks)
		require.Empty(t, payload.State[1].Err)
		subWorkflow2Payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[1].Payload, &subWorkflow2Payload))
		require.Equal(t, workflow2Params.Value, subWorkflow2Payload.PayloadValue)

	})

	t.Run("can complete both", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2",
		}
		subworkflow1Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				return w.Complete(cereal.WithResult(TestWorkflowPayload{
					PayloadValue: params.Value,
				}))
			},
			nil,
			nil,
		)
		subworkflow2Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				return w.Complete(cereal.WithResult(TestWorkflowPayload{
					PayloadValue: params.Value,
				}))
			},
			nil,
			nil,
		)
		executor, err := NewChainWorkflowExecutor(subworkflow1Executor, subworkflow2Executor)
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params, workflow2Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Empty(t, payload.State[0].Err)
		subWorkflow1Payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &subWorkflow1Payload))
		require.Equal(t, workflow1Params.Value, subWorkflow1Payload.PayloadValue)

		require.True(t, payload.State[1].IsFinished)
		require.Equal(t, 0, payload.State[1].EnqueuedTasks)
		require.Equal(t, 0, payload.State[1].CompletedTasks)
		require.Empty(t, payload.State[1].Err)
		subWorkflow2Payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[1].Result, &subWorkflow2Payload))
		require.Equal(t, workflow2Params.Value, subWorkflow2Payload.PayloadValue)
	})

	t.Run("can fail second", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2",
		}
		failureErr := errors.New("fail")
		subworkflow1Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				params := TestWorkflowParams{}
				err := w.GetParameters(&params)
				require.NoError(t, err)
				return w.Complete(cereal.WithResult(TestWorkflowPayload{
					PayloadValue: params.Value,
				}))
			},
			nil,
			nil,
		)
		subworkflow2Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				return w.Fail(failureErr)
			},
			nil,
			nil,
		)
		executor, err := NewChainWorkflowExecutor(subworkflow1Executor, subworkflow2Executor)
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params, workflow2Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Empty(t, payload.State[0].Err)
		subWorkflow1Payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &subWorkflow1Payload))
		require.Equal(t, workflow1Params.Value, subWorkflow1Payload.PayloadValue)

		require.True(t, payload.State[1].IsFinished)
		require.Equal(t, 0, payload.State[1].EnqueuedTasks)
		require.Equal(t, 0, payload.State[1].CompletedTasks)
		require.Equal(t, failureErr.Error(), payload.State[1].Err)
	})

	t.Run("stops if first fails", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2",
		}
		failureErr := errors.New("fail")
		subworkflow1Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				return w.Fail(failureErr)
			},
			nil,
			nil,
		)
		subworkflow2Executor := cerealtest.NewWorkflowExecutor(
			func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
				require.Fail(t, "should not get here")
				return w.Complete()
			},
			nil,
			nil,
		)
		executor, err := NewChainWorkflowExecutor(subworkflow1Executor, subworkflow2Executor)
		require.NoError(t, err)

		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params, workflow2Params})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 1)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Equal(t, failureErr.Error(), payload.State[0].Err)
	})
}

func TestChainMultipleOnTaskComplete(t *testing.T) {
	t.Run("fails when workflow parameters invalid", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		tr := cerealtest.NewTaskResult(t)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrIncorrectParameters)
	})

	t.Run("fails when task parameters missing idx", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		tr := cerealtest.NewTaskResult(t)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrTaskWorkflowInvalid)
	})

	t.Run("ignores messages delivered to finished workflow", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished: true,
				},
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 0,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		instance.AssertContinuing()
	})

	t.Run("message delivered to the correct subworkflow can complete", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					// enqueue task is ignored
					w.EnqueueTask("testTask2", nil)
					return w.Complete()
				},
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished: true,
				},
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 1,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.True(t, payload.State[1].IsFinished)
		require.Equal(t, 1, payload.State[1].EnqueuedTasks)
		require.Equal(t, 1, payload.State[1].CompletedTasks)
	})

	t.Run("message delivered to the correct subworkflow can fail", func(t *testing.T) {
		failureErr := errors.New("fail")
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Fail(failureErr)
				},
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished: true,
				},
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 1,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.True(t, payload.State[1].IsFinished)
		require.Equal(t, failureErr.Error(), payload.State[1].Err)
		require.Equal(t, 1, payload.State[1].EnqueuedTasks)
		require.Equal(t, 1, payload.State[1].CompletedTasks)
	})

	t.Run("message delivered to the correct subworkflow can continue", func(t *testing.T) {
		workflow2TaskParams := TestTaskParams{
			TaskValue: "workflow2Task",
		}
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					w.EnqueueTask("task1Name", workflow2TaskParams)
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 2, w.TotalEnqueuedTasks())
					return w.Continue(nil)
				},
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished: true,
				},
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 1,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()

		tasks := continuing.AssertTaskEnqueued("task1Name")
		tasks.AssertCount(1)
		taskMetadata = ChainWorkflowTaskParam{}
		tasks.Tasks[0].GetParameters(&taskMetadata)
		require.Equal(t, int64(1), taskMetadata.XXX_ChainWorkflowIdx)

		taskParams := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, workflow2TaskParams, taskParams)

		payload := ChainWorkflowPayload{}
		continuing.GetPayload(&payload)

		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 0, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.False(t, payload.State[1].IsFinished)
		require.Empty(t, payload.State[1].Err)
		require.Equal(t, 2, payload.State[1].EnqueuedTasks)
		require.Equal(t, 1, payload.State[1].CompletedTasks)
	})

	t.Run("completion starts next subworkflow", func(t *testing.T) {
		workflow2TaskParams := TestTaskParams{
			TaskValue: "workflow2Task",
		}
		workflow1Payload := TestWorkflowPayload{
			PayloadValue: "workflow1Payload",
		}
		workflow2Payload := TestWorkflowPayload{
			PayloadValue: "workflow2Payload",
		}
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(workflow1Payload))
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					w.EnqueueTask("task2Name", workflow2TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Continue(workflow2Payload)
				},
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 0,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()

		tasks := continuing.AssertTaskEnqueued("task2Name")
		tasks.AssertCount(1)
		taskMetadata = ChainWorkflowTaskParam{}
		tasks.Tasks[0].GetParameters(&taskMetadata)
		require.Equal(t, int64(1), taskMetadata.XXX_ChainWorkflowIdx)

		taskParams := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, workflow2TaskParams, taskParams)

		payload := ChainWorkflowPayload{}
		continuing.GetPayload(&payload)

		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 1, payload.State[0].EnqueuedTasks)
		require.Equal(t, 1, payload.State[0].CompletedTasks)
		w1payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &w1payload))
		require.Equal(t, workflow1Payload, w1payload)

		require.False(t, payload.State[1].IsFinished)
		require.Empty(t, payload.State[1].Err)
		require.Equal(t, 1, payload.State[1].EnqueuedTasks)
		require.Equal(t, 0, payload.State[1].CompletedTasks)
		w2payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[1].Payload, &w2payload))
		require.Equal(t, workflow2Payload, w2payload)
	})

	t.Run("failure stops the chain", func(t *testing.T) {
		failureErr := errors.New("fail")
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Fail(failureErr)
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					require.Fail(t, "should not get here")
					return w.Continue(nil)
				},
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 0,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 1)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 1, payload.State[0].EnqueuedTasks)
		require.Equal(t, 1, payload.State[0].CompletedTasks)
		require.Equal(t, failureErr.Error(), payload.State[0].Err)
	})

	t.Run("completion of next subworkflow OnStart completes", func(t *testing.T) {
		workflow1Payload := TestWorkflowPayload{
			PayloadValue: "workflow1Payload",
		}
		workflow2Payload := TestWorkflowPayload{
			PayloadValue: "workflow2Payload",
		}
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(workflow1Payload))
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					return w.Complete(cereal.WithResult(workflow2Payload))
				},
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 0,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 1, payload.State[0].EnqueuedTasks)
		require.Equal(t, 1, payload.State[0].CompletedTasks)
		w1payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &w1payload))
		require.Equal(t, workflow1Payload, w1payload)

		require.True(t, payload.State[1].IsFinished)
		require.Empty(t, payload.State[1].Err)
		require.Equal(t, 0, payload.State[1].EnqueuedTasks)
		require.Equal(t, 0, payload.State[1].CompletedTasks)
		w2payload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[1].Result, &w2payload))
		require.Equal(t, workflow2Payload, w2payload)
	})

	t.Run("failure of next subworkflow OnStart completes", func(t *testing.T) {
		failureErr := errors.New("fail")
		workflow1Payload := TestWorkflowPayload{
			PayloadValue: "workflow1Payload",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(workflow1Payload))
				},
				nil,
			),
			cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					w.GetParameters(&params)
					require.Equal(t, workflow2Params, params)
					return w.Fail(failureErr)
				},
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, workflow2Params})
		require.NoError(t, err)

		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		taskMetadata := ChainWorkflowTaskParam{
			XXX_ChainWorkflowIdx: 0,
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("task1Name", tr)

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 1, payload.State[0].EnqueuedTasks)
		require.Equal(t, 1, payload.State[0].CompletedTasks)
		wpayload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &wpayload))
		require.Equal(t, workflow1Payload, wpayload)

		require.True(t, payload.State[1].IsFinished)
		require.Equal(t, 0, payload.State[1].EnqueuedTasks)
		require.Equal(t, 0, payload.State[1].CompletedTasks)
		require.Equal(t, failureErr.Error(), payload.State[1].Err)
	})
}

func TestChainMultipleOnCancel(t *testing.T) {
	t.Run("fails when workflow parameters invalid", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrIncorrectParameters)
	})

	t.Run("fails when task parameters missing idx", func(t *testing.T) {
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					require.Fail(t, "Should not get here")
					return w.Continue(nil)
				},
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrTaskWorkflowInvalid)
	})

	t.Run("stops chain if OnCancel fails", func(t *testing.T) {
		failureErr := errors.New("fail")
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					return w.Fail(failureErr)
				},
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)
		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 1)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 1, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Equal(t, failureErr.Error(), payload.State[0].Err)
	})

	t.Run("stops chain if OnCancel completes", func(t *testing.T) {
		workflow1Payload := TestWorkflowPayload{
			PayloadValue: "workflow1Payload",
		}
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					return w.Complete(cereal.WithResult(workflow1Payload))
				},
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{nil, nil})
		require.NoError(t, err)
		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		complete := instance.AssertComplete()

		payload := ChainWorkflowPayload{}
		complete.GetResult(&payload)

		require.True(t, payload.Finished())
		require.Len(t, payload.State, 1)

		require.True(t, payload.State[0].IsFinished)
		require.Equal(t, 1, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Empty(t, payload.State[0].Err)
		wpayload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Result, &wpayload))
		require.Equal(t, workflow1Payload, wpayload)
	})

	t.Run("continues chain if OnCancel continues", func(t *testing.T) {
		workflow1TaskParams := TestTaskParams{
			TaskValue: "workflow1Task",
		}
		workflow1Payload := TestWorkflowPayload{
			PayloadValue: "workflow1Payload",
		}
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		executor, err := NewChainWorkflowExecutor(
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					params := TestWorkflowParams{}
					require.NoError(t, w.GetParameters(&params))
					require.Equal(t, workflow1Params, params)
					w.EnqueueTask("task1Name", workflow1TaskParams)
					return w.Continue(workflow1Payload)
				},
			),
			cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				nil,
			))
		require.NoError(t, err)
		params, err := ToChainWorkflowParameters([]interface{}{workflow1Params, nil})
		require.NoError(t, err)
		currentPayload := ChainWorkflowPayload{
			State: []WorkflowState{
				{
					IsFinished:    false,
					EnqueuedTasks: 1,
				},
			},
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(currentPayload)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		continuing := instance.AssertContinuing()
		task := continuing.AssertTaskEnqueued("task1Name")
		task.AssertCount(1)
		tparams := TestTaskParams{}
		task.Tasks[0].GetParameters(&tparams)
		require.Equal(t, workflow1TaskParams, tparams)

		payload := ChainWorkflowPayload{}
		continuing.GetPayload(&payload)

		require.False(t, payload.Finished())
		require.Len(t, payload.State, 1)

		require.False(t, payload.State[0].IsFinished)
		require.Equal(t, 2, payload.State[0].EnqueuedTasks)
		require.Equal(t, 0, payload.State[0].CompletedTasks)
		require.Empty(t, payload.State[0].Err)
		wpayload := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(payload.State[0].Payload, &wpayload))
		require.Equal(t, workflow1Payload, wpayload)
	})
}
