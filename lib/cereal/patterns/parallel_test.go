package patterns

import (
	"encoding/json"
	"errors"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/cerealtest"
)

func fromMap(m map[string]cereal.WorkflowExecutor) ParallelWorkflowExecutorFor {
	return func(subworkflow string) (cereal.WorkflowExecutor, bool) {
		v, ok := m[subworkflow]
		return v, ok
	}
}

func TestParallelWorkflowOnStart(t *testing.T) {
	t.Run("fails on unknown workflow", func(t *testing.T) {
		executor := NewParallelWorkflowExecutor(fromMap(nil))
		params, err := ToParallelWorkfowParameters([]string{"foo", "bar"}, map[string]interface{}{})
		require.NoError(t, err)

		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		instance.AssertFailed()
	})

	t.Run("all can continue", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}
		workflow1TaskParams := TestTaskParams{
			TaskValue: "taskworkflow1",
		}
		workflow2TaskParams := TestTaskParams{
			TaskValue: "taskworkflow2",
		}
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow1Params, params)
					w.EnqueueTask("testTask1", workflow1TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Continue(TestWorkflowPayload{
						PayloadValue: params.Value,
					})
				},
				nil,
				nil,
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					w.EnqueueTask("testTask2", workflow2TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Continue(TestWorkflowPayload{
						PayloadValue: params.Value,
					})
				},
				nil,
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{
				"workflow1": workflow1Params,
				"workflow2": workflow2Params,
			})
		require.NoError(t, err)

		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		continuing := instance.AssertContinuing()
		tasks := continuing.AssertTaskEnqueued("testTask1").AssertCount(1)
		task1Params := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&task1Params)
		require.Equal(t, workflow1TaskParams, task1Params)
		assertTaskHasKey(t, tasks.Tasks[0], "workflow1")

		tasks = continuing.AssertTaskEnqueued("testTask2").AssertCount(1)
		task2Params := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&task2Params)
		require.Equal(t, workflow2TaskParams, task2Params)
		assertTaskHasKey(t, tasks.Tasks[0], "workflow2")

		payload := ParallelWorkflowPayload{}
		continuing.GetPayload(&payload)
		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.False(t, stateWorkflow1.IsFinished)
		require.Equal(t, 1, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Empty(t, stateWorkflow1.Err)
		payload1 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow1.Payload, &payload1))
		require.Equal(t, workflow1Params.Value, payload1.PayloadValue)

		stateWorkflow2 := payload.State["workflow2"]
		require.False(t, stateWorkflow2.IsFinished)
		require.Equal(t, 1, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow2.CompletedTasks)
		require.Empty(t, stateWorkflow2.Err)
		payload2 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow2.Payload, &payload2))
		require.Equal(t, workflow2Params.Value, payload2.PayloadValue)
	})

	t.Run("one can continue, one can complete", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}

		workflow2TaskParams := TestTaskParams{
			TaskValue: "taskworkflow2",
		}
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow1Params, params)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 0, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(TestWorkflowPayload{
						PayloadValue: params.Value,
					}))
				},
				nil,
				nil,
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					w.EnqueueTask("testTask2", workflow2TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Continue(TestWorkflowPayload{
						PayloadValue: params.Value,
					})
				},
				nil,
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{
				"workflow1": workflow1Params,
				"workflow2": workflow2Params,
			})
		require.NoError(t, err)

		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		continuing := instance.AssertContinuing()

		tasks := continuing.AssertTaskEnqueued("testTask2").AssertCount(1)
		task2Params := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&task2Params)
		require.Equal(t, workflow2TaskParams, task2Params)
		assertTaskHasKey(t, tasks.Tasks[0], "workflow2")

		payload := ParallelWorkflowPayload{}
		continuing.GetPayload(&payload)
		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Empty(t, stateWorkflow1.Err)
		payload1 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow1.Result, &payload1))
		require.Equal(t, workflow1Params.Value, payload1.PayloadValue)

		stateWorkflow2 := payload.State["workflow2"]
		require.False(t, stateWorkflow2.IsFinished)
		require.Equal(t, 1, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow2.CompletedTasks)
		require.Empty(t, stateWorkflow2.Err)
		payload2 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow2.Payload, &payload2))
		require.Equal(t, workflow2Params.Value, payload2.PayloadValue)
	})

	t.Run("one can continue, one can fail", func(t *testing.T) {
		failureErr := errors.New("fail")
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}

		workflow2TaskParams := TestTaskParams{
			TaskValue: "taskworkflow2",
		}
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow1Params, params)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 0, w.TotalEnqueuedTasks())
					return w.Fail(failureErr)
				},
				nil,
				nil,
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					w.EnqueueTask("testTask2", workflow2TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Continue(TestWorkflowPayload{
						PayloadValue: params.Value,
					})
				},
				nil,
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{
				"workflow1": workflow1Params,
				"workflow2": workflow2Params,
			})
		require.NoError(t, err)

		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		continuing := instance.AssertContinuing()

		tasks := continuing.AssertTaskEnqueued("testTask2").AssertCount(1)
		task2Params := TestTaskParams{}
		tasks.Tasks[0].GetParameters(&task2Params)
		require.Equal(t, workflow2TaskParams, task2Params)
		assertTaskHasKey(t, tasks.Tasks[0], "workflow2")

		payload := ParallelWorkflowPayload{}
		continuing.GetPayload(&payload)
		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Equal(t, failureErr.Error(), stateWorkflow1.Err)

		stateWorkflow2 := payload.State["workflow2"]
		require.False(t, stateWorkflow2.IsFinished)
		require.Equal(t, 1, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow2.CompletedTasks)
		require.Empty(t, stateWorkflow2.Err)
		payload2 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow2.Payload, &payload2))
		require.Equal(t, workflow2Params.Value, payload2.PayloadValue)
	})

	t.Run("completes if both complete", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}

		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow1Params, params)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 0, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(TestWorkflowPayload{
						PayloadValue: params.Value,
					}))
				},
				nil,
				nil,
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 0, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(TestWorkflowPayload{
						PayloadValue: params.Value,
					}))
				},
				nil,
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{
				"workflow1": workflow1Params,
				"workflow2": workflow2Params,
			})
		require.NoError(t, err)

		require.NoError(t, err)
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ParallelWorkflowPayload{}
		complete.GetResult(&payload)
		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Empty(t, stateWorkflow1.Err)
		payload1 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow1.Result, &payload1))
		require.Equal(t, workflow1Params.Value, payload1.PayloadValue)

		stateWorkflow2 := payload.State["workflow2"]
		require.True(t, stateWorkflow2.IsFinished)
		require.Equal(t, 0, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow2.CompletedTasks)
		require.Empty(t, stateWorkflow2.Err)
		payload2 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow2.Result, &payload2))
		require.Equal(t, workflow2Params.Value, payload2.PayloadValue)
	})

	t.Run("completes if both fail", func(t *testing.T) {
		failureErr1 := errors.New("fail1")
		failureErr2 := errors.New("fail2")
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow1Params, params)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 0, w.TotalEnqueuedTasks())
					return w.Fail(failureErr1)
				},
				nil,
				nil,
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 0, w.TotalEnqueuedTasks())
					return w.Fail(failureErr2)
				},
				nil,
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{
				"workflow1": workflow1Params,
				"workflow2": workflow2Params,
			})
		require.NoError(t, err)

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()

		executor.OnStart(instance, ev)
		complete := instance.AssertComplete()

		payload := ParallelWorkflowPayload{}
		complete.GetResult(&payload)
		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Equal(t, failureErr1.Error(), stateWorkflow1.Err)

		stateWorkflow2 := payload.State["workflow2"]
		require.True(t, stateWorkflow2.IsFinished)
		require.Equal(t, 0, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow2.CompletedTasks)
		require.Equal(t, failureErr2.Error(), stateWorkflow2.Err)
	})
}

func TestParallelWorkflowOnTaskComplete(t *testing.T) {
	t.Run("fails on unknown workflow", func(t *testing.T) {
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
				nil,
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters([]string{"workflow1", "workflow2"}, map[string]interface{}{})
		require.NoError(t, err)

		payload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{},
				"workflow2": WorkflowState{},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(payload)

		taskMetadata := ParallelWorkflowTaskParam{
			XXX_ParallelWorkflowKey: "blah",
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("testTask1", tr)

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrTaskWorkflowInvalid)
	})

	t.Run("fails on missing workflow executor", func(t *testing.T) {

		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow2": cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{})
		require.NoError(t, err)

		payload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{},
				"workflow2": WorkflowState{},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(payload)

		taskMetadata := ParallelWorkflowTaskParam{
			XXX_ParallelWorkflowKey: "workflow1",
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("testTask1", tr)

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrTaskWorkflowInvalid)
	})

	t.Run("does not deliver to completed", func(t *testing.T) {

		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{})
		require.NoError(t, err)

		curPayload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{
					IsFinished:    true,
					EnqueuedTasks: 1,
				},
				"workflow2": WorkflowState{
					EnqueuedTasks: 1,
				},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(curPayload)

		taskMetadata := ParallelWorkflowTaskParam{
			XXX_ParallelWorkflowKey: "workflow1",
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("testTask1", tr)

		executor.OnTaskComplete(instance, ev)

		continuing := instance.AssertContinuing()

		payload := ParallelWorkflowPayload{}
		continuing.GetPayload(&payload)
		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 1, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 1, stateWorkflow1.CompletedTasks)
		require.Empty(t, stateWorkflow1.Err)

	})

	t.Run("completes when final completes", func(t *testing.T) {
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow2": cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Complete(cereal.WithResult(TestWorkflowPayload{
						PayloadValue: params.Value,
					}))
				},
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{
				"workflow2": workflow2Params,
			})
		require.NoError(t, err)

		curPayload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{
					IsFinished: true,
				},
				"workflow2": WorkflowState{
					EnqueuedTasks: 1,
				},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(curPayload)

		taskMetadata := ParallelWorkflowTaskParam{
			XXX_ParallelWorkflowKey: "workflow2",
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("testTask1", tr)

		executor.OnTaskComplete(instance, ev)

		complete := instance.AssertComplete()

		payload := ParallelWorkflowPayload{}
		complete.GetResult(&payload)
		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Empty(t, stateWorkflow1.Err)

		stateWorkflow2 := payload.State["workflow2"]
		require.True(t, stateWorkflow2.IsFinished)
		require.Equal(t, 1, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 1, stateWorkflow2.CompletedTasks)
		require.Empty(t, stateWorkflow2.Err)
		payload2 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow2.Result, &payload2))
		require.Equal(t, workflow2Params.Value, payload2.PayloadValue)
	})

	t.Run("completes when final fails", func(t *testing.T) {
		failureErr := errors.New("fail")
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow2": cerealtest.NewWorkflowExecutor(
				nil,
				func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {

					require.Equal(t, 1, w.TotalCompletedTasks())
					require.Equal(t, 1, w.TotalEnqueuedTasks())
					return w.Fail(failureErr)
				},
				nil,
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters(
			[]string{"workflow1", "workflow2"},
			map[string]interface{}{})
		require.NoError(t, err)

		curPayload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{
					IsFinished: true,
				},
				"workflow2": WorkflowState{
					EnqueuedTasks: 1,
				},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(curPayload)

		taskMetadata := ParallelWorkflowTaskParam{
			XXX_ParallelWorkflowKey: "workflow2",
		}
		tr := cerealtest.NewTaskResult(t).WithParameters(taskMetadata)
		ev := cerealtest.NewTaskCompleteEvent("testTask1", tr)

		executor.OnTaskComplete(instance, ev)

		complete := instance.AssertComplete()

		payload := ParallelWorkflowPayload{}
		complete.GetResult(&payload)
		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Empty(t, stateWorkflow1.Err)

		stateWorkflow2 := payload.State["workflow2"]
		require.True(t, stateWorkflow2.IsFinished)
		require.Equal(t, 1, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 1, stateWorkflow2.CompletedTasks)
		require.Equal(t, failureErr.Error(), stateWorkflow2.Err)
	})
}

func TestParallelWorkflowOnCancel(t *testing.T) {
	t.Run("fails on unknown workflow", func(t *testing.T) {
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					return w.Complete()
				},
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters([]string{"workflow1", "workflow2"}, map[string]interface{}{})
		require.NoError(t, err)

		payload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{},
				"workflow2": WorkflowState{},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(payload)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		instance.AssertFailed().WithErrorEqual(ErrTaskWorkflowInvalid)
	})

	t.Run("ignores completed workflows", func(t *testing.T) {
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow2": cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					return w.Complete()
				},
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters([]string{"workflow1", "workflow2"}, map[string]interface{}{})
		require.NoError(t, err)

		curPayload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{
					IsFinished: true,
					Err:        "failed",
				},
				"workflow2": WorkflowState{},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(curPayload)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		complete := instance.AssertComplete()

		payload := ParallelWorkflowPayload{}
		complete.GetResult(&payload)
		require.True(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.True(t, stateWorkflow1.IsFinished)
		require.Equal(t, 0, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		require.Equal(t, "failed", stateWorkflow1.Err)

		stateWorkflow2 := payload.State["workflow2"]
		require.True(t, stateWorkflow2.IsFinished)
	})

	t.Run("can continue", func(t *testing.T) {
		workflow1Params := TestWorkflowParams{
			Value: "workflow1Params",
		}
		workflow2Params := TestWorkflowParams{
			Value: "workflow2Params",
		}
		workflow1TaskParams := TestTaskParams{
			TaskValue: "taskworkflow1",
		}
		workflow2TaskParams := TestTaskParams{
			TaskValue: "taskworkflow2",
		}
		subworkflows := map[string]cereal.WorkflowExecutor{
			"workflow1": cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow1Params, params)
					w.EnqueueTask("testTask1", workflow1TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 2, w.TotalEnqueuedTasks())
					return w.Continue(TestWorkflowPayload{
						PayloadValue: params.Value,
					})
				},
			),
			"workflow2": cerealtest.NewWorkflowExecutor(
				nil,
				nil,
				func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					params := TestWorkflowParams{}
					err := w.GetParameters(&params)
					require.NoError(t, err)
					require.Equal(t, workflow2Params, params)
					w.EnqueueTask("testTask2", workflow2TaskParams)
					require.Equal(t, 0, w.TotalCompletedTasks())
					require.Equal(t, 2, w.TotalEnqueuedTasks())
					return w.Continue(TestWorkflowPayload{
						PayloadValue: params.Value,
					})
				},
			),
		}
		executor := NewParallelWorkflowExecutor(fromMap(subworkflows))
		params, err := ToParallelWorkfowParameters([]string{"workflow1", "workflow2"}, map[string]interface{}{
			"workflow1": workflow1Params,
			"workflow2": workflow2Params,
		})
		require.NoError(t, err)

		curPayload := ParallelWorkflowPayload{
			State: map[string]WorkflowState{
				"workflow1": WorkflowState{
					EnqueuedTasks: 1,
				},
				"workflow2": WorkflowState{
					EnqueuedTasks: 1,
				},
			},
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(params).
			WithPayload(curPayload)

		ev := cerealtest.NewCancelEvent()

		executor.OnCancel(instance, ev)
		continuing := instance.AssertContinuing()

		tasks1 := continuing.AssertTaskEnqueued("testTask1").AssertCount(1)
		task1Params := TestTaskParams{}
		tasks1.Tasks[0].GetParameters(&task1Params)
		require.Equal(t, workflow1TaskParams, task1Params)
		assertTaskHasKey(t, tasks1.Tasks[0], "workflow1")

		tasks2 := continuing.AssertTaskEnqueued("testTask2").AssertCount(1)
		task2Params := TestTaskParams{}
		tasks2.Tasks[0].GetParameters(&task2Params)
		require.Equal(t, workflow2TaskParams, task2Params)
		assertTaskHasKey(t, tasks2.Tasks[0], "workflow2")

		payload := ParallelWorkflowPayload{}
		continuing.GetPayload(&payload)
		require.False(t, payload.Finished())
		require.Len(t, payload.State, 2)
		require.Contains(t, payload.State, "workflow1")
		require.Contains(t, payload.State, "workflow2")

		stateWorkflow1 := payload.State["workflow1"]
		require.False(t, stateWorkflow1.IsFinished)
		require.Equal(t, 2, stateWorkflow1.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow1.CompletedTasks)
		payload1 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow1.Payload, &payload1))
		require.Equal(t, workflow1Params.Value, payload1.PayloadValue)

		stateWorkflow2 := payload.State["workflow2"]
		require.False(t, stateWorkflow2.IsFinished)
		require.Equal(t, 2, stateWorkflow2.EnqueuedTasks)
		require.Equal(t, 0, stateWorkflow2.CompletedTasks)
		payload2 := TestWorkflowPayload{}
		require.NoError(t, json.Unmarshal(stateWorkflow2.Payload, &payload2))
		require.Equal(t, workflow2Params.Value, payload2.PayloadValue)
	})
}

func assertTaskHasKey(t *testing.T, task *cerealtest.TestableTask, key string) {
	t.Helper()
	params := ParallelWorkflowTaskParam{}
	task.GetParameters(&params)
	require.Equal(t, key, params.XXX_ParallelWorkflowKey)
}
