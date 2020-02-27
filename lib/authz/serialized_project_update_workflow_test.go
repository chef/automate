package authz

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/cerealtest"
	"github.com/stretchr/testify/require"
)

func TestSerializedWorkflowOnStart(t *testing.T) {
	t.Run("errors when no domain services are provided", func(t *testing.T) {
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()
		executor.OnStart(instance, ev)

		instance.AssertFailed()
	})

	t.Run("enqueues list tasks for each svc and continues", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params)

		ev := cerealtest.NewStartEvent()
		executor.OnStart(instance, ev)

		continuing := instance.AssertContinuing()

		continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-list-tasks/svc-a")).AssertCount(1)
		continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-list-tasks/svc-b")).AssertCount(1)

		state := SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)
		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseStarting, state.Phase)
	})
}

func TestSerializedWorkflowHandleListTasks(t *testing.T) {
	t.Run("waits for all list tasks before continuing to run phase", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		svcATasks := []SerializedProjectUpdateTask{
			{
				Priority: 1,
				Params:   map[string]string{"name": "foo"},
			},
			{
				Priority: 3,
				Params:   map[string]string{"name": "bar"},
			},
		}
		listTaskAResult := SerializedProjectUpdateListTasksTaskResult{
			Tasks: svcATasks,
		}
		trA := cerealtest.NewTaskResult(t).WithResult(listTaskAResult)
		evA := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-list-tasks/svc-a"),
			trA,
		)

		instanceA := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseStarting,
			})

		executor.OnTaskComplete(instanceA, evA)
		continuing := instanceA.AssertContinuing()
		continuing.AssertNoTasksEnqueued()

		state := SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)
		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseStarting, state.Phase)
		require.Equal(t, map[string]SerializedProjectUpdateListTasksTaskResult{
			"svc-a": SerializedProjectUpdateListTasksTaskResult{
				Tasks: svcATasks,
			},
		}, state.Starting.ListTasksResults)

		// Second List finishes:
		svcBTasks := []SerializedProjectUpdateTask{
			{
				Priority: 2,
				Params:   map[string]string{"name": "foo"},
			},
			{
				Priority: 4,
				Params:   map[string]string{"name": "bar"},
			},
			{
				Priority: 0,
				Params:   map[string]string{"name": "baz"},
			},
		}
		listTaskBResult := SerializedProjectUpdateListTasksTaskResult{
			Tasks: svcBTasks,
		}
		trB := cerealtest.NewTaskResult(t).WithResult(listTaskBResult)
		evB := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-list-tasks/svc-b"),
			trB,
		)

		instanceB := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(state)

		executor.OnTaskComplete(instanceB, evB)
		continuing = instanceB.AssertContinuing()

		state = SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)

		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseRunning, state.Phase)

		require.Equal(t, 0, state.Running.CompletedTasks)
		require.Equal(t, 5, state.Running.TotalTasks)

		require.Equal(t, map[string]string{"name": "bar"}, state.Running.RunningTask.Task.Params)
		require.Equal(t, []serializedProjectUpdateTask{
			{
				DomainService: "svc-a",
				SerializedProjectUpdateTask: SerializedProjectUpdateTask{
					Priority: 3,
					Params:   map[string]string{"name": "bar"},
				},
			},
			{
				DomainService: "svc-b",
				SerializedProjectUpdateTask: SerializedProjectUpdateTask{
					Priority: 2,
					Params:   map[string]string{"name": "foo"},
				},
			},
			{
				DomainService: "svc-a",
				SerializedProjectUpdateTask: SerializedProjectUpdateTask{
					Priority: 1,
					Params:   map[string]string{"name": "foo"},
				},
			},
			{
				DomainService: "svc-b",
				SerializedProjectUpdateTask: SerializedProjectUpdateTask{
					Priority: 0,
					Params:   map[string]string{"name": "baz"},
				},
			},
		}, state.Running.RemainingTasks)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-run-task/svc-b"))
		taskEnqueued.AssertCount(1)
		taskParams := SerializedProjectUpdateRunTaskTaskParams{}
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, map[string]string{"name": "bar"}, taskParams.Params)
	})

	t.Run("completes if list tasks returns 0 tasks to run", func(t *testing.T) {
		domainServices := []string{"svc-a"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		svcATasks := []SerializedProjectUpdateTask{}
		listTaskAResult := SerializedProjectUpdateListTasksTaskResult{
			Tasks: svcATasks,
		}
		trA := cerealtest.NewTaskResult(t).WithResult(listTaskAResult)
		evA := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-list-tasks/svc-a"),
			trA,
		)

		instanceA := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseStarting,
			})

		executor.OnTaskComplete(instanceA, evA)
		complete := instanceA.AssertComplete()
		state := SerializedProjectUpdateWorkflowState{}
		complete.GetResult(&state)
		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseComplete, state.Phase)
	})

	t.Run("fails if any list tasks return error", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		trA := cerealtest.NewTaskResult(t).WithError(errors.New("errrored"))
		evA := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-list-tasks/svc-a"),
			trA,
		)

		instanceA := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseStarting,
			})

		executor.OnTaskComplete(instanceA, evA)
		instanceA.AssertFailed()
	})
}

func TestSerializedWorkflowHandleRunTask(t *testing.T) {
	t.Run("starts next task when current one finishes", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		state := SerializedProjectUpdateWorkflowState{}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskSuccess,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 2,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{
						{
							DomainService: "svc-b",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 2,
								Params:   map[string]string{"name": "foo"},
							},
						},
					},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()

		state = SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)

		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseRunning, state.Phase)

		require.Equal(t, 1, state.Running.CompletedTasks)
		require.Equal(t, 2, state.Running.TotalTasks)

		require.Equal(t, map[string]string{"name": "foo"}, state.Running.RunningTask.Task.Params)
		require.Equal(t, []serializedProjectUpdateTask{}, state.Running.RemainingTasks)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-run-task/svc-b"))
		taskEnqueued.AssertCount(1)
		taskParams := SerializedProjectUpdateRunTaskTaskParams{}
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, map[string]string{"name": "foo"}, taskParams.Params)
	})

	t.Run("fails when current task finishes and in cancelled states", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskSuccess,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseCanceling,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 2,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{
						{
							DomainService: "svc-b",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 2,
								Params:   map[string]string{"name": "foo"},
							},
						},
					},
				},
			})

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed()
	})

	t.Run("completes if all tasks complete", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		state := SerializedProjectUpdateWorkflowState{}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskSuccess,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		state = SerializedProjectUpdateWorkflowState{}
		complete.GetResult(&state)

		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseComplete, state.Phase)

		require.Equal(t, 1, state.Running.CompletedTasks)
		require.Equal(t, 1, state.Running.TotalTasks)
	})

	t.Run("completes if all tasks complete and in cancelled state", func(t *testing.T) {
		domainServices := []string{"svc-a", "svc-b"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		state := SerializedProjectUpdateWorkflowState{}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskSuccess,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseCanceling,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		complete := instance.AssertComplete()

		state = SerializedProjectUpdateWorkflowState{}
		complete.GetResult(&state)

		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseComplete, state.Phase)

		require.Equal(t, 1, state.Running.CompletedTasks)
		require.Equal(t, 1, state.Running.TotalTasks)
	})

	t.Run("saves returned ID if task is not complete", func(t *testing.T) {
		domainServices := []string{"svc-a"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskRunning,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()

		state := SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)

		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseRunning, state.Phase)
		require.Equal(t, runID, state.Running.RunningTask.ID)

		require.Equal(t, 0, state.Running.CompletedTasks)
		require.Equal(t, 1, state.Running.TotalTasks)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-monitor-task/svc-a"))
		taskEnqueued.AssertCount(1)
		taskParams := SerializedProjectUpdateMonitorTaskTaskParams{}
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, runID, taskParams.ID)
		require.Equal(t, projectUpdateID, taskParams.ProjectUpdateID)
	})

	t.Run("cancels started task if in cancelling state", func(t *testing.T) {
		domainServices := []string{"svc-a"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskRunning,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseCanceling,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()

		state := SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)

		require.Equal(t, domainServices, state.DomainServices)
		require.Equal(t, projectUpdateID, state.ProjectUpdateID)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseCanceling, state.Phase)
		require.Equal(t, runID, state.Running.RunningTask.ID)

		require.Equal(t, 0, state.Running.CompletedTasks)
		require.Equal(t, 1, state.Running.TotalTasks)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-cancel-task/svc-a"))
		taskEnqueued.AssertCount(1)
		taskParams := SerializedProjectUpdateCancelTaskTaskParams{}
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, runID, taskParams.ID)
		require.Equal(t, projectUpdateID, taskParams.ProjectUpdateID)
	})

	t.Run("fails if task failed", func(t *testing.T) {
		domainServices := []string{"svc-a"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		runID := SerializedProjectUpdateTaskID("run-id-1")
		runTaskResult := SerializedProjectUpdateRunTaskTaskResult{
			ID: runID,
			Status: SerializedProjectUpdateTaskStatus{
				State: SerializedProjectUpdateTaskFailed,
				Error: "failed",
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(runTaskResult)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed()
	})

	t.Run("fails if error returned", func(t *testing.T) {
		domainServices := []string{"svc-a"}
		projectUpdateID := "project-update-id"

		executor := NewSerializedWorkflowExecutor()
		params := SerializedProjectUpdateWorkflowParams{
			ProjectUpdateID: projectUpdateID,
			DomainServices:  domainServices,
		}

		tr := cerealtest.NewTaskResult(t).WithError(errors.New("errored"))
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-run-task/svc-a"),
			tr,
		)

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed()
	})
}

func TestSerializedWorkflowHandleMonitorTask(t *testing.T) {
	domainServices := []string{"svc-a"}
	projectUpdateID := "project-update-id"

	executor := NewSerializedWorkflowExecutor()
	params := SerializedProjectUpdateWorkflowParams{
		ProjectUpdateID: projectUpdateID,
		DomainServices:  domainServices,
	}

	t.Run("when a monitor task errors, MonitorFailures increments", func(t *testing.T) {
		tr := cerealtest.NewTaskResult(t).WithError(errors.New("errored"))
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-monitor-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				MonitorFailures: 1,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.5,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()
		state := SerializedProjectUpdateWorkflowState{}

		continuing.GetPayload(&state)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseRunning, state.Phase)
		require.Equal(t, 2, state.MonitorFailures)
		require.Equal(t, float32(0.5), state.Running.RunningTask.LastStatus.PercentageComplete)
		require.Equal(t, SerializedProjectUpdateTaskRunning, state.Running.RunningTask.LastStatus.State)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-monitor-task/svc-a"))
		taskParams := SerializedProjectUpdateMonitorTaskTaskParams{}
		taskEnqueued.AssertCount(1)
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, runID, taskParams.ID)
		require.Equal(t, projectUpdateID, taskParams.ProjectUpdateID)
	})

	t.Run("when a monitor task errors too many times, workflow fails", func(t *testing.T) {
		tr := cerealtest.NewTaskResult(t).WithError(errors.New("errored"))
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-monitor-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				MonitorFailures: 10,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.5,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed()
	})

	t.Run("when a monitor task reports status", func(t *testing.T) {
		res := SerializedProjectUpdateMonitorTaskTaskResult{
			Status: SerializedProjectUpdateTaskStatus{
				State:              SerializedProjectUpdateTaskRunning,
				PercentageComplete: 0.5,
				Metadata: map[string]string{
					"some": "metadata",
				},
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(res)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-monitor-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				MonitorFailures: 1,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.25,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()
		state := SerializedProjectUpdateWorkflowState{}

		continuing.GetPayload(&state)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseRunning, state.Phase)
		require.Equal(t, 0, state.MonitorFailures)
		require.Equal(t, float32(0.5), state.Running.RunningTask.LastStatus.PercentageComplete)
		require.Equal(t, SerializedProjectUpdateTaskRunning, state.Running.RunningTask.LastStatus.State)
		require.Equal(t, map[string]string{
			"some": "metadata",
		}, state.Running.RunningTask.LastStatus.Metadata)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-monitor-task/svc-a"))
		taskParams := SerializedProjectUpdateMonitorTaskTaskParams{}
		taskEnqueued.AssertCount(1)
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, runID, taskParams.ID)
		require.Equal(t, projectUpdateID, taskParams.ProjectUpdateID)
	})

	t.Run("when a monitor task reports success and no tasks remain, workflow completes", func(t *testing.T) {
		res := SerializedProjectUpdateMonitorTaskTaskResult{
			Status: SerializedProjectUpdateTaskStatus{
				State:              SerializedProjectUpdateTaskSuccess,
				PercentageComplete: 1,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(res)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-monitor-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				MonitorFailures: 1,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.25,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertComplete()
		state := SerializedProjectUpdateWorkflowState{}

		continuing.GetResult(&state)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseComplete, state.Phase)
		require.Equal(t, 1, state.Running.CompletedTasks)
	})

	t.Run("when a monitor task reports success and tasks remain, next task is started", func(t *testing.T) {
		res := SerializedProjectUpdateMonitorTaskTaskResult{
			Status: SerializedProjectUpdateTaskStatus{
				State:              SerializedProjectUpdateTaskSuccess,
				PercentageComplete: 1,
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(res)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-monitor-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				MonitorFailures: 1,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					CompletedTasks: 1,
					TotalTasks:     3,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.25,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{
						{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 3,
								Params:   map[string]string{"name": "foo"},
							},
						},
					},
				},
			})

		executor.OnTaskComplete(instance, ev)
		continuing := instance.AssertContinuing()
		state := SerializedProjectUpdateWorkflowState{}

		continuing.GetPayload(&state)
		require.Equal(t, SerializedProjectUpdateWorkflowPhaseRunning, state.Phase)
		require.Equal(t, SerializedProjectUpdateTaskIDUnknown, state.Running.RunningTask.ID)
		require.Equal(t, map[string]string{"name": "foo"}, state.Running.RunningTask.Task.Params)
		require.Equal(t, float32(0), state.Running.RunningTask.LastStatus.PercentageComplete)
		require.Equal(t, 2, state.Running.CompletedTasks)
		require.Equal(t, 3, state.Running.TotalTasks)
		require.Empty(t, state.Running.RemainingTasks)

		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-run-task/svc-a"))
		taskEnqueued.AssertCount(1)
		taskParams := SerializedProjectUpdateRunTaskTaskParams{}
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, map[string]string{"name": "foo"}, taskParams.Params)
		require.Equal(t, projectUpdateID, taskParams.ProjectUpdateID)
	})

	t.Run("when a monitor task reports failure, workflow fails", func(t *testing.T) {
		res := SerializedProjectUpdateMonitorTaskTaskResult{
			Status: SerializedProjectUpdateTaskStatus{
				State:              SerializedProjectUpdateTaskFailed,
				PercentageComplete: 1,
				Error:              "task errored",
			},
		}
		tr := cerealtest.NewTaskResult(t).WithResult(res)
		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-monitor-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				MonitorFailures: 1,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.25,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed()
	})
}

func TestSerializedWorkflowOnCancel(t *testing.T) {
	domainServices := []string{"svc-a"}
	projectUpdateID := "project-update-id"

	executor := NewSerializedWorkflowExecutor()
	params := SerializedProjectUpdateWorkflowParams{
		ProjectUpdateID: projectUpdateID,
		DomainServices:  domainServices,
	}

	t.Run("immediately finishes workflow if in listing phase", func(t *testing.T) {

		ev := cerealtest.NewCancelEvent()

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseStarting,
			})

		executor.OnCancel(instance, ev)
		instance.AssertFailed()
	})

	t.Run("cancels running task if we have a task id", func(t *testing.T) {
		ev := cerealtest.NewCancelEvent()

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.25,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnCancel(instance, ev)
		continuing := instance.AssertContinuing()
		state := SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)

		require.Equal(t, SerializedProjectUpdateWorkflowPhaseCanceling, state.Phase)
		taskEnqueued := continuing.AssertTaskEnqueued(cereal.NewTaskName("project-update-cancel-task/svc-a"))
		taskParams := SerializedProjectUpdateCancelTaskTaskParams{}
		taskEnqueued.AssertCount(1)
		taskEnqueued.Tasks[0].GetParameters(&taskParams)
		require.Equal(t, runID, taskParams.ID)
		require.Equal(t, projectUpdateID, taskParams.ProjectUpdateID)
	})

	t.Run("continues in cancelling state if no task id is available", func(t *testing.T) {
		ev := cerealtest.NewCancelEvent()

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseRunning,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.25,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnCancel(instance, ev)
		continuing := instance.AssertContinuing()
		state := SerializedProjectUpdateWorkflowState{}
		continuing.GetPayload(&state)

		require.Equal(t, SerializedProjectUpdateWorkflowPhaseCanceling, state.Phase)
		continuing.AssertNoTasksEnqueued()
	})
}

func TestSerializedWorkflowHandleCancel(t *testing.T) {
	domainServices := []string{"svc-a"}
	projectUpdateID := "project-update-id"

	executor := NewSerializedWorkflowExecutor()
	params := SerializedProjectUpdateWorkflowParams{
		ProjectUpdateID: projectUpdateID,
		DomainServices:  domainServices,
	}

	t.Run("completes when cancel task finishes", func(t *testing.T) {
		res := SerializedProjectUpdateCancelTaskTaskResult{}
		tr := cerealtest.NewTaskResult(t).WithResult(res)

		ev := cerealtest.NewTaskCompleteEvent(
			cereal.NewTaskName("project-update-cancel-task/svc-a"),
			tr,
		)

		runID := SerializedProjectUpdateTaskID("run-id-1")

		instance := cerealtest.
			NewWorkflowInstance(t, "serialized").
			WithParameters(params).
			WithPayload(SerializedProjectUpdateWorkflowState{
				DomainServices:  domainServices,
				ProjectUpdateID: projectUpdateID,
				Phase:           SerializedProjectUpdateWorkflowPhaseCanceling,
				MonitorFailures: 1,
				Running: serializedProjectUpdateWorkflowPhaseRunningData{
					TotalTasks: 1,
					RunningTask: serializedProjectUpdateRunningTask{
						ID: runID,
						Task: serializedProjectUpdateTask{
							DomainService: "svc-a",
							SerializedProjectUpdateTask: SerializedProjectUpdateTask{
								Priority: 4,
								Params:   map[string]string{"name": "bar"},
							},
						},
						LastStatus: SerializedProjectUpdateTaskStatus{
							State:              SerializedProjectUpdateTaskRunning,
							PercentageComplete: 0.5,
						},
					},
					RemainingTasks: []serializedProjectUpdateTask{},
				},
			})

		executor.OnTaskComplete(instance, ev)
		instance.AssertFailed().WithErrorEqual(errCanceled)
	})
}
