package authz

import (
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/cereal/cerealtest"
)

const (
	svcName         = "testSvc"
	projectUpdateID = "someid"
	startTaskName   = "testSvc/StartProjectTagUpdater"
	statusTaskName  = "testSvc/ProjectTagUpdaterStatus"
	cancelTaskName  = "testSvc/CancelUpdateProjectTags"
)

func TestWorkflowOnStart(t *testing.T) {
	workflowParams := DomainProjectUpdateWorkflowParameters{
		ProjectUpdateID: projectUpdateID,
	}
	executor := NewWorkflowExecutorForDomainService(svcName)

	t.Run("launches StartProjectTagUpdater for service", func(t *testing.T) {
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(workflowParams)

		ev := cerealtest.NewStartEvent()
		executor.OnStart(instance, ev)

		continuing := instance.AssertContinuing()
		continuing.AssertTaskEnqueued(startTaskName).AssertCount(1)
		payload := DomainProjectUpdateWorkflowPayload{}
		continuing.GetPayload(&payload)

		require.Equal(t,
			DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID: projectUpdateID,
			},
			payload)
	})
}

func TestWorkflowOnTaskComplete(t *testing.T) {
	workflowParams := DomainProjectUpdateWorkflowParameters{
		ProjectUpdateID: projectUpdateID,
	}
	executor := NewWorkflowExecutorForDomainService(svcName)

	t.Run("when StartProjectTagUpdater completes", func(t *testing.T) {
		curPayload := DomainProjectUpdateWorkflowPayload{
			ProjectUpdateID: projectUpdateID,
		}
		t.Run("when the task fails", func(t *testing.T) {
			failureErr := errors.New("fail")
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)

			tr := cerealtest.NewTaskResult(t).WithError(failureErr)
			ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
			executor.OnTaskComplete(instance, ev)
			instance.AssertFailed().WithErrorEqual(failureErr)
		})

		t.Run("when status checks not required", func(t *testing.T) {
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)

			tr := cerealtest.NewTaskResult(t).WithResult(StartResult{
				IsComplete: true,
			})
			ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
			executor.OnTaskComplete(instance, ev)
			complete := instance.AssertComplete()
			payload := DomainProjectUpdateWorkflowPayload{}
			complete.GetResult(&payload)
			require.False(t, payload.Canceled)
			require.Equal(t, projectUpdateID, payload.ProjectUpdateID)
			require.Equal(t, JobStatus{
				Completed:          true,
				PercentageComplete: 1.0,
			}, payload.MergedJobStatus)
		})

		t.Run("when status checks are required", func(t *testing.T) {
			t.Run("fails if no job ids provided", func(t *testing.T) {
				instance := cerealtest.
					NewWorkflowInstance(t, "instanceName").
					WithParameters(workflowParams).
					WithPayload(curPayload)

				tr := cerealtest.NewTaskResult(t).WithResult(StartResult{})
				ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
				executor.OnTaskComplete(instance, ev)
				instance.AssertFailed().WithErrorEqual(ErrNoJobIDs)
			})

			t.Run("launches task to check job ids", func(t *testing.T) {
				instance := cerealtest.
					NewWorkflowInstance(t, "instanceName").
					WithParameters(workflowParams).
					WithPayload(curPayload)

				jobIDs := []string{"idA", "idB"}
				tr := cerealtest.NewTaskResult(t).WithResult(StartResult{
					JobIDs: jobIDs,
				})
				ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
				executor.OnTaskComplete(instance, ev)
				continuing := instance.AssertContinuing()
				tasks := continuing.AssertTaskEnqueued(statusTaskName).AssertCount(1)
				require.NotZero(t, tasks.Tasks[0].Opts.StartAfter)
				statusParams := StatusParameters{}
				tasks.Tasks[0].GetParameters(&statusParams)
				require.Equal(t, jobIDs, statusParams.JobIDs)

				payload := DomainProjectUpdateWorkflowPayload{}
				continuing.GetPayload(&payload)
				require.Equal(t, jobIDs, payload.JobIDs)
			})
		})
	})

	t.Run("when ProjectTagUpdaterStatus completes", func(t *testing.T) {
		failureErr := errors.New("fail")
		jobIDs := []string{"idA", "idB"}

		t.Run("if an error is returned and cannot retry, fail", func(t *testing.T) {
			curPayload := DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID:             projectUpdateID,
				ConsecutiveJobCheckFailures: maxNumberOfConsecutiveFails,
				JobIDs:                      jobIDs,
			}
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)
			tr := cerealtest.NewTaskResult(t).WithError(failureErr)
			ev := cerealtest.NewTaskCompleteEvent(statusTaskName, tr)
			executor.OnTaskComplete(instance, ev)
			instance.AssertFailed().WithErrorEqual(failureErr)
		})

		t.Run("if an error is returned and can retry then continue", func(t *testing.T) {
			curPayload := DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID:             projectUpdateID,
				ConsecutiveJobCheckFailures: 1,
				MergedJobStatus: JobStatus{
					PercentageComplete: 0.5,
				},
				JobIDs: jobIDs,
			}
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)
			tr := cerealtest.NewTaskResult(t).WithError(failureErr)
			ev := cerealtest.NewTaskCompleteEvent(statusTaskName, tr)
			executor.OnTaskComplete(instance, ev)
			continuing := instance.AssertContinuing()
			// status task is enqueued again
			tasks := continuing.AssertTaskEnqueued(statusTaskName).AssertCount(1)
			require.NotZero(t, tasks.Tasks[0].Opts.StartAfter)

			payload := DomainProjectUpdateWorkflowPayload{}
			continuing.GetPayload(&payload)
			require.Equal(t, projectUpdateID, payload.ProjectUpdateID)
			require.Equal(t, 2, payload.ConsecutiveJobCheckFailures)
			require.Equal(t, float32(0.5), payload.MergedJobStatus.PercentageComplete)
			require.Equal(t, jobIDs, payload.JobIDs)
		})

		t.Run("if merged job status is complete, complete", func(t *testing.T) {
			curPayload := DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID:             projectUpdateID,
				ConsecutiveJobCheckFailures: 1,
				MergedJobStatus: JobStatus{
					PercentageComplete: 0.5,
				},
				JobIDs: jobIDs,
			}
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)
			tr := cerealtest.NewTaskResult(t).WithResult(JobStatus{
				Completed:          true,
				PercentageComplete: 1,
			})
			ev := cerealtest.NewTaskCompleteEvent(statusTaskName, tr)
			executor.OnTaskComplete(instance, ev)
			complete := instance.AssertComplete()

			payload := DomainProjectUpdateWorkflowPayload{}
			complete.GetResult(&payload)
			require.Equal(t, projectUpdateID, payload.ProjectUpdateID)
			require.Equal(t, 0, payload.ConsecutiveJobCheckFailures)
			require.True(t, payload.MergedJobStatus.Completed)
			require.Equal(t, float32(1), payload.MergedJobStatus.PercentageComplete)
			require.Equal(t, jobIDs, payload.JobIDs)
		})

		t.Run("if merged job status is not complete, run status again in the future", func(t *testing.T) {
			curPayload := DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID:             projectUpdateID,
				ConsecutiveJobCheckFailures: 1,
				MergedJobStatus: JobStatus{
					PercentageComplete: 0.5,
				},
				JobIDs: jobIDs,
			}
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)
			tr := cerealtest.NewTaskResult(t).WithResult(JobStatus{
				Completed:          false,
				PercentageComplete: 0.6,
			})
			ev := cerealtest.NewTaskCompleteEvent(statusTaskName, tr)
			executor.OnTaskComplete(instance, ev)
			continuing := instance.AssertContinuing()
			tasks := continuing.AssertTaskEnqueued(statusTaskName).AssertCount(1)
			statusParams := StatusParameters{}
			tasks.Tasks[0].GetParameters(&statusParams)
			require.Equal(t, jobIDs, statusParams.JobIDs)
			require.NotZero(t, tasks.Tasks[0].Opts.StartAfter)

			payload := DomainProjectUpdateWorkflowPayload{}
			continuing.GetPayload(&payload)
			require.Equal(t, projectUpdateID, payload.ProjectUpdateID)
			require.Equal(t, 0, payload.ConsecutiveJobCheckFailures)
			require.False(t, payload.MergedJobStatus.Completed)
			require.Equal(t, float32(0.6), payload.MergedJobStatus.PercentageComplete)
			require.Equal(t, jobIDs, payload.JobIDs)
		})
	})

	t.Run("when CancelUpdateProjectTags completes", func(t *testing.T) {
		failureErr := errors.New("fail")
		jobIDs := []string{"idA", "idB"}

		t.Run("fails if cancel task fails", func(t *testing.T) {
			curPayload := DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID:             projectUpdateID,
				ConsecutiveJobCheckFailures: 1,
				MergedJobStatus: JobStatus{
					PercentageComplete: 0.5,
				},
				JobIDs: jobIDs,
			}
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)

			tr := cerealtest.NewTaskResult(t).WithError(failureErr)
			ev := cerealtest.NewTaskCompleteEvent(cancelTaskName, tr)
			executor.OnTaskComplete(instance, ev)

			instance.AssertFailed().WithErrorEqual(failureErr)
		})

		t.Run("completes if cancel task succeeds", func(t *testing.T) {
			curPayload := DomainProjectUpdateWorkflowPayload{
				ProjectUpdateID:             projectUpdateID,
				ConsecutiveJobCheckFailures: 1,
				MergedJobStatus: JobStatus{
					PercentageComplete: 0.5,
				},
				JobIDs:   jobIDs,
				Canceled: true,
			}
			instance := cerealtest.
				NewWorkflowInstance(t, "instanceName").
				WithParameters(workflowParams).
				WithPayload(curPayload)

			tr := cerealtest.NewTaskResult(t)
			ev := cerealtest.NewTaskCompleteEvent(cancelTaskName, tr)
			executor.OnTaskComplete(instance, ev)

			complete := instance.AssertComplete()
			payload := DomainProjectUpdateWorkflowPayload{}
			complete.GetResult(&payload)
			require.True(t, payload.Canceled)
			require.Equal(t, projectUpdateID, payload.ProjectUpdateID)
			require.Equal(t, 1, payload.ConsecutiveJobCheckFailures)
			require.False(t, payload.MergedJobStatus.Completed)
			require.Equal(t, float32(0.5), payload.MergedJobStatus.PercentageComplete)
			require.Equal(t, jobIDs, payload.JobIDs)
		})
	})

}

func TestWorkflowOnCancel(t *testing.T) {
	workflowParams := DomainProjectUpdateWorkflowParameters{
		ProjectUpdateID: projectUpdateID,
	}
	executor := NewWorkflowExecutorForDomainService(svcName)
	jobIDs := []string{"idA", "idB"}

	t.Run("starts CancelUpdateProjectTags task", func(t *testing.T) {
		curPayload := DomainProjectUpdateWorkflowPayload{
			ProjectUpdateID:             projectUpdateID,
			ConsecutiveJobCheckFailures: 1,
			MergedJobStatus: JobStatus{
				PercentageComplete: 0.5,
			},
			JobIDs: jobIDs,
		}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(workflowParams).
			WithPayload(curPayload)
		ev := cerealtest.NewCancelEvent()
		executor.OnCancel(instance, ev)

		continuing := instance.AssertContinuing()
		continuing.AssertTaskEnqueued(cancelTaskName).AssertCount(1)

		payload := DomainProjectUpdateWorkflowPayload{}
		continuing.GetPayload(&payload)
		require.True(t, payload.Canceled)
		require.Equal(t, projectUpdateID, payload.ProjectUpdateID)
		require.Equal(t, 1, payload.ConsecutiveJobCheckFailures)
		require.False(t, payload.MergedJobStatus.Completed)
		require.Equal(t, float32(0.5), payload.MergedJobStatus.PercentageComplete)
		require.Equal(t, jobIDs, payload.JobIDs)
	})
}
