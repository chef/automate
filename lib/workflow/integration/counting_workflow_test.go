// +build integration

package integration_test

import (
	"context"
	"sync"
	"time"

	"github.com/chef/automate/lib/workflow"
)

// TestCountToNWorkflow launches 2 workflow instances that do
// counting tasks. One counts to 10 and the other to 100. This
// makes sure you can have multiple workflows with the same
// workflow name but different workflow instance names.
// Also tests the handling of workflow parameters
func (suite *WorkflowTestSuite) TestCountToNWorkflow() {
	taskName := randName("count1")
	workflowName := randName("countN")
	instance100Name := "instance100"
	var instance100Count int
	instance10Name := "instance10"
	var instance10Count int

	wgWorkflow := sync.WaitGroup{}
	wgWorkflow.Add(2)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(ctx context.Context, task workflow.Task) (interface{}, error) {
				var toAdd int
				suite.Require().NoError(task.GetParameters(&toAdd))
				return toAdd, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision {
					var countTo int
					suite.Require().NoError(w.GetParameters(&countTo))

					for i := 0; i < countTo; i++ {
						err := w.EnqueueTask(taskName, 1)
						suite.Require().NoError(err, "failed to enqueue task")
					}

					return w.Continue(0)
				},
				onTaskComplete: func(w workflow.WorkflowInstance, ev workflow.TaskCompleteEvent) workflow.Decision {
					var countTo int
					var currentCount int
					var taskResult int
					suite.Require().NoError(w.GetParameters(&countTo))
					suite.Require().NoError(w.GetPayload(&currentCount))
					suite.Require().NoError(ev.Result.Err())
					suite.Require().NoError(ev.Result.Get(&taskResult))
					currentCount = currentCount + taskResult
					if w.TotalCompletedTasks() == w.TotalEnqueuedTasks() {
						if w.InstanceName() == instance100Name {
							instance100Count = currentCount
						} else if w.InstanceName() == instance10Name {
							instance10Count = currentCount
						}
						wgWorkflow.Done()
						return w.Complete()
					}
					return w.Continue(currentCount)
				},
			},
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instance100Name, 100)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	err = m.EnqueueWorkflow(context.Background(), workflowName, instance10Name, 10)
	suite.Require().NoError(err, "Failed to enqueue workflow")

	wgWorkflow.Wait()
	suite.Equal(100, instance100Count)
	suite.Equal(10, instance10Count)
	// We need to sleep here to allow the running workflow to commit its completion.
	// Otherwise, Stop cancels the context, which prevents any transactions
	// from commiting.
	time.Sleep(20 * time.Millisecond)
	err = m.Stop()
	suite.NoError(err)
}
