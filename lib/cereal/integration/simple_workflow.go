// +build integration

package integration

import (
	"context"
	"sync"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

// TestCompleteSimpleWorkflow tests that a workflow the launches a
// single task completes
//
// Workflow:
// - OnStart -> Launch 'simple' task
// - OnTaskComplete -> Done
func (suite *CerealTestSuite) TestCompleteSimpleWorkflow() {
	taskName := randName("simple")
	workflowName := randName("simple")
	instanceName := randName("instance")

	// There will be once task that runs, along
	// with the TaskCompleted
	wgTask := sync.WaitGroup{}
	wgTask.Add(3)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, cereal.Task) (interface{}, error) {
				wgTask.Done()
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					err := w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					suite.Assert().Equal(1, w.TotalCompletedTasks())
					suite.Assert().Equal(1, w.TotalEnqueuedTasks())
					wgTask.Done()
					return w.Complete()
				},
			},
		),
		WithManagerOpts(
			cereal.WithOnWorkflowCompleteCallback(func(*backend.WorkflowEvent) {
				wgTask.Done()
			}),
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTask.Wait()
	err = m.Stop()
	suite.NoError(err)
}
