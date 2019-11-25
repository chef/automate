// +build integration

package integration

import (
	"context"
	"sync"

	"github.com/chef/automate/lib/cereal"
)

// TestCompleteSimpleWorkflow tests that a workflow the launches a
// single task completes
//
// Workflow:
// - OnStart -> Launch 'simple' task
// - OnTaskComplete -> Done
func (suite *CerealTestSuite) TestCompleteSimpleWorkflow() {
	taskName := cereal.NewTaskName(randName("simple"))
	workflowName := cereal.NewWorkflowName(randName("simple"))
	instanceName := randName("instance")

	// There will be once task that runs, along
	// with the TaskCompleted
	wgTask := sync.WaitGroup{}
	wgTask.Add(3)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(_ context.Context, t cereal.Task) (interface{}, error) {
				suite.Assert().False(t.GetMetadata().EnqueuedAt.IsZero())
				wgTask.Done()
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					suite.Assert().False(ev.EnqueuedAt.IsZero())
					err := w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					suite.Assert().Equal(1, w.TotalCompletedTasks())
					suite.Assert().Equal(1, w.TotalEnqueuedTasks())
					suite.Assert().False(ev.EnqueuedAt.IsZero())
					wgTask.Done()
					return w.Complete()
				},
			},
		),
		WithManagerOpts(
			cereal.WithOnWorkflowCompleteCallback(func(*cereal.WorkflowEvent) {
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
