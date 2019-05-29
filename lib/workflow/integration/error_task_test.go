// +build integration

package integration_test

import (
	"context"
	"sync"
	"time"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/workflow"
)

func (suite *WorkflowTestSuite) TestErroredTask() {
	taskName := randName("return_error")
	workflowName := randName("error_test")
	instanceName := randName("instance")

	wgTask := sync.WaitGroup{}
	wgTask.Add(1)

	testErr := errors.New("errored")

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, workflow.Task) (interface{}, error) {
				return nil, testErr
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision {
					err := w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w workflow.WorkflowInstance, ev workflow.TaskCompleteEvent) workflow.Decision {
					suite.Assert().Equal(1, w.TotalCompletedTasks())
					suite.Assert().Equal(1, w.TotalEnqueuedTasks())
					suite.Assert().Equal(testErr.Error(), ev.Result.Err().Error())
					wgTask.Done()
					return w.Complete()
				},
			},
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTask.Wait()
	time.Sleep(20 * time.Millisecond)
	err = m.Stop()
	suite.NoError(err)
}
