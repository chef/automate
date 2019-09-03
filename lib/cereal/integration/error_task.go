// +build integration

package integration

import (
	"context"
	"sync"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

func (suite *CerealTestSuite) TestErroredTask() {
	taskName := randName("return_error")
	workflowName := randName("error_test")
	instanceName := randName("instance")

	wgTask := sync.WaitGroup{}
	wgTask.Add(2)

	testErr := errors.New("errored")

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, cereal.Task) (interface{}, error) {
				return nil, testErr
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
					suite.Assert().Equal(testErr.Error(), ev.Result.Err().Error())
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
