// +build integration

package integration

import (
	"context"
	"sync"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

func (suite *CerealTestSuite) TestCancelWorkflow() {
	taskName := randName("cancel")
	workflowName := randName("cancel")
	instanceName := randName("instance")

	wgWorkflowStart := sync.WaitGroup{}
	wgWorkflowStart.Add(1)

	wgTaskStart := sync.WaitGroup{}
	wgTaskStart.Add(1)

	wgTaskDone := sync.WaitGroup{}
	wgTaskDone.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(ctx context.Context, _ cereal.Task) (interface{}, error) {
				wgTaskStart.Done()
				defer wgTaskDone.Done()
				<-ctx.Done()
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					err := w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					defer wgWorkflowStart.Done()
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					suite.Require().Fail("the task should not have completed")
					return w.Complete()
				},
				onCancel: func(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
					return w.Complete()
				},
			},
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgWorkflowStart.Wait()
	wgTaskStart.Wait()
	logrus.Debug("canceling workflow")
	err = m.CancelWorkflow(context.Background(), workflowName, instanceName)
	suite.Require().NoError(err, "Failed to cancel workflow")
	wgTaskDone.Wait()
	time.Sleep(2 * time.Second)
	err = m.Stop()
	suite.NoError(err)
}
