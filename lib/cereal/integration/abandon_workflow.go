// +build integration

package integration

import (
	"context"
	"sync"
	"time"

	"github.com/chef/automate/lib/cereal"
	"github.com/sirupsen/logrus"
)

func (suite *CerealTestSuite) TestCompleteWorkflowWithPendingWorkflowEvents() {
	taskName := randName("success-pending-evts")
	workflowName := randName("success-pending-evts")
	instanceName := randName("instance")

	wgTask := sync.WaitGroup{}

	wgTask.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, cereal.Task) (interface{}, error) {
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					err := w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					err = w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					suite.Assert().Equal(1, w.TotalCompletedTasks())
					suite.Assert().Equal(2, w.TotalEnqueuedTasks())
					defer wgTask.Done()
					return w.Complete()
				},
			},
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTask.Wait()
	time.Sleep(2 * time.Second) // Sleep a little to make sure task complete doesn't get called multiple times
	logrus.Info("calling stop")
	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestCompleteWorkflowWithPendingTasks() {
	taskName := randName("success-pending-tasks")
	workflowName := randName("success-pending-tasks")
	instanceName := randName("instance")

	wgTask := sync.WaitGroup{}
	wgTask.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, cereal.Task) (interface{}, error) {
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					err := w.EnqueueTask(taskName, nil)
					suite.Require().NoError(err, "failed to enqueue task")
					err = w.EnqueueTask(taskName+"noexec", nil)
					suite.Require().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					suite.Assert().Equal(1, w.TotalCompletedTasks())
					suite.Assert().Equal(2, w.TotalEnqueuedTasks())
					defer wgTask.Done()
					return w.Complete()
				},
			},
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTask.Wait()
	time.Sleep(2 * time.Second) // Sleep a little to make sure task complete doesn't get called multiple times
	err = m.Stop()
	suite.NoError(err)
}
