// +build integration

package integration

import (
	"context"
	"sync"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

// TestWorkflowFail tests that a call to w.Fail ends the workflow
// with an error that can be recovered.
func (suite *CerealTestSuite) TestWorkflowFail() {
	workflowName := randName("failing")
	instanceName := randName("instance")

	wg := sync.WaitGroup{}
	wg.Add(2)
	m := suite.newManager(
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					wg.Done()
					return w.Fail(errors.New("expected test error"))
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
			},
		),
		WithManagerOpts(
			cereal.WithOnWorkflowCompleteCallback(func(*backend.WorkflowEvent) {
				wg.Done()
			}),
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wg.Wait()
	w, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.NoError(err)
	suite.Error(w.Err())
	suite.False(w.IsRunning())
	suite.Equal("expected test error", w.Err().Error())
	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestWorkflowFailOnBadEnqueue() {
	workflowName := randName("failing")
	instanceName := randName("instance")

	wg := sync.WaitGroup{}
	wg.Add(2)
	m := suite.newManager(
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					wg.Done()
					w.EnqueueTask("foo", nil)
					return w.Complete(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
			},
		),
		WithManagerOpts(
			cereal.WithOnWorkflowCompleteCallback(func(*backend.WorkflowEvent) {
				wg.Done()
			}),
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wg.Wait()
	w, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.NoError(err)
	suite.Error(w.Err())
	suite.False(w.IsRunning())
	suite.Equal("EnqueueTask and Complete called in same workflow step", w.Err().Error())
	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestWorkflowFailOnUnmarshalableJSON() {
	workflowName := randName("failing")
	instanceName := randName("instance")

	wg := sync.WaitGroup{}
	wg.Add(2)
	m := suite.newManager(
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					wg.Done()
					return w.Continue(make(chan struct{})) // channels can't be marshalled
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
			},
		),
		WithManagerOpts(
			cereal.WithOnWorkflowCompleteCallback(func(*backend.WorkflowEvent) {
				wg.Done()
			}),
		),
	)
	defer m.Stop()
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, nil)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	w, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.NoError(err)
	suite.Error(w.Err())
	suite.False(w.IsRunning())
	suite.Equal("json: unsupported type: chan struct {}", w.Err().Error())
	err = m.Stop()
	suite.NoError(err)
}
