package integration

import (
	"context"
	"sync"
	"time"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
)

func (suite *CerealTestSuite) TestPatternSingleTaskWorkflowSuccess() {
	taskName := randName("single_task")
	workflowName := randName("single_task_workflow")
	instanceName := randName("instance")
	type TaskResult struct {
		Value string
	}
	type TaskParameters struct {
		Value string
	}
	tp := TaskParameters{Value: "value"}
	tr := TaskResult{Value: "value"}

	wgTask := sync.WaitGroup{}
	wgTask.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(ctx context.Context, task cereal.Task) (interface{}, error) {
				defer wgTask.Done()
				params := TaskParameters{}
				if err := task.GetParameters(&params); err != nil {
					return nil, err
				}
				return TaskResult{Value: params.Value}, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			patterns.NewSingleTaskWorkflowExecutor(taskName, false),
		),
	)
	defer m.Stop() // nolint: errcheck
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, tp)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTask.Wait()
	time.Sleep(20 * time.Millisecond)

	instance, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.Require().NoError(err)
	params := TaskParameters{}
	result := TaskResult{}
	suite.Require().False(instance.IsRunning())
	err = instance.GetParameters(&params)
	suite.Require().NoError(err)
	err = instance.GetResult(&result)
	suite.Require().NoError(err)

	suite.Assert().Equal(tp, params)
	suite.Assert().Equal(tr, result)

	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestPatternSingleTaskWorkflowFail() {
	taskName := randName("single_task")
	workflowName := randName("single_task_workflow")
	instanceName := randName("instance")
	type TaskResult struct {
		Value string
	}
	type TaskParameters struct {
		Value string
	}
	tp := TaskParameters{Value: "value"}
	errMsg := "i have failed"

	wgTask := sync.WaitGroup{}
	wgTask.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(ctx context.Context, task cereal.Task) (interface{}, error) {
				defer wgTask.Done()
				return nil, errors.New(errMsg)
			}),
		WithWorkflowExecutor(
			workflowName,
			patterns.NewSingleTaskWorkflowExecutor(taskName, false),
		),
	)
	defer m.Stop() // nolint: errcheck
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, tp)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTask.Wait()
	time.Sleep(20 * time.Millisecond)

	instance, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.Require().NoError(err)

	result := TaskResult{}
	suite.Require().False(instance.IsRunning())
	suite.Require().Error(instance.Err())
	err = instance.GetResult(&result)
	suite.Require().Error(err)
	suite.Require().Equal(errMsg, err.Error())

	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestPatternSingleTaskWorkflowCancelWhenNotAllowed() {
	taskName := randName("single_task")
	workflowName := randName("single_task_workflow")
	instanceName := randName("instance")
	type TaskResult struct {
		Value string
	}
	type TaskParameters struct {
		Value string
	}
	tp := TaskParameters{Value: "value"}
	tr := TaskResult{Value: "value"}

	wgTaskStart := sync.WaitGroup{}
	wgTaskStart.Add(1)
	wgTaskEnd := sync.WaitGroup{}
	wgTaskEnd.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(ctx context.Context, task cereal.Task) (interface{}, error) {
				defer wgTaskEnd.Done()
				wgTaskStart.Done()
				params := TaskParameters{}
				if err := task.GetParameters(&params); err != nil {
					return nil, err
				}
				select {
				case <-time.After(15 * time.Second):
					return TaskResult{Value: params.Value}, nil
				case <-ctx.Done():
					return nil, ctx.Err()
				}
			}),
		WithWorkflowExecutor(
			workflowName,
			patterns.NewSingleTaskWorkflowExecutor(taskName, false),
		),
	)
	defer m.Stop() // nolint: errcheck
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, tp)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTaskStart.Wait()
	err = m.CancelWorkflow(context.Background(), workflowName, instanceName)
	suite.Require().NoError(err)
	wgTaskEnd.Wait()
	time.Sleep(20 * time.Millisecond)

	instance, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.Require().NoError(err)
	params := TaskParameters{}
	result := TaskResult{}
	suite.Require().False(instance.IsRunning())
	err = instance.GetParameters(&params)
	suite.Require().NoError(err)
	err = instance.GetResult(&result)
	suite.Require().NoError(err)

	suite.Assert().Equal(tp, params)
	suite.Assert().Equal(tr, result)

	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestPatternSingleTaskWorkflowCancelWhenAllowed() {
	taskName := randName("single_task")
	workflowName := randName("single_task_workflow")
	instanceName := randName("instance")
	type TaskResult struct {
		Value string
	}
	type TaskParameters struct {
		Value string
	}
	tp := TaskParameters{Value: "value"}

	wgTaskStart := sync.WaitGroup{}
	wgTaskStart.Add(1)
	wgTaskEnd := sync.WaitGroup{}
	wgTaskEnd.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(ctx context.Context, task cereal.Task) (interface{}, error) {
				defer wgTaskEnd.Done()
				wgTaskStart.Done()
				params := TaskParameters{}
				if err := task.GetParameters(&params); err != nil {
					return nil, err
				}
				<-ctx.Done()
				// return value is forever lost
				return nil, ctx.Err()
			}),
		WithWorkflowExecutor(
			workflowName,
			patterns.NewSingleTaskWorkflowExecutor(taskName, true),
		),
	)
	defer m.Stop() // nolint: errcheck
	err := m.EnqueueWorkflow(context.Background(), workflowName, instanceName, tp)
	suite.Require().NoError(err, "Failed to enqueue workflow")
	wgTaskStart.Wait()
	err = m.CancelWorkflow(context.Background(), workflowName, instanceName)
	suite.Require().NoError(err)
	wgTaskEnd.Wait()
	time.Sleep(20 * time.Millisecond)

	instance, err := m.GetWorkflowInstanceByName(context.Background(), instanceName, workflowName)
	suite.Require().NoError(err)
	result := TaskResult{}
	suite.Require().False(instance.IsRunning())
	suite.Require().Error(instance.Err())
	err = instance.GetResult(&result)
	suite.Require().Equal(context.Canceled.Error(), err.Error())

	err = m.Stop()
	suite.NoError(err)
}
