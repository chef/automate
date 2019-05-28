package integration_test

import (
	"context"
	"sync"
	"time"

	"github.com/teambition/rrule-go"

	"github.com/chef/automate/lib/workflow"
)

func (suite *WorkflowTestSuite) TestSimpleScheduleWorkflow() {
	taskName := randName("simple_schedule")
	workflowName := randName("simple_schedule")
	instanceName := randName("instance")

	wgWorkflow := sync.WaitGroup{}
	wgWorkflow.Add(3)

	dtStart := time.Now().Add(2 * time.Second)
	count := 0

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, workflow.Task) (interface{}, error) {
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision {
					suite.Assert().WithinDuration(
						dtStart.Add(time.Duration(10*count)*time.Second),
						time.Now(), 2*time.Second)
					count++
					var params string
					suite.Assert().NoError(w.GetParameters(&params))
					suite.Assert().Equal("testparams", params)
					err := w.EnqueueTask(taskName, nil)
					suite.Assert().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w workflow.WorkflowInstance, ev workflow.TaskCompleteEvent) workflow.Decision {
					wgWorkflow.Done()
					return w.Complete()
				},
			},
		),
		WithNoStart(),
	)
	defer m.Stop()
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 10,
		Dtstart:  dtStart,
	})
	suite.Require().NoError(err)

	err = m.CreateWorkflowSchedule(
		instanceName,
		workflowName,
		"testparams",
		true,
		recurrence)

	suite.Require().NoError(err, "Failed to enqueue workflow")

	found := false
	schedules, err := m.ListWorkflowSchedules(context.Background())
	for _, s := range schedules {
		if s.WorkflowName == workflowName && s.Name == instanceName {
			found = true
		}
	}
	suite.Require().True(found, "schedule was not found after creation")

	err = m.Start(context.Background())
	suite.Require().NoError(err)
	wgWorkflow.Wait()

	time.Sleep(20 * time.Millisecond)
	err = m.Stop()
	suite.NoError(err)
}

func (suite *WorkflowTestSuite) TestCreateExpiredSchedule() {
	workflowName := randName("expired_schedule")
	instanceName := randName("instance")

	m := suite.newManager(
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{},
		),
		WithNoStart(),
	)

	// This recurrence is expired. It can never trigger
	dtStart := time.Now().Add(-1 * time.Hour)
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 10,
		Dtstart:  dtStart,
		Until:    dtStart.Add(5 * time.Second),
	})
	suite.Require().NoError(err)

	err = m.CreateWorkflowSchedule(
		instanceName,
		workflowName,
		"testparams",
		true,
		recurrence)

	suite.Assert().Equal(workflow.ErrInvalidSchedule, err)

}

func (suite *WorkflowTestSuite) TestExpiringSchedule() {
	taskName := randName("expiring_schedule")
	workflowName := randName("expiring_schedule")
	instanceName := randName("instance")

	wgWorkflow := sync.WaitGroup{}
	wgWorkflow.Add(1)

	m := suite.newManager(
		WithTaskExecutorF(
			taskName,
			func(context.Context, workflow.Task) (interface{}, error) {
				return nil, nil
			}),
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w workflow.WorkflowInstance, ev workflow.StartEvent) workflow.Decision {
					err := w.EnqueueTask(taskName, nil)
					suite.Assert().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w workflow.WorkflowInstance, ev workflow.TaskCompleteEvent) workflow.Decision {
					wgWorkflow.Done()
					return w.Complete()
				},
			},
		),
		WithNoStart(),
	)
	defer m.Stop()

	// This recurrence is expected to run one time starting now. It's reoccurs
	// every 10 seconds, but that's not possible because the rule is only good
	// for 5 seconds.
	dtStart := time.Now()
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 10,
		Dtstart:  dtStart,
		Until:    dtStart.Add(5 * time.Second),
	})
	suite.Require().NoError(err)

	err = m.CreateWorkflowSchedule(
		instanceName,
		workflowName,
		"testparams",
		true,
		recurrence)

	suite.Require().NoError(err, "Failed to enqueue workflow")

	found := false
	schedules, err := m.ListWorkflowSchedules(context.Background())
	for _, s := range schedules {
		if s.WorkflowName == workflowName && s.Name == instanceName {
			suite.Assert().True(s.Enabled)
			found = true
		}
	}
	suite.Require().True(found, "schedule was not found after creation")

	err = m.Start(context.Background())
	suite.Require().NoError(err)
	wgWorkflow.Wait()

	found = false
	schedules, err = m.ListWorkflowSchedules(context.Background())
	for _, s := range schedules {
		if s.WorkflowName == workflowName && s.Name == instanceName {
			suite.Assert().False(s.Enabled,
				"expected scheduled workflow to be disabled because it expired")
			found = true
		}
	}
	suite.Require().True(found, "schedule was not found after creation")

	// We sleep 20 seconds to make sure that the workflow does not again trigger
	// It will panic if it does because the waitgroup will go negative
	time.Sleep(20 * time.Second)
	err = m.Stop()
	suite.NoError(err)
}
