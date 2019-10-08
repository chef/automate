// +build integration

package integration

import (
	"context"
	"sync"
	"time"

	"github.com/teambition/rrule-go"

	"github.com/chef/automate/lib/cereal"
)

func (suite *CerealTestSuite) TestSimpleScheduleWorkflow() {
	taskName := cereal.NewTaskName(randName("simple_schedule"))
	workflowName := cereal.NewWorkflowName(randName("simple_schedule"))
	instanceName := randName("instance")

	wgWorkflow := sync.WaitGroup{}
	wgWorkflow.Add(4)

	dtStart := time.Now().Add(2 * time.Second)
	count := 0

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
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					wgWorkflow.Done()
					return w.Complete()
				},
			},
		),
		WithNoStart(),
		WithManagerOpts(
			cereal.WithOnWorkflowCompleteCallback(func(*cereal.WorkflowEvent) {
				wgWorkflow.Done()
			}),
		),
	)
	defer m.Stop() // nolint: errcheck
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 10,
		Dtstart:  dtStart,
	})
	suite.Require().NoError(err)

	err = m.CreateWorkflowSchedule(
		context.Background(),
		instanceName,
		workflowName,
		"testparams",
		true,
		recurrence)

	suite.Require().NoError(err, "Failed to enqueue workflow")

	found := false
	schedules, err := m.ListWorkflowSchedules(context.Background())
	suite.Require().NoError(err)
	for _, s := range schedules {
		if s.WorkflowName == workflowName.String() && s.InstanceName == instanceName {
			found = true
		}
	}
	suite.Require().True(found, "schedule was not found after creation")

	err = m.Start(context.Background())
	suite.Require().NoError(err)
	wgWorkflow.Wait()
	err = m.Stop()
	suite.NoError(err)
}

func (suite *CerealTestSuite) TestScheduleUpdates() {
	workflowName := cereal.NewWorkflowName(randName("test_schedule"))
	instanceName := randName("test_instance")

	m := suite.newManager(
		WithWorkflowExecutor(
			workflowName,
			&workflowExecutorWrapper{
				onStart: func(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					return w.Complete()
				},
			},
		),
		WithNoStart(),
	)
	defer m.Stop() // nolint: errcheck

	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 10,
	})
	suite.Require().NoError(err)

	err = m.CreateWorkflowSchedule(
		context.Background(),
		instanceName,
		workflowName,
		"testparams",
		false,
		recurrence)

	suite.Require().NoError(err, "Failed to create workflow schedule")

	_, err = m.GetWorkflowScheduleByName(context.Background(), instanceName, workflowName)
	suite.Require().NoError(err, "workflow schedule was not found after creation")

	updateRecurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 20,
	})
	suite.Require().NoError(err)

	err = m.UpdateWorkflowScheduleByName(context.Background(), instanceName, workflowName, cereal.UpdateRecurrence(updateRecurrence))
	suite.Require().NoError(err, "Failed to update scheduled workflow")

	schedule, err := m.GetWorkflowScheduleByName(context.Background(), instanceName, workflowName)
	suite.Require().NoError(err, "Failed to retrieve updated scheduled workflow")
	newRecurrence, err := schedule.GetRRule()
	suite.Require().NoError(err, "Failed to parse rrule in scheduled workflow")
	suite.Assert().Equal(20, newRecurrence.OrigOptions.Interval)
}

func (suite *CerealTestSuite) TestCreateExpiredSchedule() {
	workflowName := cereal.NewWorkflowName(randName("expired_schedule"))
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
		context.Background(),
		instanceName,
		workflowName,
		"testparams",
		true,
		recurrence)

	suite.Assert().Equal(cereal.ErrInvalidSchedule, err)

}

func (suite *CerealTestSuite) TestExpiringSchedule() {
	taskName := cereal.NewTaskName(randName("expiring_schedule"))
	workflowName := cereal.NewWorkflowName(randName("expiring_schedule"))
	instanceName := randName("instance")

	wgWorkflow := sync.WaitGroup{}
	wgWorkflow.Add(1)

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
					suite.Assert().NoError(err, "failed to enqueue task")
					return w.Continue(nil)
				},
				onTaskComplete: func(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
					wgWorkflow.Done()
					return w.Complete()
				},
			},
		),
		WithNoStart(),
	)
	defer m.Stop() // nolint: errcheck

	// This recurrence is expected to run one time starting now. It's reoccurs
	// every 10 seconds, but that's not possible because the rule is only good
	// for 5 seconds.
	dtStart := time.Now().Add(2 * time.Second)
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: 10,
		Dtstart:  dtStart,
		Until:    dtStart.Add(5 * time.Second),
	})
	suite.Require().NoError(err)

	err = m.CreateWorkflowSchedule(
		context.Background(),
		instanceName,
		workflowName,
		"testparams",
		true,
		recurrence)

	suite.Require().NoError(err, "Failed to enqueue workflow")

	found := false
	schedules, err := m.ListWorkflowSchedules(context.Background())
	suite.Require().NoError(err)
	for _, s := range schedules {
		if s.WorkflowName == workflowName.String() && s.InstanceName == instanceName {
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
	suite.Require().NoError(err)
	for _, s := range schedules {
		if s.WorkflowName == workflowName.String() && s.InstanceName == instanceName {
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
