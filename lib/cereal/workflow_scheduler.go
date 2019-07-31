package cereal

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/lib/cereal/backend"
)

var (
	// maxWakeupInterval is the maximum amount of time we will
	// sleep between checking the recurrence table.
	MaxWakeupInterval = 60 * time.Second
	// lateWarningThreshold is how late a job can be before we
	// will log a warning.
	LateWarningThreshold = 10 * time.Second
)

type WorkflowScheduler struct {
	backend backend.SchedulerDriver
}

func NewWorkflowScheduler(b backend.SchedulerDriver) *WorkflowScheduler {
	return &WorkflowScheduler{
		backend: b,
	}
}

func (w *WorkflowScheduler) Run(ctx context.Context) {
	var err error
	var nextSleep time.Duration
	for {
		select {
		case <-ctx.Done():
			logrus.Info("WorkflowScheduler shutting down")
			return
		case <-time.After(nextSleep):
			nextSleep, err = w.scheduleWorkflows(ctx)
			if err != nil {
				logrus.WithError(err).Error("failed to schedule workflows")
			}
			logrus.Debugf("Recurring workflow scheduler sleep for %fs", nextSleep.Seconds())
		}
	}
}

func (w *WorkflowScheduler) scheduleWorkflows(ctx context.Context) (time.Duration, error) {
	sleepTime := MaxWakeupInterval

	for {
		s, err := w.scheduleWorkflow(ctx)
		if sleepTime > s && s > 0 {
			sleepTime = s
		}
		if err == ErrNoDueWorkflows {
			return sleepTime, nil
		}
		if err != nil {
			return sleepTime, err
		}
	}
}

func (w *WorkflowScheduler) scheduleWorkflow(ctx context.Context) (time.Duration, error) {
	s, completer, err := w.backend.GetDueScheduledWorkflow(ctx)
	if err != nil {
		if err == ErrNoDueWorkflows {
			s, err2 := w.backend.GetNextScheduledWorkflow(ctx)
			if err2 != nil {
				if err2 == ErrNoScheduledWorkflows {
					return MaxWakeupInterval, err
				}
				logrus.WithError(err2).Error("failed to determine next scheduled workflow")
				return MaxWakeupInterval, err
			}
			logrus.Debugf("Woke up %fs early for task", time.Until(s.NextDueAt).Seconds())
			return time.Until(s.NextDueAt), err
		}
		return MaxWakeupInterval, errors.Wrap(err, "could not fetch recurring workflows")
	}
	defer completer.Close()

	if time.Since(s.NextDueAt) > LateWarningThreshold {
		logrus.Warnf("Recurring workflow %fs past due. (expected at %s)", time.Since(s.NextDueAt).Seconds(), s.NextDueAt)
	}

	recurrence, err := rrule.StrToRRule(s.Recurrence)
	if err != nil {
		logrus.WithError(err).Error("scheduled workflow has invalid recurrence rule, attempting to disable it")
		err2 := completer.DisableSchedule(s)
		if err2 != nil {
			return MaxWakeupInterval, errors.Wrap(err2, "failed to disable workflow with invalid recurrence rule")
		}

		// NOTE(ssd) 2019-07-15: We return no error here
		// because we've successfully disabled this rule, so
		// the polling loop can keep going.
		return MaxWakeupInterval, nil
	}

	nowUTC := time.Now().UTC()
	nextDueAt := recurrence.After(nowUTC, true).UTC()
	if nextDueAt.IsZero() {
		logrus.Infof("recurrence rule for scheduled workflow %q ends after this run", s.InstanceName)
		s.Enabled = false
	}

	sleepTime := time.Until(nextDueAt)
	s.NextDueAt = nextDueAt
	s.LastEnqueuedAt = nowUTC

	logrus.Infof("Starting scheduled workflow %q", s.InstanceName)
	err = completer.EnqueueAndUpdateScheduledWorkflow(s)
	if err != nil {
		if err == ErrWorkflowInstanceExists {
			logrus.Warnf("Scheduled workflow %q still running, consider increasing recurrence interval", s.InstanceName)
			// NOTE(ssd) 2019-07-15: If we get
			// ErrWorkflowInstanceExists, then we know
			// we've successfully pushed this job into the
			// future and it is safe to return no error
			// here.
			return sleepTime, nil
		}

		return sleepTime, errors.Wrap(err, "could not update scheduled workflow record")
	}

	return sleepTime, nil
}
