package workflow

import (
	"context"
	"time"

	"github.com/pkg/errors"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/lib/workflow/backend"

	"github.com/sirupsen/logrus"
)

var (
	// maxWakeupInterval is the maximum amount of time we will
	// sleep between checking the recurrence table.
	maxWakeupInterval = 60 * time.Second
	// lateWarningThreshold is how late a job can be before we
	// will log a warning.
	lateWarningThreshold = 10 * time.Second
)

type workflowScheduler struct {
	backend backend.Driver
}

func (w *workflowScheduler) run(ctx context.Context) {
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
			logrus.Infof("Recurring workflow scheduler sleep for %fs", nextSleep.Seconds())
		}
	}
}

func (w *workflowScheduler) scheduleWorkflows(ctx context.Context) (time.Duration, error) {
	sleepTime := maxWakeupInterval

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
		// BUG(jaym): It's possible to get into a busy loop here.
		// If there is a workflow instance that is past due and
		// currently running, we spin because we don't return an
		// error
	}
}

func (w *workflowScheduler) scheduleWorkflow(ctx context.Context) (time.Duration, error) {
	s, completer, err := w.backend.GetDueRecurringWorkflow(ctx)
	if err != nil {
		if err == ErrNoDueWorkflows {
			s, err2 := w.backend.GetNextScheduledWorkflow(ctx)
			if err2 != nil {
				if err2 == ErrNoScheduledWorkflows {
					return maxWakeupInterval, err
				}
				logrus.WithError(err2).Error("failed to determine next scheduled workflow")
				return maxWakeupInterval, err
			}
			logrus.Debugf("Woke up %fs early for task", time.Until(s.NextDueAt).Seconds())
			return time.Until(s.NextDueAt), err
		}
		return maxWakeupInterval, errors.Wrap(err, "could not fetch recurring workflows")
	}
	defer completer.Close()

	if time.Since(s.NextDueAt) > lateWarningThreshold {
		logrus.Warnf("Recurring workflow %fs past due. (expected at %s)", time.Since(s.NextDueAt).Seconds(), s.NextDueAt)
	}

	workflowInstanceName := s.Name

	// TODO(ssd) 2019-05-13: We might need two different
	// rule types here to suppor the different use cases.
	recurrence, err := rrule.StrToRRule(s.Recurrence)
	if err != nil {
		// TODO(ssd) 2019-05-13: Perhaps we should disable this rule so that it doesn't keep producing errors
		// We need to do this otherwise we'll just keep trying to enqueue this and nothing else
		return maxWakeupInterval, errors.Wrap(err, "could not parse recurrence rule for workflow, skipping")
	}

	nowUTC := time.Now().UTC()
	nextDueAt := recurrence.After(nowUTC, true).UTC()
	if nextDueAt.IsZero() {
		// BUG(jaym): nextDueAt can be zero, for example when a recurrence
		// will never again be due (Until)
		panic("Unimplemented")
	}
	sleepTime := time.Until(nextDueAt)
	logrus.Infof("Starting scheduled workflow %q", workflowInstanceName)
	err = completer.EnqueueRecurringWorkflow(s, workflowInstanceName, nextDueAt, nowUTC)
	if err != nil {
		if err == ErrWorkflowInstanceExists {
			logrus.Warnf(
				"Recurring workflow %q still running, consider increasing recurrence interval",
				workflowInstanceName)
			// TODO(jaym): what do we want to do here? i think we're going to keep trying
			//             until we succeed here? Maybe we want to skip this interval?
			return maxWakeupInterval, nil
		}
		logrus.WithError(err).Error("could not update recurring workflow record")
		return sleepTime, err
	}

	return sleepTime, nil
}
