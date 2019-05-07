package workflow

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	rrule "github.com/teambition/rrule-go"

	"github.com/sirupsen/logrus"
)

// maxWakeupInterval is the maximum amount of time we will sleep
// between checking the recurrence table.
var maxWakeupInterval = 1 * time.Minute

type Schedule struct {
	// NOTE(ssd) 2019-05-13: Since name and workflow-name are
	// user-controlled in the case of many scheduled workflows, we
	// need the ID to create unique workflow names.
	ID           int64
	Enabled      bool
	Name         string
	WorkflowName string
	Parameters   []byte
	Recurrence   string
	NextDueAt    time.Time
}

type workflowScheduler struct {
	backend Backend
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
		}
	}
}

func (w *workflowScheduler) scheduleWorkflows(ctx context.Context) (time.Duration, error) {
	sleepTime := maxWakeupInterval

	for {
		s, err := w.scheduleWorkflow(ctx)
		if err == ErrNoDueWorkflows {
			return sleepTime, nil
		}
		if err != nil {
			return sleepTime, err
		}
		if sleepTime > s {
			sleepTime = s
		}

	}
}

func (w *workflowScheduler) scheduleWorkflow(ctx context.Context) (time.Duration, error) {
	s, completer, err := w.backend.GetDueRecurringWorkflow(ctx)
	if err != nil {
		if err == ErrNoDueWorkflows {
			return maxWakeupInterval, err
		}
		return maxWakeupInterval, errors.Wrap(err, "could not fetch recurring workflows")
	}
	defer completer.Close()

	workflowInstanceName := fmt.Sprintf("%s/%s/%d", s.WorkflowName, s.Name, s.ID)

	// TODO(ssd) 2019-05-13: We might need two different
	// rule types here to suppor the different use cases.
	recurrence, err := rrule.StrToRRule(s.Recurrence)
	if err != nil {
		// TODO(ssd) 2019-05-13: Perhaps we should disable this rule so that it doesn't keep producing errors
		// We need to do this otherwise we'll just keep trying to enqueue this and nothing else
		return maxWakeupInterval, errors.Wrap(err, "could not parse recurrence rule for workflow, skipping")
	}

	nowUTC := time.Now().UTC()
	// NOTE(ssd) 2019-05-13: compliance looks 5 seconds in
	// the past to make sure that a job with a count of 1
	// actually gets run. However, I'm currently thinking
	// that we can push those kind of jobs onto the
	// workflow-instances queue immediately.
	nextDueAt := recurrence.After(nowUTC, true).UTC()
	sleepTime := time.Until(nextDueAt)
	err = completer.EnqueueRecurringWorkflow(s, workflowInstanceName, nextDueAt, nowUTC)
	if err != nil {
		if err == ErrWorkflowInstanceExists {
			// TODO(jaym): what do we want to do here? i think we're going to keep trying
			//             until we succeed here? Maybe we want to skip this interval?
			return maxWakeupInterval, nil
		}
		logrus.WithError(err).Error("could not update recurring workflow record")
		return sleepTime, err
	}

	return sleepTime, nil
}
