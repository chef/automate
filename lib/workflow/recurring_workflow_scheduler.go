package workflow

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	rrule "github.com/teambition/rrule-go"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

// maxWakeupInterval is the maximum amount of time we will sleep
// between checking the recurrance table.
var maxWakeupInterval = 1 * time.Minute

const (
	getDueRecurringWorkflowsQuery = `
SELECT id, enabled, name, workflow_name, parameters, recurrence
FROM recurring_workflow_schedules
WHERE next_run_at < NOW() AND enabled = TRUE
FOR UPDATE SKIP LOCKED
`
	updateRecurringWorkflowQuery = `
UPDATE recurring_workflow_schedules SET next_run_at = $2, last_enqueued_at = $3 WHERE id = $1
`
)

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

type WorkflowScheduler struct {
	db *sql.DB
}

// This can probably live on the manager, it is separate for now to avoid conflicts
func NewWorkflowScheduler(connInfo string) (*WorkflowScheduler, error) {
	db, err := sql.Open("postgres", connInfo)
	if err != nil {
		return nil, err
	}

	return &WorkflowScheduler{
		db: db,
	}, nil
}

func (w *WorkflowScheduler) Start(ctx context.Context) {
	go w.run(ctx)
}

func (w *WorkflowScheduler) run(ctx context.Context) {
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

type workflowInstanceInput struct {
	name          string
	workflow_name string
	parameters    string
}

func (w *WorkflowScheduler) scheduleWorkflows(ctx context.Context) (time.Duration, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := w.db.BeginTx(ctx, nil)
	if err != nil {
		return maxWakeupInterval, errors.Wrap(err, "could not start transaction to query workflow schedules")
	}

	rows, err := tx.QueryContext(ctx, getDueRecurringWorkflowsQuery)
	if err != nil {
		return maxWakeupInterval, errors.Wrap(err, "could not query recurring workflows")
	}

	defer rows.Close()

	toEnqueue := make([]*Schedule, 0)
	for rows.Next() {
		var scheduledWorkflow Schedule
		err := rows.Scan(
			&scheduledWorkflow.ID,
			&scheduledWorkflow.Enabled,
			&scheduledWorkflow.Name,
			&scheduledWorkflow.WorkflowName,
			&scheduledWorkflow.Parameters,
			&scheduledWorkflow.Recurrence,
		)
		if err != nil {
			logrus.WithError(err).Error("could not scan workflow schedule from database, skipping")
			// TODO(ssd) 2019-05-13: Should we return here?
			continue
		}
		toEnqueue = append(toEnqueue, &scheduledWorkflow)
	}

	sleepDuration := maxWakeupInterval
	for _, s := range toEnqueue {
		// TODO(ssd) 2019-05-13: We might need two different
		// rule types here to suppor the different use cases.
		recurrence, err := rrule.StrToRRule(s.Recurrence)
		if err != nil {
			// TODO(ssd) 2019-05-13: Perhaps we should disable this rule so that it doesn't keep producing errors
			logrus.WithError(err).Error("could not parse recurrence rule for workflow, skipping")
			continue
		}

		workflowInstanceName := fmt.Sprintf("%s/%s/%d", s.WorkflowName, s.Name, s.ID)

		_, err = tx.ExecContext(ctx, enqueueWorkflowQuery, workflowInstanceName, s.WorkflowName,
			s.Parameters)
		if err != nil {
			logrus.WithError(err).Error("could not enqueue workflow instance for scheduled workflow")
			continue
		}

		nowUTC := time.Now().UTC()
		// NOTE(ssd) 2019-05-13: compliance looks 5 seconds in
		// the past to make sure that a job with a count of 1
		// actually gets run. However, I'm currently thinking
		// that we can push those kind of jobs onto the
		// workflow-instances queue immediately.
		nextDueAt := recurrence.After(nowUTC, true).UTC()
		_, err = tx.ExecContext(ctx, updateRecurringWorkflowQuery, s.ID, nextDueAt, nowUTC)
		if err != nil {
			logrus.WithError(err).Error("could not update recurring workflow record")
			continue
		}

		if time.Until(nextDueAt) < sleepDuration {
			sleepDuration = time.Until(nextDueAt)
		}

	}

	return sleepDuration, tx.Commit()
}
