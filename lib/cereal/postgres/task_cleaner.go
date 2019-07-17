package postgres

import (
	"context"
	"database/sql"
	"math"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
)

const (
	defaultMaxWorkflowResults            = 10000
	defaultWorkflowResultsDeletionMargin = 1000
)

type taskCleaner struct {
	db      *sql.DB
	wgStart sync.WaitGroup
	wgStop  sync.WaitGroup
	stop    context.CancelFunc

	checkInterval time.Duration
	taskTimeout   time.Duration

	// The high-water mark for rows in the workflow results table.
	// We will start deleting rows beyond this point.
	maxWorkflowResults int
	// How many extra rows to delete when we've hit the high water
	// mark. This is to try to make sure we aren't just constantly
	// deleting rows.
	workflowResultsDeletionMargin int
}

func newTaskCleaner(db *sql.DB) *taskCleaner {
	cleaner := &taskCleaner{
		db:                            db,
		checkInterval:                 60 * time.Second,
		taskTimeout:                   300 * time.Second,
		maxWorkflowResults:            defaultMaxWorkflowResults,
		workflowResultsDeletionMargin: defaultWorkflowResultsDeletionMargin,
	}
	cleaner.wgStart.Add(1)
	return cleaner
}

func (w *taskCleaner) Start(ctx context.Context) {
	// Make sure this function is only called once. A second call will
	// cause this to panic
	w.wgStart.Done()

	// The wgStop will be used to wait for the goroutine to exit
	w.wgStop.Add(1)

	ctx, cancel := context.WithCancel(ctx)
	w.stop = cancel

	go func() {
		logrus.Debug("starting task cleaner")
	OUTER:
		for {
			select {
			case <-ctx.Done():
				break OUTER
			case <-time.After(w.checkInterval):
				logrus.Debug("checking for dead tasks")
				if err := w.expireDeadTasks(ctx, int64(math.Ceil(w.taskTimeout.Seconds()))); err != nil {
					logrus.WithError(err).Error("failed to run periodic dead-task cleaner")
				}

				logrus.Debug("cleaning workflow results table")
				if err := w.cleanResultsTable(ctx); err != nil {
					logrus.WithError(err).Error("failed to run periodic workflow-results cleaner")
				}
			}
		}
		w.wgStop.Done()
		logrus.Debug("exiting task cleaner")
	}()
}

func (w *taskCleaner) Stop() {
	w.stop()
	w.wgStop.Wait()
}

func (w *taskCleaner) expireDeadTasks(ctx context.Context, expireOlderThanSeconds int64) error {
	rows, err := w.db.QueryContext(ctx,
		"SELECT * FROM cereal_expire_tasks($1)", expireOlderThanSeconds)
	if err != nil {
		return err
	}

	defer func() {
		if err := rows.Close(); err != nil {
			logrus.WithError(err).Error("failed to close db rows")
		}
	}()

	for rows.Next() {
		var tid int64
		var workflowInstanceID int64
		err := rows.Scan(&tid, &workflowInstanceID)
		if err != nil {
			return err
		}
		logrus.WithFields(
			logrus.Fields{
				"tid":                tid,
				"workflowInstanceID": workflowInstanceID,
			}).Warnf("Expired task")
	}
	return rows.Err()
}

func (w *taskCleaner) cleanResultsTable(ctx context.Context) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := w.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}

	// NOTE(ssd) 2019-07-17: We are using a pg advsiory lock here,
	// because this might run concurrently and we want the
	// predictability of only running this deleting once.
	row := tx.QueryRowContext(ctx, "SELECT pg_try_advisory_xact_lock(23320, 4090)")
	var locked bool
	err = row.Scan(&locked)
	if err != nil {
		return err
	}

	if !locked {
		logrus.Debug("failed to acquired advisory lock for cleanup task, returning")
		return tx.Commit()
	}

	row = tx.QueryRowContext(ctx,
		"SELECT cereal_workflow_clean_workflow_results($1, $2)",
		w.maxWorkflowResults,
		w.workflowResultsDeletionMargin)

	var deletedRows int
	err = row.Scan(&deletedRows)
	if err != nil {
		return err
	}

	err = tx.Commit()
	if err != nil {
		return err
	}

	if deletedRows > 0 {
		logrus.Infof("Cleanup up %d rows from the cereal_workflow_results table", deletedRows)
	} else {
		logrus.Debug("No rows deleted from the cereal_workflow_results table")
	}

	return nil
}
