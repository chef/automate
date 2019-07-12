package postgres

import (
	"context"
	"database/sql"
	"math"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
)

type taskCleaner struct {
	db      *sql.DB
	wgStart sync.WaitGroup
	wgStop  sync.WaitGroup
	stop    context.CancelFunc

	checkInterval time.Duration
	taskTimeout   time.Duration
}

func newTaskCleaner(db *sql.DB) *taskCleaner {
	cleaner := &taskCleaner{
		db:            db,
		checkInterval: 60 * time.Second,
		taskTimeout:   300 * time.Second,
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
					logrus.WithError(err).Error("failed to run periodic task cleaner")
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
