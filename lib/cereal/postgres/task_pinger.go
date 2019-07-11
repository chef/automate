package postgres

import (
	"context"
	"database/sql"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
)

type taskPinger struct {
	taskID          int64
	db              *sql.DB
	wgStart         sync.WaitGroup
	wgStop          sync.WaitGroup
	stop            context.CancelFunc
	checkinInterval time.Duration
}

func newTaskPinger(db *sql.DB, taskID int64) *taskPinger {
	pinger := &taskPinger{
		taskID:          taskID,
		db:              db,
		checkinInterval: 30 * time.Second,
	}
	pinger.wgStart.Add(1)
	return pinger
}

// Start starts a goroutine that checks the task in on some interval.
// This function is only allowed to be called once.
func (w *taskPinger) Start(ctx context.Context) {
	// Make sure this function is only called once. A second call will
	// cause this to panic
	w.wgStart.Done()

	// The wgStop will be used to wait for the goroutine to exit
	w.wgStop.Add(1)

	// Create a cancelable context that we can stop from Stop
	ctx, cancel := context.WithCancel(ctx)
	w.stop = cancel
	go func() {
		defer w.stop()

	OUTER:
		for {
			select {
			case <-ctx.Done():
				break OUTER
			case <-time.After(w.checkinInterval):
				shouldExit, err := w.ping(ctx)
				if err == nil && shouldExit {
					break OUTER
				}
			}
		}
		w.wgStop.Done()
	}()
}

func (w *taskPinger) ping(ctx context.Context) (shouldExit bool, err error) {
	logctx := logrus.WithFields(logrus.Fields{
		"taskID": w.taskID,
	})
	logctx.Debug("checkin for task")
	_, err = w.db.ExecContext(ctx, "SELECT cereal_ping_task($1)", w.taskID)
	if err != nil {
		logctx.WithError(err).Error("failed to update task check-in time")
		if isErrTaskLost(err) {
			return true, nil
		}
		return false, err
	}

	return false, nil
}

func (w *taskPinger) Stop() {
	w.stop()
	w.wgStop.Wait()
}
