package postgres

import (
	"context"
	"database/sql"
	"sync"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

const defaultPingDuration = 15 * time.Second

type taskPinger struct {
	taskID       int64
	db           *sql.DB
	wg           sync.WaitGroup
	sg           cereal.StartGuard
	stop         context.CancelFunc
	pingInterval time.Duration
}

func newTaskPinger(db *sql.DB, taskID int64, pingInterval time.Duration) *taskPinger {
	pinger := &taskPinger{
		taskID:       taskID,
		db:           db,
		pingInterval: pingInterval,
		sg:           cereal.NewStartGuard("Start(ctx, onTaskLost) called more than once on postgres.taskPinger!"),
	}
	return pinger
}

// Start starts a goroutine that checks the task in on some interval.
// This function is only allowed to be called once. If the task is lost,
// onTaskLost will be called. This can be used to cancel a context passed
// to the TaskExecutor so that it can exit early.
func (w *taskPinger) Start(ctx context.Context, onTaskLost func()) {
	// Make sure this function is only called once. A second call will
	// cause this to panic
	w.sg.Started()

	// The wg will be used to wait for the goroutine to exit
	w.wg.Add(1)

	// Create a cancelable context that we can stop from Stop
	ctx, cancel := context.WithCancel(ctx)
	w.stop = cancel
	go func() {
		defer w.stop()

		logctx := logrus.WithFields(logrus.Fields{
			"taskID": w.taskID,
		})
		logctx.Debug("Starting taskPinger")
	OUTER:
		for {

			select {
			case <-ctx.Done():
				break OUTER
			case <-time.After(w.pingInterval):
				shouldExit, err := w.ping(ctx)
				if err == nil && shouldExit {
					onTaskLost()
					break OUTER
				}
			}
		}
		w.wg.Done()
		logctx.Debug("Exiting taskPinger")
	}()
}

func (w *taskPinger) ping(ctx context.Context) (shouldExit bool, err error) {
	logctx := logrus.WithFields(logrus.Fields{
		"taskID": w.taskID,
	})
	logctx.Debug("Pinging task")
	_, err = w.db.ExecContext(ctx, "SELECT cereal_ping_task($1)", w.taskID)
	if err != nil {
		logctx.WithError(err).Error("Failed to ping task")
		if isErrTaskLost(err) {
			return true, nil
		}
		return false, err
	}

	return false, nil
}

func (w *taskPinger) Stop() {
	w.stop()
	w.wg.Wait()
}
