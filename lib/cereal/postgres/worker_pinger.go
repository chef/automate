package postgres

import (
	"context"
	"database/sql"
	"sync"
	"time"

	"github.com/chef/automate/lib/uuid4"
	"github.com/sirupsen/logrus"
)

type workerPinger struct {
	workerID        uuid4.UUID
	db              *sql.DB
	wgStart         sync.WaitGroup
	wgStop          sync.WaitGroup
	stop            context.CancelFunc
	checkinInterval time.Duration
}

func newWorkerPinger(db *sql.DB, workerID uuid4.UUID) *workerPinger {
	pinger := &workerPinger{
		workerID:        workerID,
		db:              db,
		checkinInterval: 30 * time.Second,
	}
	pinger.wgStart.Add(1)
	return pinger
}

// Start starts a goroutine that checks the worker in on some interval.
// This function is only allowed to be called once.
func (w *workerPinger) Start(ctx context.Context) {
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

func (w *workerPinger) ping(ctx context.Context) (shouldExit bool, err error) {
	logctx := logrus.WithFields(logrus.Fields{
		"workerID": w.workerID,
	})
	logctx.Debug("checkin for worker")
	res, err := w.db.ExecContext(ctx, "UPDATE cereal_busy_task_workers SET last_checkin = NOW() WHERE id = $1", w.workerID.String())
	if err != nil {
		logctx.WithError(err).Error("failed to update worker check-in time")
		return false, err
	}

	rowsAffected, err := res.RowsAffected()
	if err != nil {
		logctx.WithError(err).Error("unable to determine if checkin successful")
		return false, err
	}

	if rowsAffected == 0 {
		logctx.Errorf("worker has been expired")
		return true, nil
	} else if rowsAffected > 1 {
		logctx.WithField("rowsAffected", rowsAffected).Errorf("checkin should have updated one row")
	}
	return false, nil
}

func (w *workerPinger) Stop() {
	w.stop()
	w.wgStop.Wait()
}
