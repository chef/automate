package postgres

import (
	"context"
	"database/sql"
	"math"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
)

type workerCleaner struct {
	db      *sql.DB
	wgStart sync.WaitGroup
	wgStop  sync.WaitGroup
	stop    context.CancelFunc

	checkInterval time.Duration
	workerTimeout time.Duration
}

func newWorkerCleaner(db *sql.DB) *workerCleaner {
	cleaner := &workerCleaner{
		db:            db,
		checkInterval: 60 * time.Second,
		workerTimeout: 300 * time.Second,
	}
	cleaner.wgStart.Add(1)
	return cleaner
}

func (w *workerCleaner) Start(ctx context.Context) {
	// Make sure this function is only called once. A second call will
	// cause this to panic
	w.wgStart.Done()

	// The wgStop will be used to wait for the goroutine to exit
	w.wgStop.Add(1)

	ctx, cancel := context.WithCancel(ctx)
	w.stop = cancel

	go func() {
		logrus.Debug("starting worker cleaner")
	OUTER:
		for {
			select {
			case <-ctx.Done():
				break OUTER
			case <-time.After(w.checkInterval):
				logrus.Debug("checking for dead task workers")
				if err := w.expireDeadWorkers(ctx); err != nil {
					logrus.WithError(err).Error("failed to run periodic cereal_expire_dead_workers")
				}
			}
		}
		w.wgStop.Done()
		logrus.Debug("exiting worker cleaner")
	}()
}

func (w *workerCleaner) Stop() {
	w.stop()
	w.wgStop.Wait()
}

func (w *workerCleaner) expireDeadWorkers(ctx context.Context) error {
	rows, err := w.db.QueryContext(ctx,
		"SELECT cereal_expire_dead_workers($1)", int64(math.Ceil(w.workerTimeout.Seconds())))
	if err != nil {
		return err
	}

	defer func() {
		if err := rows.Close(); err != nil {
			logrus.WithError(err).Error("failed to close db rows")
		}
	}()

	for rows.Next() {
		var workerID string
		err := rows.Scan(&workerID)
		if err != nil {
			return nil
		}
		logrus.Warnf("Task worker %s lost!", workerID)
	}
	return rows.Err()
}
