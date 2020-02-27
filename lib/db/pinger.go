package db

import (
	"context"
	"database/sql"
	"sync"
	"time"

	"github.com/sirupsen/logrus"
)

type pinger struct {
	db           *sql.DB
	pingInterval time.Duration

	wg   sync.WaitGroup
	sg   startGuard
	stop context.CancelFunc
}

func newPinger(db *sql.DB, pingInterval time.Duration) *pinger {
	return &pinger{
		db:           db,
		pingInterval: pingInterval,
		sg:           newStartGuard("Start(ctx) called more than once on db.pinger!"),
	}
}

// Start starts a goroutine that pings the db on some interval.  This
// function is only allowed to be called once
func (w *pinger) Start(ctx context.Context) {
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

	OUTER:
		for {

			select {
			case <-ctx.Done():
				break OUTER
			case <-time.After(w.pingInterval):
				err := w.db.PingContext(ctx)
				if isDBClosed(err) {
					logrus.WithError(err).Debug("underlying sql.DB is closed, exiting connection pinger")
					break OUTER
				}
			}
		}
		w.wg.Done()
	}()
}

func (w *pinger) Stop() {
	w.stop()
	w.wg.Wait()
}

func isDBClosed(err error) bool {
	if err == nil {
		return false
	}
	switch err.Error() {
	case "sql: database is closed":
		return true
	default:
		return false
	}
}

// TODO(ssd) 2020-02-26: copy/pasta from cereal, consider moving into
// common library.
//
// startGuard is used to enforce that Start is only called once for
// various structures inside cereal.
//
// We were previously using sync.WaitGroup for this, but this allows
// us to customize the error message a bit.
//
// The zero-value of this should work as expected, but you can create
// a StartGuard with a custom message using NewStartGuard.
type startGuard struct {
	msg     string
	started bool
	mu      sync.Mutex
}

func newStartGuard(msg string) startGuard {
	return startGuard{
		msg: msg,
	}
}

func (s *startGuard) Started() {
	s.mu.Lock()
	if s.started {
		if s.msg != "" {
			panic(s.msg)
		} else {
			panic("Started() on startGuard called more than once.")
		}
	}
	s.started = true
	s.mu.Unlock()
}
