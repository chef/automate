package server

import (
	"context"
	"sync"
	"time"

	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
)

// A Looper runs a user-supplied func repeatedly. The Looper will wait
// at least the length of the interval between calls to the
// user-supplied function.
type Looper struct {
	interval time.Duration
	userFunc func()
	quitChan chan struct{}
	// Since we want to be able to stop and then restart the same
	// looper, we can't use the nice sync.Once interface to
	// prevent multiple Starts and Stops. Rather we use a boolean
	// and a mutex.
	running bool
	mu      sync.Mutex
}

// NewLooper returns a Looper
func NewLooper(interval time.Duration, userFunc func()) *Looper {
	return &Looper{
		interval: interval,
		userFunc: userFunc,
		running:  false,
	}
}

// Start starts the Looper. Start is safe to call multiple times;
// Calls to Start on a running Looper will do nothing. Start is safe
// to call on a Looper that has been previously stopped and will
// restart Looper.
func (l *Looper) Start() {
	l.mu.Lock()
	defer l.mu.Unlock()

	if l.running {
		return
	}

	// quitChan gets closed on stop, so make a new one.
	l.quitChan = make(chan struct{})
	l.running = true
	go l.runLoop(l.quitChan)
}

// Stop will stop a running Looper. Stop is safe to call on an already
// stopped Looper.  Stop will not kill any currently running userFunc.
func (l *Looper) Stop() {
	l.mu.Lock()
	defer l.mu.Unlock()

	if !l.running {
		return
	}

	l.running = false
	close(l.quitChan)
}

func (l *Looper) runLoop(quitChan chan struct{}) {
	timer := time.NewTimer(0)
	defer timer.Stop()
	jobDoneChan := make(chan struct{})
	logrus.Infof("Converging deployment every %s", l.interval)
	for {
		select {
		case <-timer.C:
			// Run the userFunc in a goroutine so
			// we don't get blocked and can
			// always respond to a close.
			go func() {
				l.userFunc()
				close(jobDoneChan)
			}()
		case <-jobDoneChan:
			select {
			case <-quitChan:
				// Don't reset the timer if quit has
				// already been called.
				logrus.Infof("Stopping converge loop")
				return
			default:
				jobDoneChan = make(chan struct{})
				timer.Reset(l.interval)
			}
		case <-quitChan:
			logrus.Infof("Stopping converge loop")
			return
		}
	}
}

// gRPC request handlers
func (s *server) StartConverge(context.Context, *api.StartConvergeRequest) (*api.StartConvergeResponse, error) {
	s.convergeLoop.Start()
	return &api.StartConvergeResponse{}, nil
}

func (s *server) StopConverge(context.Context, *api.StopConvergeRequest) (*api.StopConvergeResponse, error) {
	s.convergeLoop.Stop()
	return &api.StopConvergeResponse{}, nil
}
