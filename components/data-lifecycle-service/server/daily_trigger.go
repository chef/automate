package server

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/sirupsen/logrus"
)

// DailyTriggerServer is a server that runs Data Lifecycle methods once a day
type DailyTriggerServer struct {
	stop      chan bool
	timeOfDay *ValidatedTimeOfDay
	dls       *Server
}

// NewDailyTriggerServer returns a DailyTriggerServer that runs at atTimeUtc daily
func NewDailyTriggerServer(dls *Server, atTimeUtc *ValidatedTimeOfDay) *DailyTriggerServer {
	return &DailyTriggerServer{
		stop:      make(chan bool),
		timeOfDay: atTimeUtc,
		dls:       dls,
	}
}

// Start starts the server.
func (s *DailyTriggerServer) Start() {
	go s.run()
}

// Stop stops the server
func (s *DailyTriggerServer) Stop() {
	//TODO: maybe wait for stop
	s.stop <- true
}

func (s *DailyTriggerServer) run() {
	nextRun := s.next(time.Now().UTC())

	for {
		sleepFor := time.Until(nextRun)
		logrus.Infof("Next Run at %v (in %v)", nextRun, sleepFor)
		// Negative duration seems to be available right away
		timer := time.NewTimer(sleepFor)
		select {
		case <-s.stop:
			break
		case now := <-timer.C:
			if now.After(nextRun) {
				logrus.Info("Triggering purge")
				_, err := s.dls.TriggerPurge(context.Background(), &data_lifecycle.TriggerPurgeRequest{})
				if err != nil {
					logrus.WithError(err).Error("Triggered purge failed")
				} else {
					logrus.Info("Triggered purge completed")
				}
			}
			nextRun = s.next(nextRun)
		}
	}
}

func (s *DailyTriggerServer) next(now time.Time) time.Time {
	year, month, day := now.UTC().Date()

	nextRun := time.Date(year, month, day, s.timeOfDay.Hour(), s.timeOfDay.Min(), s.timeOfDay.Sec(), 0, time.UTC)

	// The minute padding is used to allow some room for clock skew
	for nextRun.Before(now.Add(time.Minute)) {
		nextRun = nextRun.AddDate(0, 0, 1)
	}

	return nextRun
}
