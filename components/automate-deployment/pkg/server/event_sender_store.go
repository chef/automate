package server

import (
	"sync"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/events"
)

// eventSenderStore is a threadsafe map for storing eventSender by
// taskId. It is a thin wrapper around sync.Map that handles type
// conversion. The zero value is an empty store.
//
// TODO(ssd) 2018-02-09: TODO -- we should be storing the tasks
// + sender, but this will do for now
type eventSenderStore struct {
	sync.Map
}

func (s *eventSenderStore) Set(id string, e events.EventSender) {
	s.Store(id, e)
}

func (s *eventSenderStore) Get(id string) (events.EventSender, bool) {
	val, found := s.Load(id)
	if found {
		if ret, ok := val.(events.EventSender); ok {
			return ret, true
		}
		logrus.WithFields(logrus.Fields{
			"task-id": id,
		}).Warn("Found eventSenderStore entry but could not convert it to event sender")
		return nil, false
	}

	return nil, false
}
