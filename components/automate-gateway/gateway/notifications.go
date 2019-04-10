package gateway

import (
	"context"

	ingestProto "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/notifications-client/builder"
	log "github.com/sirupsen/logrus"
)

func (s *Server) AsyncChefRunNotification(ctx context.Context, run *ingestProto.Run) {
	// TODO @afiune remove other parts of the system where we have implemented this code
	ev, err := builder.ChefClientConverge(s.automateURL.String(), run)
	if err != nil {
		log.WithFields(log.Fields{"id": run.Id}).Warnf("Could not build Chef Client event: %v", err)
		return
	}

	notifier, err := s.clientsFactory.Notifier()
	if err != nil {
		log.WithFields(log.Fields{
			"service": "Notifications",
			"error":   err,
		}).Warn("Could not create client")
		return
	}

	// This happens asynchronously
	notifier.Send(ctx, ev)
}
