package nats

import (
	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/proto"

	stan "github.com/nats-io/go-nats-streaming"
	log "github.com/sirupsen/logrus"
)

func (nc *NatsClient) Subscribe() (stan.Subscription, error) {
	log.WithFields(log.Fields{
		"subject":    nc.subject,
		"durable_id": nc.durableID,
		"client_id":  nc.clientID,
	}).Info("Subscribing to subject")

	// Subscribe with durable name
	return nc.conn.Subscribe(nc.subject, func(msg *stan.Msg) {

		log.WithFields(log.Fields{
			"protocol": "nats",
			"data":     string(msg.Data),
			"subject":  nc.subject,
		}).Debug("Message received")

		// TODO: @afiune We should have a way to have multi-message-type ingestion
		// Unmarshal the data and send the message to the channel
		var habMsg habitat.HealthCheckEvent
		err := proto.Unmarshal(msg.Data, &habMsg)

		if err != nil {
			log.WithFields(log.Fields{
				"data":    string(msg.Data),
				"subject": nc.subject,
			}).Error("Unknown message, dropping")
		} else {
			nc.EventsCh <- &habMsg
		}

	}, stan.DurableName(nc.durableID))
}
