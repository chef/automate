package nats

import (
	stan "github.com/nats-io/go-nats-streaming"
	log "github.com/sirupsen/logrus"
)

func (nc *NatsClient) Subscribe(eventsCh chan<- []byte) (stan.Subscription, error) {

	log.WithFields(log.Fields{
		"subject":    nc.subject,
		"durable_id": nc.durableID,
		"client_id":  nc.clientID,
	}).Info("Subscribing to subject")

	// Subscribe with durable name
	return nc.conn.Subscribe(nc.subject, func(msg *stan.Msg) {

		log.WithFields(log.Fields{
			"protocol":          "nats",
			"message_data":      string(msg.Data),
			"message_timestamp": msg.Timestamp,
			"message_subject":   msg.Subject,
			"message_sequence":  msg.Sequence,
		}).Debug("Message received")

		eventsCh <- msg.Data
	}, stan.DurableName(nc.durableID))
}
