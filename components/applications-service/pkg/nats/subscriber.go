package nats

import (
	natsc "github.com/nats-io/nats.go"
	stan "github.com/nats-io/stan.go"
	log "github.com/sirupsen/logrus"
)

func (nc *NatsClient) Subscribe(eventsCh chan<- []byte) error {

	log.WithFields(log.Fields{
		"subject":    deprecatedNATSStreamHealthCheckChannel,
		"durable_id": nc.durableID,
		"client_id":  nc.clientID,
	}).Info("Subscribing to subject")

	// Subscribe with durable name
	_, err := nc.streamConn.Subscribe(deprecatedNATSStreamHealthCheckChannel, func(msg *stan.Msg) {

		log.WithFields(log.Fields{
			"protocol":          "nats streaming",
			"channel":           deprecatedNATSStreamHealthCheckChannel,
			"message_data":      string(msg.Data),
			"message_timestamp": msg.Timestamp,
			"message_subject":   msg.Subject,
			"message_sequence":  msg.Sequence,
		}).Debug("Message received")

		eventsCh <- msg.Data
	}, stan.DurableName(nc.durableID))

	if err != nil {
		return err
	}
	_, err = nc.msgConn.QueueSubscribe(natsMessagingSubscribeGroup, natsMessagingQueueGroup, func(msg *natsc.Msg) {
		log.WithFields(log.Fields{
			"protocol":        "nats messaging",
			"message_data":    string(msg.Data),
			"message_subject": msg.Subject,
		}).Debug("Message received")
		eventsCh <- msg.Data
	})

	return err

}
