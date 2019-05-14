package nats

import (
	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/proto"

	log "github.com/sirupsen/logrus"
)

// PublishHabEvent publishes a Habitat event message to the NATS server
//
// Usage:
// ```
// msg := &habitat.HealthCheckEvent{}
// client.PublishHabEvent(msg)
// ```
func (nc *NatsClient) PublishHabEvent(msg *habitat.HealthCheckEvent) error {
	log.WithFields(log.Fields{
		"type":    "HealthCheckEvent",
		"message": msg,
	}).Info("Publishing message")

	b, err := proto.Marshal(msg)
	if err != nil {
		return err
	}
	return nc.conn.Publish(nc.subject, b)
}

// PublishBytes publishes an array of bytes to the NATS server
//
// Usage:
// ```
// msg := []byte("Here is my message...")
// client.PublishBytes(msg)
// ```
func (nc *NatsClient) PublishBytes(b []byte) error {
	log.WithFields(log.Fields{
		"type":    "bytes",
		"message": string(b),
	}).Info("Publishing message")

	return nc.conn.Publish(nc.subject, b)
}
