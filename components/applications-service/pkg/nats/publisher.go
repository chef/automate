package nats

import (
	"github.com/chef/automate/api/external/applications"
	"github.com/golang/protobuf/proto"

	log "github.com/sirupsen/logrus"
)

// PublishHabService publishes a HabService message to the NATS server
//
// Usage:
// ```
// msg := &applications.HabService{
//   SupervisorId: "asdfg1234qwer5678",
//   Group:        "default",
//   PkgIdent:     &applications.PackageIdent{
//     Origin:  "core",
//     Name:    "redis",
//     Version: "0.1.0",
//     Release: "20190101121212",
//   },
// }
// client.PublishHabService(msg)
// ```
func (nc *NatsClient) PublishHabService(msg *applications.HabService) error {
	log.WithFields(log.Fields{
		"type":    "HabService",
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
