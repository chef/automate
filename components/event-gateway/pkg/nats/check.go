package nats

import (
	"context"
	"crypto/rand"
	"crypto/tls"
	"encoding/hex"
	"fmt"
	"time"

	natsc "github.com/nats-io/nats.go"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/event-gateway/pkg/config"
)

func ConnectivityCheck(c *config.EventGatewayConfig) error {

	token, err := ReadHealthCheckCredentials(c)
	if err != nil {
		return errors.Wrap(err, "reading credentials for healthcheck")
	}

	natsURL := fmt.Sprintf("nats://%s@localhost:4222", token)

	opts := []natsc.Option{}

	if !c.Service.DisableFrontendTLS {
		t := &tls.Config{
			MinVersion:         tls.VersionTLS12,
			InsecureSkipVerify: true,
		}
		opts = append(opts, natsc.Secure(t))
	}

	log.WithFields(log.Fields{"url": natsURL}).Debug("connecting to event gateway")
	conn, err := natsc.Connect(natsURL, opts...)
	if err != nil {
		return errors.Wrapf(err, "connecting to NATS server %q", natsURL)
	}
	defer conn.Close()

	// Generate a random hex string to use as a sub-topic. This prevents two
	// healthchecks from interacting if they run concurrently.
	b := make([]byte, 20)
	_, err = rand.Read(b)
	if err != nil {
		return errors.Wrap(err, "generating random topic name for healthcheck")
	}

	slug := hex.EncodeToString(b)
	topicName := fmt.Sprintf("healthcheck.%s", slug)

	// subscriber that will reply
	sub, err := conn.Subscribe(topicName, func(m *natsc.Msg) {
		log.WithFields(log.Fields{"data": string(m.Data), "topic_name": topicName}).Debug("received healthcheck message")
		conn.Publish(m.Reply, m.Data) // nolint errcheck
	})
	if err != nil {
		return errors.Wrapf(err, "subscription to topic %q failed", topicName)
	}
	defer sub.Drain() // nolint errcheck

	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()

	log.WithFields(log.Fields{"data": slug, "topic_name": topicName}).Debug("sending reply request to event gateway")
	reply, err := conn.RequestWithContext(ctx, topicName, []byte(slug))

	if err != nil {
		return errors.Wrap(err, "publishing message and receive reply within timeout")
	}

	log.WithFields(log.Fields{"data": string(reply.Data), "expected": slug}).Debug("received reply from event gateway")

	if string(reply.Data) != slug {
		return errors.Errorf("received unexpected reply. Expected %q, got %q", slug, string(reply.Data))
	}

	return nil
}
