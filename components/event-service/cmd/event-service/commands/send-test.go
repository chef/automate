package commands

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io/ioutil"
	"time"

	natsc "github.com/nats-io/go-nats"
	streamc "github.com/nats-io/go-nats-streaming"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var selfTestFlags = struct {
	token string // API token for external ingest NATS endpoint
}{}

// TODO FIXME: move the guts into a different package
func newSendTestCmd() *cobra.Command {
	c := &cobra.Command{
		Use:          "self-test",
		Short:        "Send test message to messaging server",
		Hidden:       true,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {

			if selfTestFlags.token == "" {
				return errors.New("command requires a valid token to connect to the external NATS endpoint")
			}

			clusterID := "event-service"

			externalURL := fmt.Sprintf("nats://%s@localhost:4222", selfTestFlags.token)

			externalNATS, err := natsc.Connect(externalURL)
			if err != nil {
				return errors.Wrap(err, "connecting to NATS server")
			}
			defer externalNATS.Close()

			internalURL := "nats://localhost:10140"

			mTLSKeyPair, err := tls.LoadX509KeyPair("/hab/svc/event-service/config/service.crt", "/hab/svc/event-service/config/service.key")
			if err != nil {
				return errors.Wrap(err, "loading client TLS certs")
			}

			automateRootCA := x509.NewCertPool()
			f := "/hab/svc/event-service/config/root_ca.crt"
			rootPEM, err := ioutil.ReadFile(f)
			if err != nil || rootPEM == nil {
				return errors.Wrap(err, "loading or parsing rootCA file")
			}
			ok := automateRootCA.AppendCertsFromPEM(rootPEM)
			if !ok {
				return errors.Wrapf(err, "parsing root certificate from %q", f)
			}

			mTLSConfig := &tls.Config{
				MinVersion:   tls.VersionTLS12,
				Certificates: []tls.Certificate{mTLSKeyPair},
				RootCAs:      automateRootCA,
			}
			mTLSConfig.ServerName = "event-service"

			internalNATS, err := natsc.Connect(internalURL, natsc.Secure(mTLSConfig))
			if err != nil {
				return errors.Wrap(err, "connecting to NATS server")
			}
			defer internalNATS.Close()

			// subscribe on internal side
			internalNATS.Subscribe("event-service-self-test-int", func(m *natsc.Msg) {
				log.Infof("Received a message via internal NATS [event-service-self-test-1]: %s\n", string(m.Data))
			})

			// subscribe on external side
			externalNATS.Subscribe("event-service-self-test-ext", func(m *natsc.Msg) {
				log.Infof("Received a message via external NATS [event-service-self-test-2]: %s\n", string(m.Data))
			})

			// publish from internal to internal
			message := fmt.Sprintf("internal to internal at %s", time.Now())
			log.Infof("sending message %q", message)
			internalNATS.Publish("event-service-self-test-int", []byte(message)) // does not return until an ack has been received from NATS Streaming

			// publish from external to external
			message = fmt.Sprintf("external to external at %s", time.Now())
			log.Infof("sending message %q", message)
			externalNATS.Publish("event-service-self-test-ext", []byte(message)) // does not return until an ack has been received from NATS Streaming

			// publish from external to internal
			message = fmt.Sprintf("external to internal at %s", time.Now())
			log.Infof("sending message %q", message)
			externalNATS.Publish("event-service-self-test-int", []byte(message)) // does not return until an ack has been received from NATS Streaming

			// publish from internal to external
			message = fmt.Sprintf("internal to external at %s", time.Now())
			log.Infof("sending message %q", message)
			internalNATS.Publish("event-service-self-test-ext", []byte(message)) // does not return until an ack has been received from NATS Streaming

			ncExternal, err := streamc.Connect(clusterID, "cli-test-external-pub", streamc.NatsConn(externalNATS))
			if err != nil {
				return errors.Wrap(err, "connecting to NATS Streaming server")
			}
			defer ncExternal.Close()

			ncInternal, err := streamc.Connect(clusterID, "cli-test-internal-sub", streamc.NatsConn(internalNATS))
			if err != nil {
				return errors.Wrap(err, "connecting to NATS Streaming server")
			}
			defer ncInternal.Close()

			// Subscriber on internal side
			sub, _ := ncInternal.Subscribe("event-service-self-test-streaming", func(m *streamc.Msg) {
				log.Infof("Received a message [event-service-self-test]: %s\n", string(m.Data))
			})
			defer sub.Unsubscribe()

			// Publisher on external side
			message = fmt.Sprintf("NATS streaming self test at %s", time.Now())
			log.Infof("sending message %q", message)
			ncExternal.Publish("event-service-self-test-streaming", []byte(message)) // does not return until an ack has been received from NATS Streaming

			time.Sleep(1 * time.Second) // wait for the messages to come in

			return nil
		},
	}

	c.PersistentFlags().StringVarP(&selfTestFlags.token, "token", "t", "", "API token for NATS ingest endpoint")

	return c
}
