package nats

import (
	"crypto/tls"
	"crypto/x509"
	"math/rand"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/lib/tls/certs"

	natsc "github.com/nats-io/go-nats"
	stan "github.com/nats-io/go-nats-streaming"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

// The subject is the topic this subscriber will be listening to,
// right now we will hardcode this value but in the future it could
// be configurable.
//
// Additionally, we could scope our messages into streams or topics.
// For example:
//
// * habitat            - top level subject to enclose all habitat messages
// * habitat.service    - messages about service events
// * habitat.supervisor - messages about supervisor events
//
// With this subjects we could define boilerplate messages and have services
// subscribe to specific topics of interest.
//
// TODO @afiune Make a proposal with diagrams to explain this
type NatsClient struct {
	natsURL   string
	clusterID string
	clientID  string
	durableID string
	subject   string
	certs.TLSConfig
	conn               stan.Conn
	retries            int
	HabServiceEventCh  chan *applications.HabService // TODO: @afiune make a pipeline instead
	InsecureSkipVerify bool
	DisableTLS         bool
}

func NewExternalClient(url, cluster, client, durable, subject string) *NatsClient {
	return &NatsClient{
		natsURL:           url,
		clusterID:         cluster,
		clientID:          client,
		durableID:         durable,
		subject:           subject,
		HabServiceEventCh: make(chan *applications.HabService), // buffered channel?
		retries:           5,
	}
}

// New creates a new client struct with some defaults
func New(url, cluster, client, durable, subject string, tlsConfig certs.TLSConfig) *NatsClient {
	return &NatsClient{
		natsURL:           url,
		clusterID:         cluster,
		clientID:          client,
		durableID:         durable,
		subject:           subject,
		HabServiceEventCh: make(chan *applications.HabService), // buffered channel?
		retries:           5,
		TLSConfig:         tlsConfig,
	}
}

// NewDefaults creates a new client struct with some defaults
func NewDefaults(url, id string, tlsConfig certs.TLSConfig) *NatsClient {
	return New(
		url,
		id,
		"applications-service",
		"applications-service",
		"habitat",
		tlsConfig,
	)
}

// NewAndConnect creates a new client struct and connects to the NATS Server
func NewAndConnect(url, id string, tlsConfig certs.TLSConfig) (*NatsClient, error) {
	client := NewDefaults(url, id, tlsConfig)

	err := client.Connect()

	return client, err
}

// Connect tries to connect and retries until the maximun time configured
// perhaps in the future when this is not beta we don't exit after a number
// of retries and instead continue until we succeed. (when the server is up)
func (nc *NatsClient) Connect() error {
	var (
		conn  stan.Conn
		err   error
		tries = uint64(0)
	)

	tlsConf, err := nc.natsTLSConfig()
	if err != nil {
		return err
	}
	var mTLSEnabled bool
	if tlsConf != nil {
		mTLSEnabled = true
	}

	log.WithFields(log.Fields{
		"nats_url":     nc.natsURL,
		"cluster_id":   nc.clusterID,
		"client_id":    nc.clientID,
		"mtls_enabled": mTLSEnabled,
	}).Info("Connecting to NATS Server")

	for tries < uint64(nc.retries) {
		conn, err = nc.tryConnect(tlsConf)
		if err == nil {
			nc.conn = conn
			return nil
		}

		log.WithFields(log.Fields{
			"error":       err.Error(),
			"nats_url":    nc.natsURL,
			"cluster_id":  nc.clusterID,
			"retries":     tries,
			"max_retries": nc.retries,
		}).Error("Unable to connect to server")

		baseSleep := int64(uint32(1) << tries)
		randomization := rand.Int63n(baseSleep)
		totalSleep := baseSleep + randomization
		time.Sleep(time.Duration(totalSleep) * time.Second)

		tries++
	}

	return errors.Wrapf(err, "failed to connect to nats streaming via %q", nc.natsURL)
}

func (nc *NatsClient) tryConnect(tlsConf *tls.Config) (stan.Conn, error) {
	natsOpts := []natsc.Option{}
	if tlsConf != nil {
		natsOpts = append(natsOpts, natsc.Secure(tlsConf))
	}

	rawNATSconn, err := natsc.Connect(nc.natsURL, natsOpts...)
	if err != nil {
		return nil, err
	}

	conn, err := stan.Connect(nc.clusterID,
		nc.clientID,
		stan.NatsConn(rawNATSconn),
	)
	if err != nil {
		rawNATSconn.Close()
		return nil, err
	}
	return conn, nil
}

// ConnectAndSubscribe will attempt to connect to the NATS Server and then
// subscribe to the subject that the client was configured
func (nc *NatsClient) ConnectAndSubscribe() error {

	err := nc.Connect()
	if err != nil {
		return err
	}

	_, err = nc.Subscribe()
	if err != nil {
		return err
	}

	// Do not unsubscribe this client so that when it comes back online
	// the server will resume delivering events where it left off
	// defer sub.Unsubscribe()
	return nil
}

// ConnectAndPublish will attempt to connect to the NATS Server and then
// publish a message to the subject that the client was configured
func (nc *NatsClient) ConnectAndPublish(msg *applications.HabService) error {

	err := nc.Connect()
	if err != nil {
		return err
	}

	err = nc.PublishHabService(msg)
	if err != nil {
		return err
	}

	nc.Close()

	return nil
}

// Close closes the NATS streaming connection and then the underlying raw NATS
// connection. Since we have to create the raw NATS connection manually in
// order to configure TLS, we are responsible for closing it.
func (nc *NatsClient) Close() {
	natsConn := nc.conn.NatsConn()
	nc.conn.Close()  // nolint: errcheck
	natsConn.Close() // nolint: errcheck
}

func (nc *NatsClient) natsTLSConfig() (*tls.Config, error) {
	if nc.DisableTLS {
		return nil, nil
	}

	t := &tls.Config{
		MinVersion:         tls.VersionTLS12,
		InsecureSkipVerify: nc.InsecureSkipVerify,
	}

	// if CertPath is blank, we aren't doing mutual TLS so just use the minimal
	// settings
	if nc.TLSConfig.CertPath == "" {
		return t, nil
	}

	certs, err := nc.TLSConfig.ReadCerts()
	if err != nil {
		return nil, errors.Wrap(err, "failed to load TLS certs/key for NATS messaging client")
	}

	automateRootCA := x509.NewCertPool()
	automateRootCA.AddCert(certs.RootCACert)

	t.Certificates = []tls.Certificate{*certs.ServiceKeyPair}
	t.RootCAs = automateRootCA
	t.ServerName = "event-service"

	return t, nil

}
