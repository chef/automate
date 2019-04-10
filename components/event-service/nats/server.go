package nats

import (
	"crypto/tls"
	"fmt"
	"sync"
	"time"

	"github.com/chef/automate/components/event-service/config"

	natsd "github.com/nats-io/gnatsd/server"
	"github.com/nats-io/go-nats"
	streamd "github.com/nats-io/nats-streaming-server/server"
	stores "github.com/nats-io/nats-streaming-server/stores"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

type shutdowner interface {
	Shutdown()
}

type multiEmbeddedServer struct {
	servers []shutdowner
	errchan chan error
	mu      sync.Mutex
}

func (m *multiEmbeddedServer) registerServer(s shutdowner) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.servers = append(m.servers, s)
}

func (m *multiEmbeddedServer) runInGoroutine(f func() error) {
	go func() {
		err := f()
		m.errchan <- err
	}()
}

func (m *multiEmbeddedServer) waitForErrors() error {
	err := <-m.errchan
	log.Errorf("Shutting down all embedded servers due to error %q", err)
	for _, s := range m.servers {
		s.Shutdown()
	}
	return err
}

func Spawn(c *config.EventConfig) error {
	m := &multiEmbeddedServer{
		errchan: make(chan error),
	}

	if err := spawnNatsInternalServer(c, m); err != nil {
		return err
	}
	if err := runNATSStreamingServer(c, m); err != nil {
		return err
	}
	return m.waitForErrors()
}

func mTLSConfFor(c *config.EventConfig, service string) *tls.Config {
	// NOTE: the natsd options struct has elements for cert/key/ca, but they
	// don't get used when embedding, so we have to make a tls.Config. Also, we
	// need to make a tls.Config in order to set the ServerName anyway
	natsTLSOpts := &natsd.TLSConfigOpts{
		CertFile: c.TLSConfig.CertPath,
		KeyFile:  c.TLSConfig.KeyPath,
		CaFile:   c.TLSConfig.RootCACertPath,
		Verify:   true,
	}

	natsTLSConf, err := natsd.GenTLSConfig(natsTLSOpts)
	if err != nil {
		log.Fatalf("failed to configure NATS TLS options: %s", err)
	}

	natsTLSConf.RootCAs = natsTLSConf.ClientCAs
	if service != "" {
		natsTLSConf.ServerName = service
	}

	return natsTLSConf
}

func spawnNatsInternalServer(c *config.EventConfig, m *multiEmbeddedServer) error {
	nopts := natsdOptions(c)

	ns, err := natsd.NewServer(nopts)
	if err != nil {
		return errors.Wrapf(err, "NATS server options failed validation for opts %+v", nopts)
	}
	// NATS won't log at all if we don't make it configure its logger
	ns.ConfigureLogger()

	log.WithFields(log.Fields{
		"host": nopts.Host,
		"port": nopts.Port,
	}).Info("Starting NATS messaging server for internal services")

	m.runInGoroutine(func() error {
		m.registerServer(ns)
		ns.Start()
		return errors.New("NATS server exited")
	})

	// Wait for it to be able to accept connections
	if !ns.ReadyForConnections(10 * time.Second) {
		return errors.Errorf("not able to start NATS server %+v", ns)
	}
	return nil
}

func runNATSStreamingServer(c *config.EventConfig, m *multiEmbeddedServer) error {
	// Configure NATS Streaming Server with unique cluster identifier
	// and a persistent file based datastore
	opts := streamd.GetDefaultOptions()
	opts.NATSServerURL = fmt.Sprintf("nats://localhost:%d", c.InternalMessaging.Port)
	opts.ID = c.StreamService.ClusterID
	opts.StoreType = stores.TypeFile
	opts.FilestoreDir = c.StreamService.Store
	opts.ClientCert = c.TLSConfig.CertPath
	opts.ClientKey = c.TLSConfig.KeyPath
	opts.ClientCA = c.TLSConfig.RootCACertPath
	opts.Secure = true
	opts.EnableLogging = true

	if c.LogConfig.LogLevel == "debug" {
		opts.Debug = true
		opts.Trace = true
	}

	clientTLSconfig := mTLSConfFor(c, "event-service")
	opts.NATSClientOpts = []nats.Option{nats.Secure(clientTLSconfig)}

	natsServerOpts := streamd.NewNATSOptions()

	log.WithFields(log.Fields{
		"cluster_id":     opts.ID,
		"store_type":     opts.StoreType,
		"store_location": opts.FilestoreDir,
	}).Info("Starting NATS Streaming Server")

	m.runInGoroutine(func() error {
		nsServer, err := streamd.Run(opts, natsServerOpts)
		if err != nil {
			return err
		}
		m.registerServer(nsServer)
		// Poll for NATS streaming shutdown. NATS streaming doesn't offer a
		// blocking way to run the server, so we must watch to see if it's shutdown
		// in order to be able to bring down all of events service in case of
		// unexpected exit.
		for {
			if nsServer.State() == streamd.Shutdown {
				return errors.New("NATS streaming server exited")
			}
			time.Sleep(1 * time.Second)
		}
	})
	return nil
}

func natsdOptions(c *config.EventConfig) *natsd.Options {
	nopts := &natsd.Options{}

	// Logging options:
	nopts.NoLog = false

	if c.LogConfig.LogLevel == "debug" {
		// This seems to be the only option for adjusting the logging level
		// available to us. NATS doesn't log too much on "info" level so it should
		// be ok.
		nopts.Debug = true
		// Trace logs all client activity in messages like this:
		// [TRC] 127.0.0.1:53438 - cid:2 - <<- [SUB _STAN.discover.event-service  2]
		nopts.Trace = true
	}

	// Setting to zero disables the HTTP monitoring server.
	nopts.HTTPPort = 0
	nopts.HTTPSPort = 0

	nopts.Host = c.ServiceConfig.Host
	nopts.Port = c.InternalMessaging.Port

	// This TLS config isn't used for outbound connections, so disable setting
	// the server name
	nopts.TLSConfig = mTLSConfFor(c, "")

	// the external NATS will initiate a connection using NATS gateway protocol
	// on a separate port.
	nopts.Gateway.Name = "event-service"
	nopts.Gateway.Host = c.ServiceConfig.Host
	nopts.Gateway.Port = c.InternalMessaging.GatewayPort

	// internal NATS communicates to event-gateway over NATS gateway
	// protocol. Event Gateway will bind to us via hab and initiate a
	// gateway connection. The internal NATS will then "discover" Automate Event
	// Gateway as an "implicit" gateway and try to connect to it to share NATS
	// routes, etc. in both directions. Since we will verify the CN of the remote
	// server's cert, we need to set our TLS config to expect the remote service
	// to have Event Gateway's cert. This is the only connection we
	// expect event service NATS to initiate so the hardcoding shouldn't be an issue.
	nopts.Gateway.TLSConfig = mTLSConfFor(c, "event-gateway")

	return nopts
}
