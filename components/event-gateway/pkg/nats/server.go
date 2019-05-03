package nats

import (
	"crypto/tls"
	"fmt"
	"net/url"

	"github.com/chef/automate/components/event-gateway/pkg/config"
	"github.com/chef/automate/lib/grpc/secureconn"

	natsd "github.com/nats-io/gnatsd/server"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

// NATSdConfigurator is a functional-options-pattern function that configures
// the NATS server.
type NATSdConfigurator func(*natsd.Options) error

// WithNATSFrontEndCerts returns a NATSdConfigurator that configures the NATS
// server to use the frontend TLS certs set on the EventGatewayConfig. Multiple
// certificates can be used; WithNATSFrontEndCerts will call
// tls.Config.BuildNameToCertificate() so that server name indication (SNI)
// will work.
func WithNATSFrontEndCerts(c *config.EventGatewayConfig) NATSdConfigurator {
	return func(nopts *natsd.Options) error {
		if c.Service.DisableFrontendTLS {
			return nil
		}

		keyPairs := []tls.Certificate{}

		for _, cert := range c.FrontendTLS {
			keyPair, err := tls.LoadX509KeyPair(cert.CertPath, cert.KeyPath)
			if err != nil {
				return errors.Wrapf(err, "Failed to load cert/key pair %+v", cert)
			}
			keyPairs = append(keyPairs, keyPair)
		}

		nopts.TLSConfig = &tls.Config{
			Certificates:             keyPairs,
			MinVersion:               tls.VersionTLS12,
			PreferServerCipherSuites: true,
			CipherSuites:             secureconn.DefaultCipherSuites(),
		}
		nopts.TLSConfig.BuildNameToCertificate()
		return nil
	}
}

// WithA2Auth returns a NATSdConfigurator that configures the NATS server to
// use A2's authN and authZ
func WithNATSA2AuthIntegration(c *config.EventGatewayConfig) NATSdConfigurator {
	return func(nopts *natsd.Options) error {
		authenticator, err := newAutomateAuthenticator(c)
		if err != nil {
			return err
		}

		nopts.CustomClientAuthentication = authenticator
		return nil
	}

}

// WithNATSGateway returns a NATSdConfigurator that configures the NATS server
// to use the NATS gateway feature to connect to the internal event service.
func WithNATSGateway(c *config.EventGatewayConfig) NATSdConfigurator {
	return func(nopts *natsd.Options) error {
		// NATS Gateway speaks a slightly different protocol and listens on a
		// different port. Even though event-gateway initiates the
		// connection to the internal NATS, we have to open the port. The internal
		// NATS will "discover" event-gateway and connect back to it.
		nopts.Gateway.Name = "event-gateway"
		nopts.Gateway.Host = c.Service.Host
		nopts.Gateway.Port = c.Service.GatewayPort
		// Use mTLS for gateway protocol connections. After this NATS server
		// initiates a gateway connection to the internal NATS, the internal NATS
		// will connect back.
		mTLSConfig, err := mTLSConf(c)
		if err != nil {
			return err
		}
		nopts.Gateway.TLSConfig = mTLSConfig

		internalURLstr := fmt.Sprintf("nats://%s:%d", c.InternalEventService.Host, c.InternalEventService.GatewayPort)
		internalNATSURL, err := url.Parse(internalURLstr)
		if err != nil {
			return errors.Wrapf(err, "generated invalid URL %q for event-service NATS service", internalURLstr)
		}
		nopts.Gateway.Gateways = []*natsd.RemoteGatewayOpts{
			{Name: "event-service", TLSConfig: mTLSConfig, URLs: []*url.URL{internalNATSURL}},
		}
		return nil
	}
}

// WithNATSLogging returns a NATSdConfigurator that makes NATS log in a way
// that kinda matches the log level set in the EventGatewayConfig. It only
// "kinda" matches because NATS only lets us turn debug and trace logging on or
// off, so we turn them both on for debug and off for log level info and higher.
func WithNATSLogging(c *config.EventGatewayConfig) NATSdConfigurator {
	return func(nopts *natsd.Options) error {
		// Logging
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
		return nil
	}
}

// WithNATSListenOpts returns a NATSdConfigurator that sets the listen host and
// port as set in the EventGatewayConfig
func WithNATSListenOpts(c *config.EventGatewayConfig) NATSdConfigurator {
	return func(nopts *natsd.Options) error {
		// Setting the listen host to `""` will make go try to listen on all IPv4
		// and IPv6 addresses, though go will do a check to ensure IPv6 is really
		// working before listening on IPv6. This roughly matches the behavior of
		// automate-load-balancer where we've implemented our own "is IPv6
		// viable?" check and pass that via config to nginx.
		// References:
		// * https://stackoverflow.com/questions/49067160/what-is-the-difference-in-listening-on-0-0-0-080-and-80
		// * https://golang.org/pkg/net/#Listen
		// * https://github.com/golang/go/blob/56517216c052649daab6c439f386f9dc02e90c3a/src/net/ipsock.go#L236-L300
		nopts.Host = ""
		nopts.Port = c.Service.Port
		return nil
	}
}

// WithNATSHTTPMonitoringDisabled disables the HTTP(S) monitoring servers in NATS.
func WithNATSHTTPMonitoringDisabled(c *config.EventGatewayConfig) NATSdConfigurator {
	return func(nopts *natsd.Options) error {
		// Setting to zero disables the HTTP monitoring server.
		nopts.HTTPPort = 0
		nopts.HTTPSPort = 0
		return nil
	}
}

// Spawn starts an embedded NATS server with all the Automate integrations enabled.
func Spawn(c *config.EventGatewayConfig) error {
	configurators := []NATSdConfigurator{
		WithNATSFrontEndCerts(c),
		WithNATSLogging(c),
		WithNATSListenOpts(c),
		WithNATSHTTPMonitoringDisabled(c),
		WithNATSGateway(c),
		WithNATSA2AuthIntegration(c),
	}

	ns, err := ServerWithNATSConfig(configurators...)
	if err != nil {
		return err
	}

	log.WithFields(log.Fields{
		"host": c.Service.Host,
		"port": c.Service.Port,
	}).Info("Starting NATS messaging Server for event-gateway")

	ns.Start()
	return errors.New("NATS server exited")
}

// ServerWithNATSConfig configures an embedded NATS server with a custom set of NATS
// config options. It's intended to be used in tests where we want to test some
// aspect of the system without needing all of Automate running
func ServerWithNATSConfig(opts ...NATSdConfigurator) (*natsd.Server, error) {
	nopts := &natsd.Options{}

	for _, n := range opts {
		err := n(nopts)
		if err != nil {
			return nil, errors.Wrap(err, "Failed configuring automate integrations for NATS")
		}
	}

	ns, err := natsd.NewServer(nopts)
	if err != nil {
		return nil, errors.Wrapf(err, "NATS server options failed validation for opts %+v", nopts)
	}

	// NATS won't log at all if we don't make it configure its logger
	ns.ConfigureLogger()

	return ns, nil
}

func mTLSConf(c *config.EventGatewayConfig) (*tls.Config, error) {
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
		return nil, errors.Wrap(err, "failed to configure NATS TLS options")
	}

	natsTLSConf.RootCAs = natsTLSConf.ClientCAs
	natsTLSConf.ServerName = "event-service"

	return natsTLSConf, nil
}
