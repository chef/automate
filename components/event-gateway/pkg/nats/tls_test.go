package nats

import (
	"crypto/tls"
	"crypto/x509"
	"io/ioutil"
	"testing"
	"time"

	"github.com/chef/automate/components/event-gateway/pkg/config"
	"github.com/chef/automate/lib/tls/certs"
	natsc "github.com/nats-io/nats.go"
	"github.com/stretchr/testify/require"
)

func TestNATSWithSNI(t *testing.T) {
	c := &config.EventGatewayConfig{
		Service: config.Nats{
			Host: "localhost",
			// use a different port so you can run the tests while having Automate
			// running
			Port:                       24222,
			GatewayPort:                0, // not used
			HealthCheckCredentialsFile: "/NOT_USED",
		},
		LogConfig: config.LogConfig{
			LogFormat: "text",
			LogLevel:  "debug",
		},
		FrontendTLS: []certs.TLSConfig{
			// Two certs with different CNs. Later in the test we connect to the
			// server and use SNI to pick which cert we expect to get
			{
				CertPath: "../../../../dev/certs/event-gateway-sni-one.crt",
				KeyPath:  "../../../../dev/certs/event-gateway-sni-one.key",
			},
			{
				CertPath: "../../../../dev/certs/event-gateway-sni-two.crt",
				KeyPath:  "../../../../dev/certs/event-gateway-sni-two.key",
			},
		},
	}

	configurators := []NATSdConfigurator{
		WithNATSLogging(c),
		WithNATSListenOpts(c),
		WithNATSHTTPMonitoringDisabled(c),
		WithNATSFrontEndCerts(c),
	}

	ns, err := ServerWithNATSConfig(configurators...)

	require.NoError(t, err)

	go ns.Start()
	defer ns.Shutdown()

	// Wait for it to be able to accept connections
	ready := ns.ReadyForConnections(10 * time.Second)
	require.True(t, ready)

	automateRootCA := x509.NewCertPool()
	f := "../../../../dev/certs/Chef_Automate_FAKE_Dev.crt"
	rootPEM, err := ioutil.ReadFile(f)
	require.NoError(t, err)
	require.NotNil(t, rootPEM)
	ok := automateRootCA.AppendCertsFromPEM(rootPEM)
	require.True(t, ok)

	// ServerName in the tls.Config will be used with server name indication to
	// pick the cert the server will use; the local client will check that the
	// cert's CN matches.
	clientTLSConfigOne := &tls.Config{
		MinVersion: tls.VersionTLS12,
		ServerName: "event-gateway-sni-one",
		RootCAs:    automateRootCA,
	}

	clientTLSConfigTwo := &tls.Config{
		MinVersion: tls.VersionTLS12,
		ServerName: "event-gateway-sni-two",
		RootCAs:    automateRootCA,
	}

	clientTLSConfigBAD := &tls.Config{
		MinVersion: tls.VersionTLS12,
		ServerName: "event-gateway-sni-BAD", // this one is supposed to fail
		RootCAs:    automateRootCA,
	}

	natsURL := "nats://localhost:24222"
	connOne, err := natsc.Connect(natsURL, natsc.Secure(clientTLSConfigOne))
	require.NoError(t, err)
	connOne.Close()

	connTwo, err := natsc.Connect(natsURL, natsc.Secure(clientTLSConfigTwo))
	require.NoError(t, err)
	connTwo.Close()

	_, err = natsc.Connect(natsURL, natsc.Secure(clientTLSConfigBAD))
	require.Error(t, err)
}

func TestNATSWithTLSDisabled(t *testing.T) {
	c := &config.EventGatewayConfig{
		Service: config.Nats{
			Host: "localhost",
			// use a different port so you can run the tests while having Automate
			// running
			Port:                       24222,
			GatewayPort:                0, // not used
			HealthCheckCredentialsFile: "/NOT_USED",
			DisableFrontendTLS:         true,
		},
		LogConfig: config.LogConfig{
			LogFormat: "text",
			LogLevel:  "debug",
		},
		FrontendTLS: nil,
	}

	configurators := []NATSdConfigurator{
		WithNATSLogging(c),
		WithNATSListenOpts(c),
		WithNATSHTTPMonitoringDisabled(c),
		WithNATSFrontEndCerts(c),
	}

	ns, err := ServerWithNATSConfig(configurators...)

	require.NoError(t, err)

	go ns.Start()
	defer ns.Shutdown()

	// Wait for it to be able to accept connections
	ready := ns.ReadyForConnections(10 * time.Second)
	require.True(t, ready)

	natsURL := "nats://localhost:24222"
	conn, err := natsc.Connect(natsURL)
	require.NoError(t, err)
	require.Equal(t, natsc.CONNECTED, conn.Status())
	require.False(t, conn.TLSRequired())
	conn.Close()
}
