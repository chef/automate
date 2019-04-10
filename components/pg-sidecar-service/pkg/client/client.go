package client

import (
	"context"
	"fmt"
	"time"

	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/pg_sidecar"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

// Opts are the pg-sidecar-server's gRPC client's options
type Opts struct {
	conTimeout    time.Duration
	host          string
	port          int
	tlsCertPath   string
	tlsKeyPath    string
	tlsRootCAPath string
}

// Address returns the pg-sidecar-server address from the host and port
func (o *Opts) Address() string {
	return fmt.Sprintf("%s:%d", o.host, o.port)
}

func defaultOpts() *Opts {
	return &Opts{
		conTimeout: 1 * time.Second,
		host:       "localhost",
		port:       10100,
	}
}

// OptFunc is a Client functional argument
type OptFunc func(c *Opts)

// WithConTimeout configures the timeout duration
func WithConTimeout(timeout time.Duration) OptFunc {
	return func(opts *Opts) {
		opts.conTimeout = timeout
	}
}

// WithHost configures the service host
func WithHost(host string) OptFunc {
	return func(opts *Opts) {
		opts.host = host
	}
}

// WithPort configures the service port
func WithPort(port int) OptFunc {
	return func(opts *Opts) {
		opts.port = port
	}
}

// WithTLSCertPath configures the mTLS cert path
func WithTLSCertPath(path string) OptFunc {
	return func(opts *Opts) {
		opts.tlsCertPath = path
	}
}

// WithTLSKeyPath configures the mTLS key path
func WithTLSKeyPath(path string) OptFunc {
	return func(opts *Opts) {
		opts.tlsKeyPath = path
	}
}

// WithTLSRootCAPath configures the mTLS root CA cert path
func WithTLSRootCAPath(path string) OptFunc {
	return func(opts *Opts) {
		opts.tlsRootCAPath = path
	}
}

// Client is the pg-sidecar-service client
type Client struct {
	api.PGSidecarClient
}

// NewClient takes optional functional client args and returns a new Client
func NewClient(OptFuncs ...OptFunc) (*Client, error) {
	opts := defaultOpts()
	for _, o := range OptFuncs {
		o(opts)
	}

	c := certs.TLSConfig{
		CertPath:       opts.tlsCertPath,
		KeyPath:        opts.tlsKeyPath,
		RootCACertPath: opts.tlsRootCAPath,
	}
	certData, err := c.ReadCerts()
	if err != nil {
		return nil, err
	}

	ctx, cancel := context.WithTimeout(context.Background(), opts.conTimeout)
	defer cancel()

	con, err := secureconn.NewFactory(*certData).DialContext(
		ctx,
		"pg-sidecar-service",
		opts.Address(),
		grpc.WithBlock(),
	)
	if err != nil {
		return nil, err
	}

	return &Client{PGSidecarClient: api.NewPGSidecarClient(con)}, nil
}
