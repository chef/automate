package secureconn

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"

	"github.com/chef/automate/lib/grpc/debug"
	"github.com/chef/automate/lib/grpc/debug/debug_api"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
)

// Factory is used to create secure service to service connections
type Factory struct {
	ServiceKeyPair tls.Certificate
	CertPool       *x509.CertPool

	DisableDebugServer bool
	DebugServerOpts    []debug.ServerOpt
}

// FactoryOpt are options on the factory
type FactoryOpt func(*Factory)

// DisableDebugServer prevents the factory from automatically registering the debug server
func DisableDebugServer() FactoryOpt {
	return func(f *Factory) {
		f.DisableDebugServer = true
	}
}

// WithVersionInfo sets the version information that the debug version
// endpoint returns.
func WithVersionInfo(version, gitRef string) FactoryOpt {
	return func(f *Factory) {
		f.DebugServerOpts = append(f.DebugServerOpts, debug.WithVersionInfo(version, gitRef))
	}
}

// NewFactory creates a new secureconn.Factory that is capable of creating grpc.Servers and
// and grpc.ClientConns that use mutual tls for service to service encryption and authorization
func NewFactory(serviceCerts certs.ServiceCerts, opts ...FactoryOpt) *Factory {
	certPool := serviceCerts.NewCertPool()

	f := &Factory{
		ServiceKeyPair: *serviceCerts.ServiceKeyPair,
		CertPool:       certPool,
	}

	for _, o := range opts {
		o(f)
	}

	return f
}

// Dial is a DialContext with context.Background. See (*Factory).DialContext.
func (f *Factory) Dial(serviceName string, target string, opts ...grpc.DialOption) (*grpc.ClientConn, error) {
	return f.DialContext(context.Background(), serviceName, target, opts...)
}

// DialContext is a wrapper for grpc.DialContext that adds options to use the
// current services key pair to authenticate with the server being dialed into.
// You must also provide the service name pointed to by target as the client
// will check the service's certificate's CN to make sure it matches.
//
// Our convention is that service name, the component's directory name in
// components/ and the component's certificate filename in dev/certs/ should
// all match.
func (f *Factory) DialContext(
	ctx context.Context,
	serviceName string,
	target string,
	opts ...grpc.DialOption,
) (conn *grpc.ClientConn, err error) {
	return grpc.DialContext(ctx, target, append(f.DialOptions(serviceName), opts...)...)
}

// NewServer is a wrapper for grpc.NewServer that adds server options to verify clients using
// the factory's root CA
func (f *Factory) NewServer(opt ...grpc.ServerOption) *grpc.Server {
	s := grpc.NewServer(append(f.ServerOptions(), opt...)...)

	if !f.DisableDebugServer {
		debug_api.RegisterDebugServer(s, debug.NewDebugServer(f.DebugServerOpts...))
	}

	return s
}

// ServerOptions returns a list of ServerOptions this factory uses to create a server
func (f *Factory) ServerOptions() []grpc.ServerOption {
	creds := credentials.NewTLS(&tls.Config{
		ClientAuth:   tls.RequireAndVerifyClientCert,
		Certificates: []tls.Certificate{f.ServiceKeyPair},
		ClientCAs:    f.CertPool,
		// gRPC is used between services we own and thus we
		// can use a more limited set of TLS options.
		//
		// TLS1.2 is required for the upgrade to
		// HTTP2. Marking this explicitly helps us pass
		// scanners who can't pass the client auth and thus
		// only see what is offered in the initial handshake.
		MinVersion: tls.VersionTLS12,
		// We're the boss.
		PreferServerCipherSuites: true,
		CipherSuites:             DefaultCipherSuites(),
	})
	return []grpc.ServerOption{
		grpc.Creds(creds),
	}
}

// DialOptions returns a list of DialOptions this factory uses to connect to clients.
func (f *Factory) DialOptions(serviceName string) []grpc.DialOption {
	creds := credentials.NewTLS(&tls.Config{
		ServerName:   serviceName,
		Certificates: []tls.Certificate{f.ServiceKeyPair},
		RootCAs:      f.CertPool,
	})
	return []grpc.DialOption{
		grpc.WithBackoffConfig(grpc.BackoffConfig{
			MaxDelay: 10 * time.Second,
		}),
		grpc.WithTransportCredentials(creds),
		tracing.GlobalClientInterceptor(),
	}
}

// The following is a re-implementation of some of what is in the
// Golang standard library to construct the default cipher suite list
// with minor modifications.
//
// We re-implement it here to disable 3DES cipher suites while still
// getting the hardware-capability-based cipher ordering selection.
var (
	once                sync.Once
	defaultCipherSuites []uint16
)

func DefaultCipherSuites() []uint16 {
	once.Do(initCipherSuites)
	return defaultCipherSuites
}

// initCipherSuites populates defaultCipherSuites. The
// defaultCipherSuites are ordered by preference. We attempt to detect
// the presence of CPU features that make AES-GCM ciphers faster and
// prefer those if present.
func initCipherSuites() {
	var topCipherSuites []uint16
	if HasAESNI() {
		// If AES-GCM hardware is provided then prioritize AES-GCM
		// cipher suites.
		topCipherSuites = []uint16{
			tls.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256,
			tls.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384,
			tls.TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256,
			tls.TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384,
			tls.TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305,
			tls.TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305,
		}
	} else {
		// Without AES-GCM hardware, we put the ChaCha20-Poly1305
		// cipher suites first.
		topCipherSuites = []uint16{
			tls.TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305,
			tls.TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305,
			tls.TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256,
			tls.TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384,
			tls.TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256,
			tls.TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384,
		}
	}
	commonCiphers := []uint16{
		// Disabled since our new SSL scanner disallows anything with 128 Bit CBC
		//
		// tls.TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA,
		// tls.TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA,
		tls.TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA,
		tls.TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA,
		tls.TLS_RSA_WITH_AES_128_GCM_SHA256,
		tls.TLS_RSA_WITH_AES_256_GCM_SHA384,
		// tls.TLS_RSA_WITH_AES_128_CBC_SHA,
		tls.TLS_RSA_WITH_AES_256_CBC_SHA,
		// Golang enables these but we disable them
		// because 3DES makes scanners unhappy
		//
		// tls.TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA,
		// tls.TLS_RSA_WITH_3DES_EDE_CBC_SHA,
		//
		// Disabled in Golang since only SHA1 CBC has Lucky13 mitigations
		// tls.TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256,
		// tls.TLS_RSA_WITH_AES_128_CBC_SHA256,
		// tls.TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256,
	}

	defaultCipherSuites = append(topCipherSuites, commonCiphers...)
}
