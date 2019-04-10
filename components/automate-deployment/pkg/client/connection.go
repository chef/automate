package client

import (
	"context"
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/pkg/errors"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

// ServiceAddressKey is the env variable key for service address
const ServiceAddressKey = "DEPLOYMENT_SERVICE_ADDRESS"
const defaultAddress = "localhost:10160"

// DefaultDeployTimeout is the timeout duration to use when initializing the
// Connection in the context of a command that deploys things
const DefaultDeployTimeout = 5 * time.Second

// DefaultClientTimeout is the timeout duration to use when initializing the
// Connection in the context of a command that does basic API queries
const DefaultClientTimeout = 1 * time.Second

var persistentConnection *DSClient

// GetAddress fetches the service address from an env variable
func GetAddress() (string, error) {
	address := os.Getenv(ServiceAddressKey)
	if address == "" {
		return defaultAddress, nil
	}
	match, err := regexp.MatchString(`[^:]+:\d`, address)
	if err != nil {
		return "", errors.Wrap(err, "failed to parse network address regular expression")
	}
	if !match {
		return "", fmt.Errorf("host or port incorrectly formatted: %s", address)
	}
	return address, nil
}

// GetHostPort does some validation on the address and splits it
// into host and port, respectively
func GetHostPort() (string, string, error) {
	address, err := GetAddress()
	if err != nil {
		return "", "", err
	}

	addressSlice := strings.Split(address, ":")

	host := addressSlice[0]
	port := addressSlice[1]
	return host, port, nil
}

type connectOptions struct{}

// A ConnOpt is a connection option that can be passed to the
// Connection function.
type ConnOpt func(c *connectOptions)

func defaultConnectOptions() *connectOptions {
	return &connectOptions{}
}

func Disconnect() {
	if persistentConnection != nil {
		_ = persistentConnection.Close()
		persistentConnection = nil
	}
}

// DSClient multiplexes the various clients that the
// deployment-service can serve.
type DSClient struct {
	api.DeployClientStreamer
	api.CertificateAuthorityClient
}

func NewDSClient(c *grpc.ClientConn) *DSClient {
	return &DSClient{
		api.NewDeployClientStreamer(c),
		api.NewCertificateAuthorityClient(c),
	}
}

// Connection creates (if necessary) a gRPC connection to the
// deployment service, saves it, and returns it.
func Connection(connectTimeout time.Duration, opts ...ConnOpt) (*DSClient, error) {
	var connection *grpc.ClientConn

	connOptions := defaultConnectOptions()
	for _, o := range opts {
		o(connOptions)
	}

	// NB: this method is currently used by the health check so if you remove the
	// Ping call or do anything to make it less useful for that purpose, please
	// update the health check with a suitable replacement.
	if persistentConnection == nil {
		address, err := GetAddress()
		if err != nil {
			return nil, err
		}
		c := certs.TLSConfig{
			CertPath:       constants.CertPath,
			KeyPath:        constants.KeyPath,
			RootCACertPath: constants.RootCertPath,
		}

		certData, err := c.ReadCerts()
		if err != nil {
			return nil, status.Wrap(err, status.FileAccessError, "Failed to read deployment-service TLS certificates")
		}
		ctx, cancel := context.WithTimeout(context.Background(), connectTimeout)
		defer cancel()
		connFactory := secureconn.NewFactory(*certData)
		connection, err = connFactory.DialContext(
			ctx,
			"deployment-service",
			address,
			grpc.WithBlock())
		if err != nil {
			return nil, status.Wrapf(err,
				status.DeploymentServiceUnreachableError,
				"could not connect to server at address %s",
				address,
			)
		}

		persistentConnection = NewDSClient(connection)
		_, err = persistentConnection.Ping(context.Background(), &api.PingRequest{})
		if err != nil {
			return nil, status.Wrapf(err,
				status.DeploymentServiceCallError,
				"could not ping server at address %s",
				address,
			)
		}
	}

	return persistentConnection, nil
}
