package integration

import (
	"context"
	"encoding/base64"
	"os"
	"testing"

	"github.com/spf13/viper"
	"github.com/stretchr/testify/require"
	"github.com/stretchr/testify/suite"
	"google.golang.org/grpc"

	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
)

const (
	gatewayBindAddress = "127.0.0.1:2001"
)

type GatewayTestSuite struct {
	ctx     context.Context
	target  *acceptanceTarget
	clients gateway.ClientsFactory
	gwConn  *grpc.ClientConn
	suite.Suite
}

type acceptanceTarget struct {
	Key   string
	Host  string
	Host2 string
	User  string
}

func TestGateway(t *testing.T) {
	s, err := NewGatewayTestSuite(context.Background(), t, getAcceptanceTargetFromEnvironment())
	require.NoError(t, err)
	suite.Run(t, s)
}

func NewGatewayTestSuite(ctx context.Context, t *testing.T, target *acceptanceTarget) (*GatewayTestSuite, error) {
	connFactory := helpers.SecureConnFactoryHabWithDeploymentServiceCerts()
	viper.SetConfigFile("/hab/svc/automate-gateway/config/config.toml")
	err := viper.ReadInConfig()
	if err != nil {
		return nil, err
	}
	cfg, err := gateway.ConfigFromViper()
	clients, err := gateway.NewClientsFactory(cfg.GrpcClients, connFactory)
	if err != nil {
		return nil, err
	}
	gwConn, err := connFactory.Dial("automate-gateway", gatewayBindAddress)
	if err != nil {
		return nil, err
	}

	return &GatewayTestSuite{
		ctx:     ctx,
		target:  target,
		clients: clients,
		gwConn:  gwConn,
	}, nil
}

func getAcceptanceTargetFromEnvironment() *acceptanceTarget {
	return &acceptanceTarget{
		Key:   acceptanceTargetKey(),
		Host:  os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_HOST"),
		Host2: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_HOST2"),
		User:  os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_USER"),
	}
}

func acceptanceTargetKey() string {
	defaultKey := "default_key"

	if encoded := os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_KEY"); encoded != "" {
		key, err := base64.StdEncoding.DecodeString(encoded)
		if err != nil {
			return defaultKey
		}

		return string(key)
	}

	return defaultKey
}
