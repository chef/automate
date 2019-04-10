package server_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
	"go.uber.org/zap"

	"github.com/chef/automate/components/authn-service/server"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestReflectionIsPossible(t *testing.T) {
	serviceCerts := helpers.LoadDevCerts(t, "authn-service")
	config := server.Config{
		ServiceCerts: serviceCerts,
	}
	var err error
	config.Logger, err = zap.NewProductionConfig().Build()
	if err != nil {
		t.Fatalf("could not init logger: %s", err)
	}
	serv, err := server.NewServer(context.Background(), config)
	require.NoError(t, err)

	grpctest.AssertReflection(t, serv.NewGRPCServer(nil))
}
