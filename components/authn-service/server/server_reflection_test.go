package server_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
	"go.uber.org/zap"

	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authn-service/server"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
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

	authzCerts := helpers.LoadDevCerts(t, "authz-service")
	authzConnFactory := secureconn.NewFactory(*authzCerts)
	grpcAuthz := authzConnFactory.NewServer()
	authzServer := grpctest.NewServer(grpcAuthz)
	authzConn, err := authzConnFactory.Dial("authz-service", authzServer.URL)
	require.NoError(t, err)
	authzV2Client := authz_v2.NewAuthorizationClient(authzConn)

	serv, err := server.NewServer(context.Background(), config, authzV2Client)
	require.NoError(t, err)

	grpctest.AssertReflection(t, serv.NewGRPCServer(nil, nil))
}
