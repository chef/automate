package server_test

import (
	"context"
	"testing"

	"go.uber.org/zap"

	"github.com/chef/automate/components/local-user-service/server"
	usersMock "github.com/chef/automate/components/local-user-service/users/mock"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestReflectionIsPossible(t *testing.T) {
	serviceCerts := helpers.LoadDevCerts(t, "local-user-service")
	config := server.Config{
		ServiceCerts: serviceCerts,
		Users:        &usersMock.Config{},
	}
	var err error
	config.Logger, err = zap.NewProductionConfig().Build()
	if err != nil {
		t.Fatalf("could not init logger: %s", err)
	}

	serv, err := server.NewServer(context.Background(), config)
	if err != nil {
		t.Fatalf("could not init server: %s", err)
	}

	grpctest.AssertReflection(t, serv.NewGRPCServer())
}
