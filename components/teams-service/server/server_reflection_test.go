package server_test

import (
	"testing"

	"github.com/chef/automate/components/teams-service/server"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestReflectionIsPossible(t *testing.T) {
	l, err := logger.NewLogger("text", "debug")
	if err != nil {
		t.Fatalf("could not init logger")
	}
	serviceCerts := helpers.LoadDevCerts(t, "teams-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	serv, err := service.NewInMemoryService(l, connFactory, nil)
	if err != nil {
		t.Fatalf("init server: %s", err)
	}

	grpctest.AssertReflection(t, server.NewGRPCServer(serv))
}
