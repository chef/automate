// +build integration

package integration

import (
	"context"
	"database/sql"
	"os"
	"testing"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/cereal-service/pkg/server"
	"github.com/chef/automate/lib/cereal"

	grpccereal "github.com/chef/automate/api/interservice/cereal"
	libgrpc "github.com/chef/automate/lib/cereal/grpc"
	cerealintegration "github.com/chef/automate/lib/cereal/integration"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/platform/pg"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
	"github.com/stretchr/testify/suite"
	"google.golang.org/grpc"
)

const (
	defaultTestDatabaseName  = "cereal_test"
	defaultAdminDatabaseName = "template1"
)

var defaultA2ConnInfo = pg.A2ConnInfo{
	Host:  "localhost",
	Port:  5432,
	User:  "automate",
	Certs: pg.A2SuperuserCerts,
}

func adminDBURL() string {
	if os.Getenv("PG_ADMIN_URL") != "" {
		return os.Getenv("PG_ADMIN_URL")
	}
	return defaultA2ConnInfo.ConnURI(defaultAdminDatabaseName)
}

func testDBURL() string {
	if os.Getenv("PG_URL") != "" {
		return os.Getenv("PG_URL")
	}
	return defaultA2ConnInfo.ConnURI(defaultTestDatabaseName)
}

func runResetDB() error {
	db, err := sql.Open("postgres", adminDBURL())
	if err != nil {
		return errors.Wrap(err, "could not initialize db connection")
	}
	defer db.Close()
	_, err = db.Exec(pg.DropDatabaseQuery(defaultTestDatabaseName))
	if err != nil {
		return errors.Wrap(err, "could not drop database")
	}
	_, err = db.Exec(pg.CreateDatabaseQuery(defaultTestDatabaseName))
	if err != nil {
		return errors.Wrap(err, "could not create database")
	}
	return nil
}

func TestGrpcPostgres(t *testing.T) {
	ctx := context.Background()
	logrus.SetLevel(logrus.DebugLevel)
	require.NoError(t, runResetDB())
	pgBackend := postgres.NewPostgresBackend(testDBURL(), postgres.WithTaskPingInterval(3*time.Second))
	require.NoError(t, pgBackend.Init())

	grpcServer := grpc.NewServer()
	svc := server.NewCerealService(ctx, pgBackend)
	grpccereal.RegisterCerealServer(grpcServer, svc)
	g := grpctest.NewServer(grpcServer)
	cereal.MaxWakeupInterval = 2 * time.Second

	defer g.Close()

	conn, err := grpc.Dial(g.URL, grpc.WithInsecure(), grpc.WithMaxMsgSize(64*1024*1024))
	if err != nil {
		panic(err)
	}
	grpcBackend := libgrpc.NewGrpcBackendFromConn(conn)
	s := cerealintegration.NewSuiteForBackend(ctx, t, grpcBackend)
	suite.Run(t, s)
	require.NoError(t, grpcBackend.Close())
}
