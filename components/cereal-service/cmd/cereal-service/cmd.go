package main

import (
	"fmt"
	"net"
	"os"

	grpcceral "github.com/chef/automate/api/interservice/cereal"
	"github.com/chef/automate/components/cereal-service/pkg/server"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/platform/pg"
	"google.golang.org/grpc"
)

const defaultDatabaseName = "cereal_test"

func defaultConnURIForDatabase(dbname string) string {
	if os.Getenv("PG_URL") != "" {
		return os.Getenv("PG_URL")
	}
	connInfo := pg.A2ConnInfo{
		Host:  "localhost",
		Port:  5432,
		User:  "automate",
		Certs: pg.A2SuperuserCerts,
	}
	return connInfo.ConnURI(dbname)
}

func main() {
	dbName := defaultDatabaseName

	pgBackend := postgres.NewPostgresBackend(defaultConnURIForDatabase(dbName))
	if err := pgBackend.Init(); err != nil {
		panic(err)
	}

	svc := server.NewCerealService(pgBackend)
	grpcServer := grpc.NewServer()
	grpcceral.RegisterCerealServer(grpcServer, svc)
	lis, err := net.Listen("tcp", fmt.Sprintf(":3210"))
	if err != nil {
		panic(err)
	}

	grpcServer.Serve(lis)
}
