package grpc

import (
	"fmt"
	"io/ioutil"
	"net"
	"strings"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/secrets-service/config"
	"github.com/chef/automate/components/secrets-service/dao"
	"github.com/chef/automate/components/secrets-service/server"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// Spawn starts a grpc server using the provided host and port.
func Spawn(config *config.Secrets, connFactory *secureconn.Factory) error {

	secretsKey, err := getSecretsKey(&config.SecretsKey)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("Finding secrets key")
		return err
	}

	db, err := dao.New(&config.Postgres, secretsKey)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("Creating postgres connection")
		return err
	}

	uri := fmt.Sprintf("%s:%d", config.Service.Host, config.Service.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting secrets-service gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("TCP listen failed")
		return err
	}

	grpcServer := NewGRPCServer(db, connFactory)

	return grpcServer.Serve(conn)
}

func getSecretsKey(secretsKeyConf *config.SecretsKey) (string, error) {
	if secretsKeyConf.File != "" {
		data, err := ioutil.ReadFile(secretsKeyConf.File)
		if err != nil {
			return "", errors.Wrapf(err, "failed to read secrets key from file %s", secretsKeyConf.File)
		}
		strippedData := strings.TrimSpace(string(data))

		return strippedData, nil
	} else if secretsKeyConf.Key == "" {
		return "", errors.Errorf("failed to find the secrets key")
	}

	return secretsKeyConf.Key, nil
}

// NewGRPCServer returns a server that provides our services: secrets
// and health.
func NewGRPCServer(db *dao.DB, connFactory *secureconn.Factory) *grpc.Server {
	grpcServer := connFactory.NewServer()

	secretsServer := server.New(db)
	secrets.RegisterSecretsServiceServer(grpcServer, secretsServer)

	health.RegisterHealthServer(grpcServer, secretsServer.Health())

	reflection.Register(grpcServer)

	return grpcServer
}
