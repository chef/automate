package service

import (
	"reflect"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
)

// Service holds the internal state and configuration of the Infra proxy service.
type Service struct {
	Logger      logger.Logger
	ConnFactory *secureconn.Factory
	Storage     storage.Storage
	Secrets     secrets.SecretsServiceClient
}

// Start returns an instance of Service that connects to a postgres storage backend.
func Start(l logger.Logger, migrationsConfig migration.Config, connFactory *secureconn.Factory, secretsClient secrets.SecretsServiceClient,
	authzClient authz.AuthorizationClient) (*Service, error) {
	p, err := postgres.New(l, migrationsConfig, authzClient)
	if err != nil {
		return nil, err
	}

	return &Service{
		Logger:      l,
		ConnFactory: connFactory,
		Storage:     p,
		Secrets:     secretsClient,
	}, nil
}

// ParseStorageError parses common storage errors into a user-readable format.
func ParseStorageError(err error, v interface{}, noun string) error {
	if err != nil {
		switch err {
		case storage.ErrNotFound:
			return status.Errorf(codes.NotFound, "no %s found with ID %q", noun, reflect.Indirect(reflect.ValueOf(v)).FieldByName("Id"))
		case storage.ErrCannotDelete:
			return status.Errorf(codes.FailedPrecondition, "cannot delete server %q because it still has organizations attached", reflect.Indirect(reflect.ValueOf(v)).FieldByName("Id"))
		case storage.ErrConflict:
			return status.Errorf(codes.AlreadyExists, "%s with ID %q already exists", noun, reflect.Indirect(reflect.ValueOf(v)).FieldByName("Id"))
		case storage.ErrForeignKeyViolation:
			return status.Errorf(codes.NotFound, "no server found with ID %q", reflect.Indirect(reflect.ValueOf(v)).FieldByName("ServerId"))
		default:
			return status.Error(codes.Internal, err.Error())
		}
	}
	return nil
}
