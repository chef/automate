package service

import (
	"fmt"
	"github.com/chef/automate/api/interservice/local_user"
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
	Logger             logger.Logger
	ConnFactory        *secureconn.Factory
	Storage            storage.Storage
	Migration          storage.MigrationStorage
	Secrets            secrets.SecretsServiceClient
	AuthzProjectClient authz.ProjectsServiceClient
	LocalUser          local_user.UsersMgmtServiceClient
}

// Start returns an instance of Service that connects to a postgres storage backend.
func Start(l logger.Logger, migrationsConfig migration.Config, connFactory *secureconn.Factory, secretsClient secrets.SecretsServiceClient,
	authzClient authz.AuthorizationServiceClient, authzProjectClient authz.ProjectsServiceClient, localUserClient local_user.UsersMgmtServiceClient) (*Service, error) {
	p, pObj, err := postgres.New(l, migrationsConfig, authzClient)
	if err != nil {
		return nil, err
	}
	return &Service{
		Logger:             l,
		ConnFactory:        connFactory,
		Storage:            p,
		Migration:          pObj,
		Secrets:            secretsClient,
		AuthzProjectClient: authzProjectClient,
		LocalUser:          localUserClient,
	}, nil
}

// ParseStorageError parses common storage errors into a user-readable format.
func ParseStorageError(err error, v interface{}, noun string) error {
	if err != nil {
		switch err {
		case storage.ErrNotFound:
			return status.Errorf(codes.NotFound, "no %s found with ID %q", noun, getFieldValue(v, "Id"))
		case storage.ErrCannotDelete:
			return status.Errorf(codes.FailedPrecondition, "cannot delete server %q because it still has organizations attached", getFieldValue(v, "Id"))
		case storage.ErrConflict:
			return status.Errorf(codes.AlreadyExists, "%s with ID %q already exists", noun, getFieldValue(v, "Id"))
		case storage.ErrForeignKeyViolation:
			return status.Errorf(codes.NotFound, "no server found with ID %q", getFieldValue(v, "ServerId"))
		default:
			return status.Error(codes.Internal, err.Error())
		}
	}
	return nil
}

func getFieldValue(v interface{}, field string) string {
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	case reflect.Struct:
		return rv.FieldByName(field).String()
	default:
		return fmt.Sprintf("%v", rv)
	}
}
