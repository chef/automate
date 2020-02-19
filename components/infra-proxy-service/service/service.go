package service

import (
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	secrets "github.com/chef/automate/api/external/secrets"
	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
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
	Authz       authz.SubjectPurgeClient
	Secrets     secrets.SecretsServiceClient
}

// Start returns an instance of Service that connects to a postgres storage backend.
func Start(l logger.Logger, migrationsConfig migration.Config, connFactory *secureconn.Factory, secretsClient secrets.SecretsServiceClient,
	authzClient authz.SubjectPurgeClient, authzV2PoliciesClient authz_v2.PoliciesClient, authzV2AuthorizationClient authz_v2.AuthorizationClient) (*Service, error) {
	p, err := postgres.New(l, migrationsConfig, authzV2AuthorizationClient)
	if err != nil {
		return nil, err
	}

	return &Service{
		Logger:      l,
		ConnFactory: connFactory,
		Storage:     p,
		Authz:       authzClient,
		Secrets:     secretsClient,
	}, nil
}

// ParseStorageError parses common storage errors into a user-readable format.
func ParseStorageError(err error, id interface{}, noun string) error {
	if err != nil {
		switch err {
		case storage.ErrNotFound:
			return status.Errorf(codes.NotFound, "no %s found with id %q", noun, id)
		case storage.ErrConflict:
			return status.Errorf(codes.AlreadyExists, "%s with that name %q already exists", noun, id)
		default:
			return status.Error(codes.Internal, err.Error())
		}
	}
	return nil
}
