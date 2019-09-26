package service

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/components/teams-service/storage/memstore"
	"github.com/chef/automate/components/teams-service/storage/postgres"
	"github.com/chef/automate/components/teams-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
)

// Service holds the internal state and configuration of the Teams service.
type Service struct {
	Logger      logger.Logger
	ConnFactory *secureconn.Factory
	Storage     storage.Storage
	AuthzClient authz.SubjectPurgeClient
}

// NewInMemoryService returns an instance of Service that uses the memstore storage backend.
func NewInMemoryService(l logger.Logger, connFactory *secureconn.Factory,
	authzClient authz.SubjectPurgeClient) (*Service, error) {

	m, err := memstore.New(context.Background(), l)
	if err != nil {
		return nil, err
	}

	return &Service{
		AuthzClient: authzClient,
		Logger:      l,
		ConnFactory: connFactory,
		Storage:     m,
	}, nil
}

// NewPostgresService returns an instance of Service that connects to a postgres storage backend.
func NewPostgresService(l logger.Logger, connFactory *secureconn.Factory, migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config, authzSubjectClient authz.SubjectPurgeClient,
	authzV2PoliciesClient authz_v2.PoliciesClient, authzV2AuthorizationClient authz_v2.AuthorizationClient) (*Service, error) {

	resp, err := authzV2PoliciesClient.GetPolicyVersion(context.Background(), &authz_v2.GetPolicyVersionReq{})
	if err != nil {
		return nil, err
	}

	p, err := postgres.New(l, migrationsConfig, dataMigrationsConfig, resp.Version.Major == authz_v2.Version_V2, authzV2AuthorizationClient)
	if err != nil {
		return nil, err
	}

	return &Service{
		AuthzClient: authzSubjectClient,
		Logger:      l,
		ConnFactory: connFactory,
		Storage:     p,
	}, nil
}

// ParseStorageError parses common storage errors into a user-readable format.
func ParseStorageError(err error, id interface{}, noun string) error {
	if err != nil {
		switch err {
		case storage.ErrNotFound:
			return status.Errorf(codes.NotFound, "no %s found with id %q", noun, id)
		case storage.ErrConflict:
			return status.Errorf(codes.AlreadyExists, "%s with that name already exists", noun)
		case storage.ErrCannotDelete:
			return status.Errorf(codes.InvalidArgument,
				"%s with id '%s' is marked as un-deletable", noun, id)
		default:
			return status.Error(codes.Internal, err.Error())
		}
	}
	return nil
}
