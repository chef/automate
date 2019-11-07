package service

import (
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
)

// Service holds the internal state and configuration of the Teams service.
type Service struct {
	Logger      logger.Logger
	ConnFactory *secureconn.Factory
	Storage     storage.Storage
}

// NewPostgresService returns an instance of Service that connects to a postgres storage backend.
func NewPostgresService(l logger.Logger, migrationsConfig migration.Config, connFactory *secureconn.Factory) (*Service, error) {
	p, err := postgres.New(l, migrationsConfig)
	if err != nil {
		return nil, err
	}

	return &Service{
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
		default:
			return status.Error(codes.Internal, err.Error())
		}
	}
	return nil
}
