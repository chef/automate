package service

import (
	"github.com/chef/automate/components/infra-proxy/storage"
	"github.com/chef/automate/components/infra-proxy/storage/postgres"
	"github.com/chef/automate/components/infra-proxy/storage/postgres/migration"
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
func NewPostgresService(l logger.Logger, connFactory *secureconn.Factory, migrationsConfig migration.Config) (*Service, error) {
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
