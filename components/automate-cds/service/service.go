package service

import (
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
)

// Service holds the internal state and configuration of the Automate CDS service.
type Service struct {
	Logger      logger.Logger
	ConnFactory *secureconn.Factory
}

// Start returns an instance of Service
func Start(l logger.Logger, connFactory *secureconn.Factory) (*Service, error) {

	return &Service{
		Logger:      l,
		ConnFactory: connFactory,
	}, nil
}
