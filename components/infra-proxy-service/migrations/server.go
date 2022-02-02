package migrations

import (
	"github.com/chef/automate/components/infra-proxy-service/service"
)

type MigrationServer struct {
	service *service.Service
}

// NewMigrationServer returns an infra-proxy migration server
func NewMigrationServer(service *service.Service) *MigrationServer {
	return &MigrationServer{
		service: service,
	}
}
