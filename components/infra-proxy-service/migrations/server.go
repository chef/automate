package migrations

import (
	"github.com/chef/automate/components/infra-proxy-service/migrations/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/service"
)

type MigrationServer struct {
	service          *service.Service
	phaseOnePipeline pipeline.PhaseOnePipleine
}

// NewMigrationServer returns an infra-proxy migration server
func NewMigrationServer(service *service.Service) *MigrationServer {
	c := pipeline.SetupPhaseOnePipeline()
	return &MigrationServer{
		service:          service,
		phaseOnePipeline: c,
	}
}
