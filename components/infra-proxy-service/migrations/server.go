package migrations

import (
	"github.com/chef/automate/components/infra-proxy-service/migrations/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/service"
)

type MigrationServer struct {
	service          *service.Service
	phaseOnePipeline pipeline.PhaseOnePipeline
	phaseTwoPipeline pipeline.PhaseTwoPipeline
}

// NewMigrationServer returns an infra-proxy migration server
func NewMigrationServer(service *service.Service) *MigrationServer {
	phaseOnePipeline := pipeline.SetupPhaseOnePipeline(service)
	phaseTwoPipeline := pipeline.SetupPhaseTwoPipeline(service)

	return &MigrationServer{
		service:          service,
		phaseOnePipeline: phaseOnePipeline,
		phaseTwoPipeline: phaseTwoPipeline,
	}
}
