package migrations

import (
	"github.com/chef/automate/components/infra-proxy-service/migrations/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/service"
)

type MigrationServer struct {
	service          *service.Service
	phaseOnePipeline pipeline.PhaseOnePipleine
	phaseTwoPipeline pipeline.PhaseTwoPipleine
}

// NewMigrationServer returns an infra-proxy migration server
func NewMigrationServer(service *service.Service) *MigrationServer {
	phaseOnePipeline := pipeline.SetupPhaseOnePipeline()
	phaseTwoPipeline := pipeline.SetupPhaseTwoPipeline()

	return &MigrationServer{
		service:          service,
		phaseOnePipeline: phaseOnePipeline,
		phaseTwoPipeline: phaseTwoPipeline,
	}
}
