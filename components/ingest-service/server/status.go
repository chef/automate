//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package server

import (
	"time"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/migration"
	"github.com/chef/automate/components/ingest-service/pipeline/processor"
	log "github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

type IngestStatus struct {
	migrationStatus  *migration.Status
	chefIngestServer *ChefIngestServer
	client           backend.Client
}

// NewIngestStatus creates a new status server instance
func NewIngestStatus(client backend.Client, A1migration *migration.Status) *IngestStatus {
	return &IngestStatus{migrationStatus: A1migration, client: client}
}

func (is *IngestStatus) SetChefIngestServer(s *ChefIngestServer) {
	is.chefIngestServer = s
}

// GetMetrics returns the metrics of the ingest service
func (is *IngestStatus) GetMetrics(ctx context.Context, empty *ingest.MetricsRequest) (*ingest.Metrics, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	metrics := &ingest.Metrics{
		Uptime: time.Since(processor.StartedAt).Seconds(),
		Pipeline: &ingest.PipelineMetrics{
			TotalRunMessages:    is.chefIngestServer.TotalChefRunMessages(),
			TotalActionMessages: is.chefIngestServer.TotalChefActionMessages(),
		},
	}

	return metrics, nil
}

// GetHealth returns the health of the ingest service
func (is *IngestStatus) GetHealth(ctx context.Context, empty *ingest.HealthRequest) (*ingest.Health, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	health := &ingest.Health{
		Status: "ok",
	}
	// Verify if we have initialized the data store
	if is.client.Initializing() {
		// This status should match with the health_check hook in habitat
		health.Status = "initialization"
	}
	return health, nil
}

// GetMigrationStatus returns the migration status if there is any migration from A1 to A2
func (is *IngestStatus) GetMigrationStatus(ctx context.Context, empty *ingest.MigrationStatusRequest) (*ingest.MigrationStatus, error) {
	log.WithFields(log.Fields{"func": nameOfFunc()}).Debug("rpc call")

	migrationStatus := &ingest.MigrationStatus{
		Total:     is.migrationStatus.TotalTasks(),
		Completed: is.migrationStatus.CompletedTasks(),
		Status:    is.migrationStatus.String(),
		Finished:  is.migrationStatus.Done(),
	}

	return migrationStatus, nil
}
