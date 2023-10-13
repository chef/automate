package profiles

import (
	"github.com/sirupsen/logrus"

	automate_event "github.com/chef/automate/api/interservice/event"
	statusserver "github.com/chef/automate/components/compliance-service/api/status/server"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	dbstore "github.com/chef/automate/components/compliance-service/profiles/db"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

// New creates a new server
func New(db *pgdb.DB, esBackend *relaxting.ES2Backend, esClient *ingestic.ESClient, profiles *config.Profiles,
	eventsClient automate_event.EventServiceClient, statusSrv *statusserver.Server, firejailProfilePath string) *PGProfileServer {

	srv := &PGProfileServer{
		profiles:            profiles,
		es:                  esBackend,
		esClient:            esClient,
		store:               &dbstore.Store{DB: db},
		eventsClient:        eventsClient,
		firejailProfilePath: firejailProfilePath,
	}

	// TODO: unbundle object creation from service bootup sanity check

	statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPRO, "Ensuring Market profiles are up-to-date...")
	// ensure all market profiles are up to date
	err := srv.store.LoadMarketProfiles(profiles.MarketPath, firejailProfilePath)
	if err != nil {
		logrus.Errorf("could not ensure all market profiles are up to date: %v", err)
	}

	// ensure all profile json's are stored in elastic
	err = srv.rebuildElasticProfileCache()
	if err != nil {
		logrus.Errorf("could not ensure elastic profile cache is up-to-date: %v", err)
	}

	statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPRO, "Migrating profiles from disk, if any...")
	// migrate a1 user profiles profiles
	go func() {
		err = srv.migrateDiskProfiles()
		if err != nil {
			logrus.Errorf("could not migrate disk profiles: %v", err)
			statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPRO, statusserver.MigrationFailedMsg)
			return
		}

		statusserver.AddMigrationUpdate(statusSrv, statusserver.MigrationLabelPRO, statusserver.MigrationCompletedMsg)
	}()

	return srv
}
