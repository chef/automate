package profiles

import (
	"github.com/sirupsen/logrus"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/api/status"
	statusserver "github.com/chef/automate/components/compliance-service/api/status/server"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	dbstore "github.com/chef/automate/components/compliance-service/profiles/db"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

// New creates a new server
func New(db *pgdb.DB, es *relaxting.ES2Backend, profiles *config.Profiles,
	eventsClient automate_event.EventServiceClient, statusSrv *statusserver.Server) *PGProfileServer {
	srv := &PGProfileServer{
		profiles:     profiles,
		es:           es,
		store:        &dbstore.Store{DB: db},
		eventsClient: eventsClient,
	}

	// TODO: unbundle object creation from service bootup sanity check

	statusserver.AddMigrationUpdate(statusSrv, status.MigrationLabelPRO, "Ensuring Market profiles are up-to-date...")
	// ensure all market profiles are up to date
	err := srv.store.LoadMarketProfiles(profiles.MarketPath)
	if err != nil {
		logrus.Errorf("could not ensure all market profiles are up to date: %v", err)
	}

	// ensure all profile json's are stored in elastic
	err = srv.rebuildElasticProfileCache()
	if err != nil {
		logrus.Errorf("could not ensure elastic profile cache is up-to-date: %v", err)
	}

	statusserver.AddMigrationUpdate(statusSrv, status.MigrationLabelPRO, "Migrating profiles from disk, if any...")
	// migrate a1 user profiles profiles
	go func() {
		err = srv.migrateDiskProfiles()
		if err != nil {
			logrus.Errorf("could not migrate disk profiles: %v", err)
			statusserver.AddMigrationUpdate(statusSrv, status.MigrationLabelPRO, status.MigrationFailedMsg)
			return
		}

		statusserver.AddMigrationUpdate(statusSrv, status.MigrationLabelPRO, status.MigrationCompletedMsg)
	}()

	return srv
}
