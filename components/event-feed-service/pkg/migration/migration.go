package migration

import (
	"context"
	"time"

	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	log "github.com/sirupsen/logrus"
)

var (
	eventFeedZeroIndexName    = "eventfeed-0-feeds"
	eventFeedOneIndexName     = "eventfeed-1-feeds"
	compliance1FeedsIndexName = "comp-1-feeds"
	compliance2FeedsIndexName = "comp-2-feeds"
)

// Migrator migration state struct
type Migrator struct {
	feedStore  persistence.FeedStore
	ctx        context.Context
	migrations []migrationVersion
}

type migrationVersion struct {
	migratedIfNeeded func() (bool, error)
}

// New create a new MigrationState
func New(ctx context.Context, feedStore persistence.FeedStore) *Migrator {
	migrator := &Migrator{
		feedStore: feedStore,
		ctx:       ctx,
	}
	migrator.migrations = migrator.getMigrations()

	return migrator
}

// InitializeStore migrate or newly initialize the data store
func (migrator *Migrator) InitializeStore() error {
	err := migrator.removeZeroIndexIfNeeded()
	if err != nil {
		return err
	}

	for _, mv := range migrator.migrations {
		migrated, err := mv.migratedIfNeeded()
		if err != nil {
			return err
		}
		if migrated {
			return nil
		}
	}

	return migrator.feedStore.InitializeStore(migrator.ctx)
}

func (migrator *Migrator) getMigrations() []migrationVersion {
	return []migrationVersion{
		{
			migratedIfNeeded: func() (bool, error) {
				return migrator.complianceMigrationIfNeeded(compliance1FeedsIndexName)
			},
		},
		{
			migratedIfNeeded: func() (bool, error) {
				return migrator.complianceMigrationIfNeeded(compliance2FeedsIndexName)
			},
		},
		{
			migratedIfNeeded: func() (bool, error) {
				return migrator.complianceMigrationIfNeeded(eventFeedOneIndexName)
			},
		},
	}
}

func (migrator *Migrator) removeZeroIndexIfNeeded() error {
	hasZeroIndex, err := migrator.feedStore.DoesIndexExists(migrator.ctx, eventFeedZeroIndexName)
	if err != nil {
		return err
	}

	if hasZeroIndex {
		log.Infof("Deleting %s index", eventFeedZeroIndexName)
		err = migrator.feedStore.DeleteIndex(migrator.ctx, eventFeedZeroIndexName)
		if err != nil {
			return err
		}
	}

	return nil
}

func (migrator *Migrator) complianceMigrationIfNeeded(complianceFeedIndexName string) (bool, error) {
	hasComplianceFeedData, err := migrator.feedStore.DoesIndexExists(migrator.ctx, complianceFeedIndexName)
	if err != nil {
		return false, err
	}
	if hasComplianceFeedData {
		err = migrator.migrateComplianceFeedToCurrent(complianceFeedIndexName)
		return true, err
	}

	return false, nil
}

func (migrator *Migrator) migrateComplianceFeedToCurrent(complianceFeedIndexName string) error {
	err := migrator.feedStore.InitializeStore(migrator.ctx)
	if err != nil {
		return err
	}

	log.Infof("Reindexing %s index to current", complianceFeedIndexName)
	reindexTaskID, err := migrator.feedStore.ReindexFeedsToLatest(migrator.ctx, complianceFeedIndexName)
	if err != nil {
		return err
	}

	// Wait for Reindex task to complete
	for {
		time.Sleep(time.Millisecond * 500)

		status, err := migrator.feedStore.JobStatus(migrator.ctx, reindexTaskID)
		if err != nil {
			return err
		}

		if status.Completed {
			log.Infof("Reindexing is %f complete", status.PercentageComplete)
			break
		}
	}

	log.Infof("Deleting %s index", complianceFeedIndexName)
	err = migrator.feedStore.DeleteIndex(migrator.ctx, complianceFeedIndexName)
	if err != nil {
		return err
	}

	log.Info("Migration from compliance-service finished successfully")
	return nil
}
