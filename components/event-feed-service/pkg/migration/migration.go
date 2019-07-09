package migration

import (
	"context"
	"time"

	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	log "github.com/sirupsen/logrus"
)

var (
	eventFeedZeroIndexName  = "eventfeed-0-feeds"
	complianceFeedIndexName = "comp-2-feeds"
)

// Migrator migration state struct
type Migrator struct {
	feedStore persistence.FeedStore
	ctx       context.Context
}

// New create a new MigrationState
func New(ctx context.Context, feedStore persistence.FeedStore) *Migrator {
	return &Migrator{
		feedStore: feedStore,
		ctx:       ctx,
	}
}

// InitializeStore migrate or newly initialize the data store
func (migrator *Migrator) InitializeStore() error {
	err := migrator.removeZeroIndexIfNeeded()
	if err != nil {
		return err
	}

	migrated, err := migrator.complianceMigrationIfNeeded()
	if err != nil {
		return err
	}
	if migrated {
		return nil
	}

	return migrator.feedStore.InitializeStore(migrator.ctx)
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

func (migrator *Migrator) complianceMigrationIfNeeded() (bool, error) {
	hasComplianceFeedData, err := migrator.hasComplianceFeedData()
	if err != nil {
		return false, err
	}
	if hasComplianceFeedData {
		err = migrator.migrateComplianceFeedToCurrent()
		return true, err
	}

	return false, nil
}

func (migrator *Migrator) hasComplianceFeedData() (bool, error) {
	return migrator.feedStore.DoesIndexExists(migrator.ctx, complianceFeedIndexName)
}

func (migrator *Migrator) migrateComplianceFeedToCurrent() error {
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
