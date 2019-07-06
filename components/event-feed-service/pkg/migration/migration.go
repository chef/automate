package migration

import (
	"context"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
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
	migrated, err := migrator.complianceMigrationIfNeeded()
	if err != nil {
		return err
	}
	if migrated {
		return nil
	}

	return migrator.feedStore.InitializeStore(migrator.ctx)
}

func (migrator *Migrator) complianceMigrationIfNeeded() (bool, error) {

	// check if we need to migrate the data

	// migrate the data

	return false, nil
}
