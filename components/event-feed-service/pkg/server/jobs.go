package server

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/teambition/rrule-go"

	"github.com/chef/automate/components/event-feed-service/pkg/config"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/datalifecycle/purge"
)

const (
	PurgeJobName        = "purge"
	PurgeScheduleName   = "periodic_purge"
	PurgeFeedPolicyName = "feed"
	PurgeFeedIndexName  = persistence.IndexNameFeeds
)

var DefaultPurgePolicies = &purge.Policies{
	Es: map[string]purge.EsPolicy{
		PurgeFeedPolicyName: {
			Name:             PurgeFeedPolicyName,
			IndexName:        PurgeFeedIndexName,
			CustomPurgeField: "pub_timestamp",
		},
	},
}

// ConfigureJobManager migrates purge policies from the server event feed
// config and creates a scheduled workflow if it doesn't exist.
func ConfigureJobManager(man *cereal.Manager, config *config.EventFeed) error {

	// Migrate default policy values from the config. The default policies are
	// only persisted the first time the workflow is created, after which only
	// new default policies are added and/or existing policies indicies are
	// updated in case they have been migrated.
	//
	// When run with deployment-service it had a default retention periods of 7
	// days. The configuration has been deprecated and is no longer valid and
	// the default of 30 has been removed, making the default value 0. If it's
	// set to 0 it should be set at 7, otherwise it should be set at the user
	// defined value at the time of the initial migration.

	p := DefaultPurgePolicies.Es[PurgeFeedPolicyName]
	switch config.DefaultPurgeAfterDays {
	case 0:
		p.OlderThanDays = 30
	default:
		p.OlderThanDays = int32(config.DefaultPurgeAfterDays)
	}
	DefaultPurgePolicies.Es[PurgeFeedPolicyName] = p

	r, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.DAILY,
		Interval: 1,
		Dtstart:  time.Now().UTC(),
	})
	if err != nil {
		return errors.Wrap(err, "could not create recurrence rule for job configuration")
	}

	return purge.CreateOrUpdatePurgeWorkflow(
		context.Background(),
		man,
		PurgeScheduleName,
		PurgeJobName,
		DefaultPurgePolicies,
		true,
		r,
	)
}
