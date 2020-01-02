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

var PurgeWorkflowName = cereal.NewWorkflowName("purge")

const (
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
	// new default policies are added and/or existing policies indices are
	// updated in case they have been migrated.

	p := DefaultPurgePolicies.Es[PurgeFeedPolicyName]
	p.OlderThanDays = int32(config.DefaultPurgeAfterDays)
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
		PurgeWorkflowName,
		DefaultPurgePolicies,
		true,
		r,
	)
}
