package server

import (
	"context"
	"time"

	"github.com/chef/automate/components/ingest-service/serveropts"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/datalifecycle/purge"
	"github.com/pkg/errors"
	rrule "github.com/teambition/rrule-go"
)

const (
	PurgeJobName              = "purge_timeseries"
	PurgeScheduleName         = "periodic_purge_timeseries"
	PurgeConvergeHistoryName  = "converge-history"
	PurgeConvergeHistoryIndex = "converge-history"
	PurgeActionsName          = "actions"
	PurgeActionsIndex         = "actions"
)

var DefaultPurgePolicies = &purge.Policies{
	Es: map[string]purge.EsPolicy{
		PurgeConvergeHistoryName: purge.EsPolicy{
			Name:          PurgeConvergeHistoryName,
			IndexName:     PurgeConvergeHistoryIndex,
			OlderThanDays: 30,
		},
		PurgeActionsName: purge.EsPolicy{
			Name:          PurgeActionsName,
			IndexName:     PurgeActionsIndex,
			OlderThanDays: 30,
		},
	},
}

// ConfigurePurge migrates purge policies from the server options
// that are passed at startup and creates a scheduled workflow if it doesn't
// exist.
func ConfigurePurge(man *cereal.Manager, opts *serveropts.Opts) error {
	// Migrate default policy values from the config. The default policies are
	// only persisted the first time the workflow is created, after which only
	// new default policies are added and/or existing policies indicies are
	// updated in case they have been migrated.
	//
	// Ingest defaults to -1 when run outside of the deployment-service, which
	// signaled that it should be disabled. When run with deployment-service it
	// had a default retention periods of 30 days. The configuration has been
	// deprecated and is no longer valid and the default of 30 has been removed,
	// making the default value 0. If we have -1 it should be disabled,
	// if it's set to 0 it should be set at 30, otherwise it should be set at
	// the user defined value at the time of the initial migration.

	cp := DefaultPurgePolicies.Es[PurgeConvergeHistoryName]
	switch opts.PurgeConvergeHistoryAfterDays {
	case -1:
		cp.Disabled = true
	case 0:
		cp.OlderThanDays = 30
	default:
		cp.OlderThanDays = opts.PurgeConvergeHistoryAfterDays
	}
	DefaultPurgePolicies.Es[PurgeConvergeHistoryName] = cp

	ap := DefaultPurgePolicies.Es[PurgeActionsName]
	switch opts.PurgeActionsAfterDays {
	case -1:
		ap.Disabled = true
	case 0:
		ap.OlderThanDays = 30
	default:
		ap.OlderThanDays = opts.PurgeActionsAfterDays
	}
	DefaultPurgePolicies.Es[PurgeActionsName] = ap

	r, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.DAILY,
		Interval: 1,
		Dtstart:  time.Now(),
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
