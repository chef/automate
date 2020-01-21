package server

import (
	"context"
	"time"

	"github.com/pkg/errors"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/components/ingest-service/serveropts"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/datalifecycle/purge"
)

var (
	PurgeWorkflowName = cereal.NewWorkflowName("purge_timeseries")
)

const (
	PurgeScheduleName         = "periodic_purge_timeseries"
	PurgeConvergeHistoryName  = "converge-history"
	PurgeConvergeHistoryIndex = "converge-history"
	PurgeActionsName          = "actions"
	PurgeActionsIndex         = "actions"
)

var DefaultPurgePolicies = &purge.Policies{
	Es: map[string]purge.EsPolicy{
		PurgeConvergeHistoryName: {
			Name:          PurgeConvergeHistoryName,
			IndexName:     PurgeConvergeHistoryIndex,
			OlderThanDays: 30,
		},
		PurgeActionsName: {
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
	// new default policies are added and/or existing policies indices are
	// updated in case they have been migrated.

	cp := DefaultPurgePolicies.Es[PurgeConvergeHistoryName]
	cp.OlderThanDays = opts.PurgeConvergeHistoryAfterDays
	if opts.PurgeConvergeHistoryAfterDays < 0 {
		cp.Disabled = true
	}
	DefaultPurgePolicies.Es[PurgeConvergeHistoryName] = cp

	ap := DefaultPurgePolicies.Es[PurgeActionsName]
	ap.OlderThanDays = opts.PurgeActionsAfterDays
	if opts.PurgeActionsAfterDays < 0 {
		ap.Disabled = true
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
		PurgeWorkflowName,
		DefaultPurgePolicies,
		true,
		r,
	)
}
