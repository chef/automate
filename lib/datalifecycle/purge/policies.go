package purge

import (
	"context"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	es "github.com/chef/automate/api/interservice/es_sidecar"
)

// Policies represent the purge policies that are persisted in the workflow
// parameters.
type Policies struct {
	Es map[string]EsPolicy `json:"es"`
	Pg map[string]PgPolicy `json:"pg"`
}

func NewPolicies() *Policies {
	return &Policies{
		Es: map[string]EsPolicy{},
		Pg: map[string]PgPolicy{},
	}
}

// EsPolicy represents an elasticsearch purge policy
type EsPolicy struct {
	Name          string `json:"name"`
	IndexName     string `json:"index_name"`
	OlderThanDays int32  `json:"older_than_days"`
	// If the custom purge field is set we'll delete via document instead of
	// timeseries index.
	CustomPurgeField string `json:"custom_purge_field"`
	Disabled         bool   `json:"disabled"`
}

// Purge purges based on the policy
func (p EsPolicy) Purge(ctx context.Context, esSidecarClient es.EsSidecarClient) error {
	var (
		err    error
		id     = time.Now().UTC().Format(time.RFC3339)
		res    *es.PurgeResponse
		req    = &es.PurgeRequest{}
		logctx = log.WithFields(log.Fields{
			"id":              id,
			"older_than_days": p.OlderThanDays,
			"index_name":      p.IndexName,
		})
	)

	if p.Disabled {
		logctx.Debug("Skipping purge because policy is disabled")
		return nil
	}

	req.Id = id
	req.Index = p.IndexName
	req.OlderThanDays = p.OlderThanDays

	if req.CustomPurgeField != "" {
		logctx = logctx.WithField("custom_purge_field", p.CustomPurgeField)
		req.CustomPurgeField = p.CustomPurgeField
	}

	logctx.Debug("Purging")
	if req.CustomPurgeField == "" {
		// We add 1 one here because otherwise we would delete an
		// index that included documents newer than olderThanDays.
		req.OlderThanDays = req.OlderThanDays + 1
		res, err = esSidecarClient.PurgeTimeSeriesIndicesByAge(ctx, req)
	} else {
		res, err = esSidecarClient.PurgeDocumentsFromIndexByAge(ctx, req)
	}

	if err != nil {
		logctx.WithError(err).Error("Failed to purge elaticsearch data")
		return err
	}

	if res.GetSuccess() {
		logctx.Debug(res.GetMessage())
		return nil
	}

	logctx.WithField("failures", res.GetFailures()).Error(res.GetMessage())
	return errors.New(res.GetMessage())
}

// NOTE: PgPolicies are just a shim until they are actually required by a service.
// Policy represents a postgres purge policy
type PgPolicy struct {
	Name     string `json:"name"`
	Disabled bool   `json:"disabled"`
}

// Purge is where we'll do the purging
func (p PgPolicy) Purge(ctx context.Context) error {
	return nil
}
