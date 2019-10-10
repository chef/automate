package purge

import (
	"context"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
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
	Name string `json:"name"`
	// IndexNames is a comma separated list of index names (or bases if custom purge field is not provided)
	// DO NOT CHANGE THE JSON NAME, YOU'LL PROBABLY BREAK THINGS. It preserves backwards compatibility
	IndexNames    string `json:"index_name"`
	OlderThanDays int32  `json:"older_than_days"`
	// If the custom purge field is set we'll delete via document instead of
	// timeseries index.
	CustomPurgeField string `json:"custom_purge_field"`
	Disabled         bool   `json:"disabled"`
}

// Purge purges based on the policy
func (p EsPolicy) Purge(ctx context.Context, esSidecarClient es.EsSidecarClient) error {
	logctx := log.WithFields(log.Fields{
		"older_than_days": p.OlderThanDays,
		"index_names":     p.IndexNames,
	})

	if p.Disabled {
		logctx.Debug("Skipping purge because policy is disabled")
		return nil
	}

	indexNames := strings.Split(p.IndexNames, ",")

	failures := []string{}
	for _, indexName := range indexNames {
		id := time.Now().UTC().Format(time.RFC3339)
		logctx := logctx.WithFields(logrus.Fields{
			"id":    id,
			"index": indexName,
		})
		req := &es.PurgeRequest{
			Id:               id,
			Index:            indexName,
			OlderThanDays:    p.OlderThanDays,
			CustomPurgeField: p.CustomPurgeField,
		}

		var err error
		var res *es.PurgeResponse
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
			failures = append(failures, err.Error())
		} else {
			if res.GetSuccess() {
				logctx.Debug(res.GetMessage())
			} else {
				logctx.WithField("failures", res.GetFailures()).Error(res.GetMessage())
				failures = append(failures, res.GetMessage())
			}
		}
	}

	if len(failures) <= 0 {
		return nil
	}
	return errors.New(strings.Join(failures, "\n"))
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
