package postgres

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	timestamp "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/applications-service/pkg/storage"
)

const (
	labelSuccess = "succeeded"
	labelFailure = "failed"
)

const (
	upsertServiceAndMetadata = `
INSERT INTO service_full (
	origin,
	name,
	version,
	release,
	package_ident,
	health,
	health_check_message,
	channel,
	update_strategy,
	supervisor_id,
	fqdn,
	site,
	service_group_name,
	service_group_name_suffix,
	application,
	environment,
	last_event_occurred_at,
	service_group_id
)
VALUES (
  $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18
  )
ON CONFLICT ON CONSTRAINT service_full_name_supervisor_id_key
DO UPDATE SET (
	origin,
	version,
	release,
	package_ident,
	health,
	health_check_message,
	channel,
	update_strategy,
	fqdn,
	site,
	service_group_name,
	service_group_name_suffix,
	application,
	environment,
	last_event_occurred_at,
	service_group_id
) = (
	EXCLUDED.origin,
	EXCLUDED.version,
	EXCLUDED.release,
	EXCLUDED.package_ident,
	EXCLUDED.health,
	EXCLUDED.health_check_message,
	EXCLUDED.channel,
	EXCLUDED.update_strategy,
	EXCLUDED.fqdn,
	EXCLUDED.site,
	EXCLUDED.service_group_name,
	EXCLUDED.service_group_name_suffix,
	EXCLUDED.application,
	EXCLUDED.environment,
	EXCLUDED.last_event_occurred_at,
	EXCLUDED.service_group_id
)
;
`
)

type packageIdent struct {
	Origin  string
	Name    string
	Version string
	Release string
}

func newPackageIdentFromString(ident string) (*packageIdent, error) {
	fields := strings.Split(ident, "/")
	if len(fields) != 4 {
		// package_ident should always have 4 fields: {origin}/{name}/{version}/{release}
		return nil, errors.Errorf(
			"malformed package_ident. (expected: 'origin/name/version/release') (actual: '%s')",
			ident,
		)
	}

	return &packageIdent{fields[0], fields[1], fields[2], fields[3]}, nil
}

func (p packageIdent) FullPackageIdent() string {
	return fmt.Sprintf("%s/%s/%s/%s", p.Origin, p.Name, p.Version, p.Release)
}

var (
	opsInFlight = promauto.NewGauge(prometheus.GaugeOpts{
		Name: "applications_ingest_ops_inflight",
		Help: "The number of applications events currently being ingested",
	})

	timeLastEventSeen = float64(time.Now().Unix())

	_ = promauto.NewCounterFunc(prometheus.CounterOpts{
		Name: "applications_ingest_ops_last_time",
		Help: "Time when the last applications message was accepted for ingestion",
	},
		func() float64 { return timeLastEventSeen },
	)

	ingestDurations = promauto.NewHistogramVec(prometheus.HistogramOpts{
		Name: "applications_ingest_ops_duration",
		Help: "Application events ingestion duration distributions",
		// LinearBuckets(start, width float64, count int)
		// so 0.1s * 20 = 2s
		Buckets: prometheus.LinearBuckets(0, 0.1, 20),
	},
		[]string{"result"},
	)
)

// process a habitat HealthCheck event and store it into the database
func (db *Postgres) IngestHealthCheckEvent(event *habitat.HealthCheckEvent) error {
	opsInFlight.Inc()
	defer opsInFlight.Dec()

	processingStart := time.Now()
	timeLastEventSeen = float64(processingStart.Unix())

	var (
		err      = db.IngestHealthCheckEventWithoutMetrics(event)
		duration = time.Since(processingStart)
		label    = labelSuccess
	)
	if err != nil {
		label = labelFailure
		log.WithError(err).Error("ingest failed")
	}

	ingestDurations.With(prometheus.Labels{
		"result": label,
	}).Observe(duration.Seconds())

	return err
}

// same as IngestHealthCheckEvent but without metrics
func (db *Postgres) IngestHealthCheckEventWithoutMetrics(event *habitat.HealthCheckEvent) error {
	log.WithFields(log.Fields{
		"storage": "postgres",
		"message": event,
	}).Debug("Processing event in storage")

	// @afiune For our first iteration we will process the messages in the storage
	// side but in the future we need to move all this logic into an ingestion
	// pipeline. I'll try to create functions that can be easily moved.

	eventMetadata := event.GetEventMetadata()
	if eventMetadata == nil {
		return errors.New("missing event metadata")
	}

	svcMetadata := event.GetServiceMetadata()
	if svcMetadata == nil {
		return errors.New("missing service metadata")
	}

	pkgIdent, err := newPackageIdentFromString(svcMetadata.GetPackageIdent())
	if err != nil {
		return err
	}

	// @afiune all our backend was designed for the health check to be all
	// uppercases but habitat is actually sending case sensitive strings
	eventHealth := strings.ToUpper(event.GetResult().String())

	err = db.upsertServiceAndMetadata(
		eventMetadata,
		svcMetadata,
		pkgIdent,
		eventHealth,
	)
	// upsert should handle races and such so we don't have to retry to handle
	// unique constraint problems, thus an error here is something we don't know how to handle.
	if err != nil {
		log.WithError(err).Error("failed to upsert healthcheck data")
	}

	return err
}

func (db *Postgres) upsertServiceAndMetadata(
	eventMetadata *habitat.EventMetadata,
	svcMetadata *habitat.ServiceMetadata,
	pkgIdent *packageIdent,
	health string) error {

	var (
		channel        string
		updateStrategy string
	)

	if svcMetadata.GetUpdateConfig() == nil {
		channel = ""
		updateStrategy = storage.NoneStrategy.String()
	} else {
		channel = svcMetadata.GetUpdateConfig().Channel
		updateStrategy = storage.HabitatUpdateStrategyToStorageFormat(svcMetadata.UpdateConfig.GetStrategy()).String()
	}

	serviceGroupUniques := fmt.Sprintf("%s:%s:%s", svcMetadata.GetServiceGroup(), eventMetadata.GetApplication(), eventMetadata.GetEnvironment())
	sgIdBytes := sha256.Sum256([]byte(serviceGroupUniques))
	sgId := hex.EncodeToString(sgIdBytes[:])

	_, err := db.DbMap.Exec(upsertServiceAndMetadata,
		pkgIdent.Origin,                 // origin,
		pkgIdent.Name,                   // name,
		pkgIdent.Version,                // version,
		pkgIdent.Release,                // release,
		pkgIdent.FullPackageIdent(),     // package_ident,
		health,                          // health,
		"",                              // health_check_message, // TODO 13Aug2019 - hab isn't sending this yet
		channel,                         // channel,
		updateStrategy,                  // update_strategy,
		eventMetadata.GetSupervisorId(), // supervisor_id,
		eventMetadata.GetFqdn(),         // fqdn,
		eventMetadata.GetSite(),         // site,
		svcMetadata.GetServiceGroup(),   // service_group_name,
		trimSuffix(svcMetadata.GetServiceGroup()),            // service_group_name_suffix,
		eventMetadata.GetApplication(),                       // application,
		eventMetadata.GetEnvironment(),                       // environment
		convertOrCreateGoTime(eventMetadata.GetOccurredAt()), // last_event_occurred_at
		sgId, // service_group_id
	)

	return err
}

// convert a proto timestamp to native go time,
// on any error return the current time (now)
func convertOrCreateGoTime(t *timestamp.Timestamp) time.Time {
	goTime, err := ptypes.Timestamp(t)
	if err != nil {
		log.WithError(err).Error("malformed protobuf timestamp, using time now")
		return time.Now()
	}
	return goTime
}

func trimSuffix(name string) string {
	parts := strings.Split(name, ".")
	return parts[1]
}
