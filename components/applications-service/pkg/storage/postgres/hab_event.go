package postgres

import (
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/api/external/habitat"
	"github.com/go-gorp/gorp"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"

	dblib "github.com/chef/automate/lib/db"
	timestamp "github.com/golang/protobuf/ptypes/timestamp"
	log "github.com/sirupsen/logrus"
)

const (
	labelSuccess = "succeeded"
	labelFailure = "failed"
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
	}

	ingestDurations.With(prometheus.Labels{
		"result": label,
	}).Observe(duration.Seconds())

	return err
}

// IngestHabEvents process habitat events and store them into the database
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

	// @afiune are we changing the grouping of a service group based of the application
	// and environment names? if not, should we update those fields?
	svc, exist := db.getServiceFromUniqueFields(
		pkgIdent.Name,
		eventMetadata.GetSupervisorId(),
	)

	// If the service already exists, we just do a simple update
	//
	// Fields that needs to be updated:
	// - Package ident (without name. the name of a service can't be changed!)
	// - Update strategy
	// - Health, Application, Environment, Site and Fqdn
	// - Timestamp of last event received (occurred_at)
	if exist {
		// Update Health
		//
		// @afiune all our backend was designed for the health check to be all
		// uppercases but habitat is actually sending case sensitive strings
		newHealth := strings.ToUpper(event.GetResult().String())

		// Verify if the service health changed, if so, save the current health
		// into the previous_health and update it with the new one
		if svc.Health != newHealth {
			svc.PreviousHealth = svc.Health
			svc.Health = strings.ToUpper(event.GetResult().String())
		}

		// Update Package Identifier
		svc.Origin = pkgIdent.Origin
		svc.Version = pkgIdent.Version
		svc.Release = pkgIdent.Release
		svc.FullPkgIdent = pkgIdent.FullPackageIdent()

		// Update Channel
		// TODO @afiune do we want to store the strategy?
		if svcMetadata.GetUpdateConfig() != nil {
			svc.Channel = svcMetadata.UpdateConfig.GetChannel()
		} else {
			svc.Channel = ""
		}

		// Update the timestamp of the last event received
		svc.LastEventOccurredAt = convertOrCreateTimestamp(eventMetadata.GetOccurredAt())

		if _, err := db.DbMap.Update(svc); err != nil {
			return errors.Wrap(err, "unable to update service")
		}
		return nil
	}

	// But if the service doesn't exist, we will handle it as a new service insertion
	return db.insertNewServiceFromHealthCheckEvent(
		eventMetadata,
		svcMetadata,
		pkgIdent,
		strings.ToUpper(event.GetResult().String()),
	)
}

func convertOrCreateTimestamp(t *timestamp.Timestamp) time.Time {
	goTime, err := ptypes.Timestamp(t)
	if err != nil {
		log.WithError(err).Error("malformed protobuf timestamp, using time now")
		return time.Now()
	}
	return goTime
}

// insertNewServiceFromHealthCheckEvent assumes the service to be inserted doesn't exist already,
// this function is wrapped with a transaction func since it could do multiple things
// like insert a supervisor, service_group, deployment and the service itself.
func (db *Postgres) insertNewServiceFromHealthCheckEvent(
	eventMetadata *habitat.EventMetadata,
	svcMetadata *habitat.ServiceMetadata,
	pkgIdent *packageIdent,
	health string) error {

	// TODO Update me!
	return dblib.Transaction(db.DbMap, func(tx *gorp.Transaction) error {

		// 1) Deployment
		did, exist := db.getDeploymentID(eventMetadata.GetApplication(), eventMetadata.GetEnvironment())
		if !exist {
			deploy := &deployment{
				AppName:     eventMetadata.GetApplication(),
				Environment: eventMetadata.GetEnvironment(),
			}
			if err := tx.Insert(deploy); err != nil {
				return errors.Wrap(err, "Unable to insert deployment")
			}
			did = deploy.ID
		}

		// 2) Service Group
		gid, exist := db.getServiceGroupID(svcMetadata.GetServiceGroup())
		if !exist {
			svcGroup := &serviceGroup{
				Name:         svcMetadata.GetServiceGroup(),
				DeploymentID: did,
			}
			if err := tx.Insert(svcGroup); err != nil {
				return errors.Wrap(err, "Unable to insert service_group")
			}
			gid = svcGroup.ID
		}

		// 3) Supervisor
		sid, exist := db.getSupervisorID(eventMetadata.GetSupervisorId())
		if !exist {
			sup := &supervisor{
				MemberID: eventMetadata.GetSupervisorId(),
				Fqdn:     eventMetadata.GetFqdn(),
				Site:     eventMetadata.GetSite(),
			}

			if err := tx.Insert(sup); err != nil {
				return errors.Wrap(err, "Unable to insert supervisor")
			}
			sid = sup.ID
		}

		// 4) Service
		svc := &service{
			Origin:              pkgIdent.Origin,
			Name:                pkgIdent.Name,
			Version:             pkgIdent.Version,
			Release:             pkgIdent.Release,
			Health:              health,
			GroupID:             gid,
			DeploymentID:        did,
			SupID:               sid,
			FullPkgIdent:        pkgIdent.FullPackageIdent(),
			LastEventOccurredAt: convertOrCreateTimestamp(eventMetadata.GetOccurredAt()),
		}

		if svcMetadata.GetUpdateConfig() != nil {
			svc.Channel = svcMetadata.UpdateConfig.GetChannel()
		}

		if err := tx.Insert(svc); err != nil {
			return errors.Wrap(err, "Unable to insert service")
		}

		return nil
	})

}

const selectDeploymentID = `
SELECT id FROM deployment
WHERE app_name = $1
  AND environment = $2
`

func (db *Postgres) getDeploymentID(app, env string) (int32, bool) {
	var id int32
	err := db.SelectOne(&id, selectDeploymentID, app, env)
	if err != nil {
		return id, false
	}

	return id, true
}

const selectSupervisorID = `
SELECT id FROM supervisor
WHERE member_id = $1
`

func (db *Postgres) getSupervisorID(member string) (int32, bool) {
	var sid int32
	err := db.SelectOne(&sid, selectSupervisorID, member)
	if err != nil {
		return sid, false
	}

	return sid, true
}

const selectServiceGroupID = `
SELECT id FROM service_group
WHERE name = $1
`

func (db *Postgres) getServiceGroupID(name string) (int32, bool) {
	var gid int32
	err := db.SelectOne(&gid, selectServiceGroupID, name)
	if err != nil {
		return gid, false
	}

	return gid, true
}
