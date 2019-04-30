package postgres

import (
	"time"

	"github.com/chef/automate/api/external/applications"
	dblib "github.com/chef/automate/lib/db"
	"github.com/go-gorp/gorp"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"

	log "github.com/sirupsen/logrus"
)

const (
	labelSuccess = "succeeded"
	labelFailure = "failed"
)

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

// IngestHabEvents process habitat events and store them into the database
func (db *Postgres) IngestHabEvent(event *applications.HabService) error {
	opsInFlight.Inc()
	defer opsInFlight.Dec()
	processingStart := time.Now()
	timeLastEventSeen = float64(processingStart.Unix())
	err := db.IngestHabEventWithoutMetrics(event)
	duration := time.Since(processingStart)

	var label string
	if err == nil {
		label = labelSuccess
	} else {
		label = labelFailure
	}

	ingestDurations.With(prometheus.Labels{"result": label}).Observe(duration.Seconds())

	return err
}

// IngestHabEvents process habitat events and store them into the database
func (db *Postgres) IngestHabEventWithoutMetrics(event *applications.HabService) error {
	log.WithFields(log.Fields{
		"storage": "postgres",
		"message": event,
	}).Debug("Processing event in storage")

	// @afiune For our first iteration we will process the messages in the storage
	// side but in the future we need to move all this logic into an ingestion
	// pipeline. I'll try to create functions that can be easily moved.

	svc, exist := db.getServiceFromUniqueFields(
		event.GetPkgIdent().GetOrigin(),
		event.GetPkgIdent().GetName(),
		event.GetSupervisorId(),
	)

	// If the service already exists, we just do a simple update
	if exist {
		updateServiceFromHabEvent(event, svc)
		if _, err := db.DbMap.Update(svc); err != nil {
			return errors.Wrap(err, "Unable to insert service")
		}
		return nil
	}

	// But if the service doesn't exist, we will handle it as a new service insertion
	return db.insertNewService(event)
}

// insertNewService assumes the service to be inserted doesn't exist already,
// this function is wrapped with a transaction func since it could do multiple things
// like insert a supervisor, service_group, deployment and the service itself.
func (db *Postgres) insertNewService(event *applications.HabService) error {

	return dblib.Transaction(db.DbMap, func(tx *gorp.Transaction) error {

		// 1) Deployment
		did, exist := db.getDeploymentID(event.GetApplication(), event.GetEnvironment())
		if !exist {
			deploy := deploymentFromHabEvent(event)
			if err := tx.Insert(deploy); err != nil {
				return errors.Wrap(err, "Unable to insert deployment")
			}
			did = deploy.ID
		}

		// 2) Service Group
		gid, exist := db.getServiceGroupID(event.FullServiceGroupName())
		if !exist {
			svcGroup := serviceGroupFromHabEvent(event, did)
			if err := tx.Insert(svcGroup); err != nil {
				return errors.Wrap(err, "Unable to insert service_group")
			}
			gid = svcGroup.ID
		}

		// 3) Supervisor
		sid, exist := db.getSupervisorID(event.GetSupervisorId())
		if !exist {
			sup := supervisorFromHabEvent(event)
			if err := tx.Insert(sup); err != nil {
				return errors.Wrap(err, "Unable to insert supervisor")
			}
			sid = sup.ID
		}

		// 4) Service
		svc := serviceFromHabEvent(event, did, sid, gid)
		if err := tx.Insert(svc); err != nil {
			return errors.Wrap(err, "Unable to insert service")
		}

		return nil
	})

}

func deploymentFromHabEvent(event *applications.HabService) *deployment {
	return &deployment{
		AppName:     event.GetApplication(),
		Environment: event.GetEnvironment(),
	}
}

func supervisorFromHabEvent(event *applications.HabService) *supervisor {
	return &supervisor{
		MemberID: event.GetSupervisorId(),
		// TODO: figure out how habitat will provide this information
		Fqdn: event.GetFqdn(),
	}
}

func serviceGroupFromHabEvent(event *applications.HabService, did int32) *serviceGroup {
	return &serviceGroup{
		Name:         event.FullServiceGroupName(),
		DeploymentID: did,
	}
}

func updateServiceFromHabEvent(event *applications.HabService, svc *service) {
	svc.Origin = event.GetPkgIdent().GetOrigin()
	svc.Name = event.GetPkgIdent().GetName()
	svc.Version = event.GetPkgIdent().GetVersion()
	svc.Release = event.GetPkgIdent().GetRelease()
	svc.Health = event.GetHealthCheck().String()
	svc.Status = event.GetStatus().String()
}

func serviceFromHabEvent(event *applications.HabService, did, sid, gid int32) *service {
	return &service{
		Origin:       event.GetPkgIdent().GetOrigin(),
		Name:         event.GetPkgIdent().GetName(),
		Version:      event.GetPkgIdent().GetVersion(),
		Release:      event.GetPkgIdent().GetRelease(),
		Health:       event.GetHealthCheck().String(),
		Status:       event.GetStatus().String(),
		GroupID:      gid,
		DeploymentID: did,
		SupID:        sid,
	}
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
