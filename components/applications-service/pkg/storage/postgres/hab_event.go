package postgres

import (
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/api/external/applications"
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
	labelSuccess            = "succeeded"
	labelFailure            = "failed"
	uniqueDeploymentError   = "Unable to insert deployment: pq: duplicate key value violates unique constraint \"deployment_unique\""
	uniqueServiceGroupError = "Unable to insert service_group: pq: duplicate key value violates unique constraint \"service_group_name_deployment_id_key\""
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

	// @afiune are we changing the grouping of a service group based of the application
	// and environment names? if not, should we update those fields?
	svc, exist := db.getServiceFromUniqueFields(
		pkgIdent.Name,
		eventMetadata.GetSupervisorId(),
	)

	// @afiune all our backend was designed for the health check to be all
	// uppercases but habitat is actually sending case sensitive strings
	eventHealth := strings.ToUpper(event.GetResult().String())
	// TODO @afiune verify health

	// If the service already exists, we just do an update
	if exist {
		return db.updateTables(
			svc,
			eventMetadata,
			svcMetadata,
			pkgIdent,
			eventHealth,
		)
	}

	// But if the service doesn't exist, we will handle it as a new service insertion
	err = db.insertNewService(
		eventMetadata,
		svcMetadata,
		pkgIdent,
		eventHealth,
	)

	if err != nil {
		// We will retry once if a unique constraint is hit,
		// in case a new deployment or service group is in the same batch of events.
		if err.Error() == uniqueDeploymentError || err.Error() == uniqueServiceGroupError {
			return db.insertNewService(
				eventMetadata,
				svcMetadata,
				pkgIdent,
				eventHealth,
			)
		}
	}
	return err
}

func (db *Postgres) updateTables(
	svc *service,
	eventMetadata *habitat.EventMetadata,
	svcMetadata *habitat.ServiceMetadata,
	pkgIdent *packageIdent,
	health string) error {

	db.updateService(svc, eventMetadata, svcMetadata, pkgIdent, health)

	deploy, err := db.getDeployment(svc.DeploymentID)
	if err != nil {
		return errors.Wrap(err, "unable to update tables")
	}
	db.updateDeployment(deploy, eventMetadata)

	sup, err := db.getSupervisor(svc.SupID)
	if err != nil {
		return errors.Wrap(err, "unable to update tables")
	}
	db.updateSupervisor(sup, eventMetadata)

	sg, err := db.getServiceGroup(svc.GroupID)
	if err != nil {
		return errors.Wrap(err, "unable to update tables")
	}
	db.updateServiceGroup(sg, svcMetadata)

	// @afiune in the future if we have more tables that might need
	// to be updated, we will just add them to this map of tables
	tables := map[string]dbTable{
		"service":       svc,
		"deployment":    deploy,
		"supervisor":    sup,
		"service_group": sg,
	}

	return db.triggerDataUpdates(tables)
}

// triggerDataUpdates receives all the data that might need to be updated
// and wraps it into a single transaction, on any error we will roll back
// the modifications made to ensure we weren't able to apply the changes
// from the message, all data structs has a field 'needUpdate' that should
// be modified when an update is required.
func (db *Postgres) triggerDataUpdates(tables map[string]dbTable) error {

	return dblib.Transaction(db.DbMap, func(tx *gorp.Transaction) error {

		for tname, data := range tables {
			if data.NeedUpdate() {
				if _, err := tx.Update(data); err != nil {
					return errors.Wrap(err, "unable to update "+tname)
				}
			}
		}

		return nil
	})
}

// updates the provided supervisor from a HealthCheck event
func (db *Postgres) updateSupervisor(
	sup *supervisor, eventMetadata *habitat.EventMetadata) {

	if sup.Fqdn != eventMetadata.GetFqdn() {
		sup.Fqdn = eventMetadata.GetFqdn()
		sup.needUpdate = true
	}

	if sup.Site != eventMetadata.GetSite() {
		sup.Site = eventMetadata.GetSite()
		sup.needUpdate = true
	}

	// TODO @afiune we could have this column in all tables as well
	// when a supervisor needs update, update the timestamp of the last event received
	//if sup.needUpdate {
	//sup.LastEventOccurredAt = convertOrCreateTimestamp(eventMetadata.GetOccurredAt())
	//}
}

// updates the provided deployment from a HealthCheck event
func (db *Postgres) updateDeployment(
	deploy *deployment, eventMetadata *habitat.EventMetadata) {

	if deploy.AppName != eventMetadata.GetApplication() {
		deploy.AppName = eventMetadata.GetApplication()
		deploy.needUpdate = true
	}

	if deploy.Environment != eventMetadata.GetEnvironment() {
		deploy.Environment = eventMetadata.GetEnvironment()
		deploy.needUpdate = true
	}

	// TODO @afiune we could have this column in all tables as well
	// when a deployment needs update, update the timestamp of the last event received
	//if deploy.needUpdate {
	//deploy.LastEventOccurredAt = convertOrCreateTimestamp(eventMetadata.GetOccurredAt())
	//}
}

// updates the provided service group from a HealthCheck event
func (db *Postgres) updateServiceGroup(
	sg *serviceGroup, svcMetadata *habitat.ServiceMetadata) {

	if sg.Name != svcMetadata.GetServiceGroup() {
		sg.Name = svcMetadata.GetServiceGroup()
		sg.needUpdate = true
	}

	// TODO @afiune we could have this column in all tables as well
	// when a service group needs update, update the timestamp of the last event received
	//if sg.needUpdate {
	//sg.LastEventOccurredAt = convertOrCreateTimestamp(eventMetadata.GetOccurredAt())
	//}
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

// updates the provided service from a HealthCheck event
//
// fields that needs to be updated:
// - Package ident (without name. the name of a service can't be changed!)
// - Update strategy
// - Health, Application, Environment, Site and Fqdn
// - Timestamp of last event received (occurred_at)
func (db *Postgres) updateService(
	svc *service,
	eventMetadata *habitat.EventMetadata,
	svcMetadata *habitat.ServiceMetadata,
	pkgIdent *packageIdent,
	health string) {

	// Verify if the service health changed, if so, save the current health
	// into the previous_health and update it with the new one
	if svc.Health != health {
		svc.HealthUpdatedAt = time.Now()
		svc.PreviousHealth = svc.Health
		svc.Health = health
		svc.needUpdate = true
	}

	// Update Package Identifier
	if svc.FullPkgIdent != pkgIdent.FullPackageIdent() {
		svc.Origin = pkgIdent.Origin
		svc.Version = pkgIdent.Version
		svc.Release = pkgIdent.Release
		svc.FullPkgIdent = pkgIdent.FullPackageIdent()
		svc.needUpdate = true
	}

	// Update Channel
	updateServiceChannel(svc, svcMetadata.GetUpdateConfig())

	// update always the timestamp of the last event received so that the database
	// has a record of when the last message was received for a service
	svc.LastEventOccurredAt = convertOrCreateGoTime(eventMetadata.GetOccurredAt())
	svc.needUpdate = true
}

// update the service channel from the provided habitat update config
// TODO @afiune do we want to store the strategy?
func updateServiceChannel(svc *service, updateConfig *habitat.UpdateConfig) {
	if updateConfig == nil && svc.Channel != "" {
		svc.Channel = ""
		svc.needUpdate = true
	}

	if updateConfig != nil {
		channel := updateConfig.GetChannel()
		if svc.Channel != channel {
			svc.Channel = channel
			svc.needUpdate = true
		}
	}
}

// insertNewService assumes the service to be inserted doesn't exist already,
// this function is wrapped with a transaction func since it could do multiple things
// like insert a supervisor, service_group, deployment and the service itself.
func (db *Postgres) insertNewService(
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
			PreviousHealth:      applications.HealthStatus_NONE.String(),
			HealthUpdatedAt:     time.Now(),
			LastEventOccurredAt: convertOrCreateGoTime(eventMetadata.GetOccurredAt()),
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
