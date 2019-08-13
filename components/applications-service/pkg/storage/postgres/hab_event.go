package postgres

import (
	"fmt"
	"strings"
	"time"

	"github.com/go-gorp/gorp"
	"github.com/golang/protobuf/ptypes"
	timestamp "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	dblib "github.com/chef/automate/lib/db"
)

const (
	labelSuccess            = "succeeded"
	labelFailure            = "failed"
	uniqueDeploymentError   = "Unable to insert deployment: pq: duplicate key value violates unique constraint \"deployment_unique\""
	uniqueServiceGroupError = "Unable to insert service_group: pq: duplicate key value violates unique constraint \"service_group_name_deployment_id_key\""
)

var IngestToSingleTable = false

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

	if IngestToSingleTable {
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
			return err
		}
	}

	svc, exist := db.getServiceFromUniqueFields(
		pkgIdent.Name,
		eventMetadata.GetSupervisorId(),
	)

	// TODO @afiune verify health

	// If the service already exists, we just do an update
	if exist {
		return db.updateServiceAndRelations(
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

type deploymentServiceGroup struct {
	ServiceGroupID   int32  `db:"service_group_id"`
	DeploymentID     int32  `db:"deployment_id"`
	ServiceGroupName string `db:"sg_name"`
	AppName          string `db:"app_name"`
	Environment      string `db:"environment"`
}

const (
	selectMatchingDeploymentAndSG = `
SELECT sg.id AS service_group_id, d.id AS deployment_id, sg.name AS sg_name, d.app_name, d.environment
  FROM service_group AS sg
  JOIN deployment AS d ON sg.deployment_id = d.id
 WHERE sg.name = $1 AND d.app_name = $2 AND d.environment = $3;
`

	maybeCleanupServiceGroup = `
DELETE FROM service_group WHERE service_group.id = $1 AND (NOT EXISTS (SELECT 1 FROM service WHERE service.group_id = service_group.id ))
`
	maybeCleanupDeployment = `
DELETE FROM deployment WHERE deployment.id = $1 AND (NOT EXISTS (SELECT 1 FROM service WHERE service.deployment_id = deployment.id ))
`
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
	last_event_occurred_at
)
VALUES (
  $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17
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
	last_event_occurred_at
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
	EXCLUDED.last_event_occurred_at
)
;
`
)

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
		updateStrategy = svcMetadata.GetUpdateConfig().Strategy.String()
	}
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
	)

	return err
}

// updateServiceAndRelations updates the fields for a service row and makes
// any required changes to an associated supervisor row. It also ensures the
// deployment and service group are accurate, as described below:
//
// The deployment and service group may have changed since the previous update.
// When this happens, we don't want to _edit_ the deployment or service_group
// because other services could still belong to those and be unchanged, so if
// we change the values then the two services will get into an edit war.
//
// First, we look for a deploymentServiceGroup (joined deployment and
// service_group) that matches the metadata we were given in the hab event.
// If none exists, then we need to create at least a service group and maybe a
// deployment, and change the service's foreign key associations to the new
// deployment and service group.
//
// If we did find a matching deployment and service group, and it's not what is
// currently set on the service, we update the foreign key relations to the
// other deployment and service group when we apply updates for the service's
// other fields.
//
// If we changed the service's deployment and service group, then we run a
// cleanup query to ensure the prior deployment and service group are deleted
// if they no longer have associated services.
func (db *Postgres) updateServiceAndRelations(
	svc *service,
	eventMetadata *habitat.EventMetadata,
	svcMetadata *habitat.ServiceMetadata,
	pkgIdent *packageIdent,
	health string) error {

	oldDID := svc.DeploymentID
	oldGID := svc.GroupID

	var existingDSG deploymentServiceGroup
	err := db.DbMap.SelectOne(&existingDSG, selectMatchingDeploymentAndSG,
		svcMetadata.GetServiceGroup(),
		eventMetadata.GetApplication(),
		eventMetadata.GetEnvironment(),
	)

	needToCreateDeploymentServiceGroup := (err != nil)
	needToChangeDeploymentServiceGroup := needToCreateDeploymentServiceGroup || (existingDSG.ServiceGroupID != oldGID)

	return dblib.Transaction(db.DbMap, func(tx *gorp.Transaction) error {

		var newDSG deploymentServiceGroup

		if needToCreateDeploymentServiceGroup {
			// 1) the deployment may or may not exist; create if needed
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

			// 2) the service group doesn't exist
			svcGroup := bakeServiceGroup(svcMetadata.GetServiceGroup(), did)
			if err := tx.Insert(svcGroup); err != nil {
				return errors.Wrap(err, "Unable to insert service_group")
			}
			gid := svcGroup.ID

			newDSG = deploymentServiceGroup{
				DeploymentID:   did,
				ServiceGroupID: gid,
			}
		} else {
			newDSG = existingDSG
		}

		db.updateService(svc, eventMetadata, svcMetadata, pkgIdent, health, newDSG)

		if _, err := tx.Update(svc); err != nil {
			return errors.Wrap(err, "Unable to update service")
		}

		if needToChangeDeploymentServiceGroup {
			if _, err := tx.Exec(maybeCleanupServiceGroup, oldGID); err != nil {
				return errors.Wrap(err, "Unable to cleanup possibly unused service group")
			}

			if _, err := tx.Exec(maybeCleanupDeployment, oldDID); err != nil {
				return errors.Wrap(err, "Unable to cleanup possibly unused service group")
			}
		}

		sup, err := db.getSupervisor(svc.SupID)
		if err != nil {
			return errors.Wrap(err, "unable to update tables")
		}
		db.updateSupervisor(sup, eventMetadata)
		if _, err := tx.Update(sup); err != nil {
			return errors.Wrap(err, "Unable to update supervisor data")
		}

		return nil
	})
}

// constructs service group
func bakeServiceGroup(name string, did int32) *serviceGroup {
	return &serviceGroup{
		Name:         name,
		NameSuffix:   trimSuffix(name),
		DeploymentID: did,
	}
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
	health string,
	dsg deploymentServiceGroup,
) {

	// Verify if the service health changed, if so, save the current health
	// into the previous_health and update it with the new one
	if svc.Health != health {
		svc.HealthUpdatedAt = time.Now()
		svc.PreviousHealth = svc.Health
		svc.Health = health
	}

	// Update Package Identifier
	if svc.FullPkgIdent != pkgIdent.FullPackageIdent() {
		svc.Origin = pkgIdent.Origin
		svc.Version = pkgIdent.Version
		svc.Release = pkgIdent.Release
		svc.FullPkgIdent = pkgIdent.FullPackageIdent()
	}

	// Update Channel
	updateServiceStrategyAndChannel(svc, svcMetadata.GetUpdateConfig())

	// Update relations to Deployment and ServiceGroup
	svc.DeploymentID = dsg.DeploymentID
	svc.GroupID = dsg.ServiceGroupID

	// update always the timestamp of the last event received so that the database
	// has a record of when the last message was received for a service
	svc.LastEventOccurredAt = convertOrCreateGoTime(eventMetadata.GetOccurredAt())
}

// update the service channel & update strategy from the provided habitat update config
func updateServiceStrategyAndChannel(svc *service, updateConfig *habitat.UpdateConfig) {
	if updateConfig == nil {
		if svc.Channel != "" {
			svc.Channel = ""
		}

		if svc.UpdateStrategy != storage.NoneStrategy.String() {
			svc.UpdateStrategy = storage.NoneStrategy.String()
		}
	} else {
		channel := updateConfig.GetChannel()
		if svc.Channel != channel {
			svc.Channel = channel
		}

		strategy := storage.HabitatUpdateStrategyToStorageFormat(updateConfig.GetStrategy())
		if svc.UpdateStrategy != strategy.String() {
			svc.UpdateStrategy = strategy.String()
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
		gid, exist := db.getServiceGroupID(svcMetadata.GetServiceGroup(), did)
		if !exist {
			svcGroup := bakeServiceGroup(svcMetadata.GetServiceGroup(), did)
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
			strategy := storage.HabitatUpdateStrategyToStorageFormat(svcMetadata.UpdateConfig.GetStrategy())
			svc.UpdateStrategy = strategy.String()
		} else {
			svc.UpdateStrategy = storage.NoneStrategy.String()
		}

		if err := tx.Insert(svc); err != nil {
			return errors.Wrap(err, "Unable to insert service")
		}

		return nil
	})

}

func trimSuffix(name string) string {
	parts := strings.Split(name, ".")
	return parts[1]
}
