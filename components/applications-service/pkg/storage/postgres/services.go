package postgres

import (
	"fmt"
	"time"

	_ "github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/applications-service/pkg/storage"
)

// composedService is a more user friendly and clear representation of a service.
// it is composed by values from other tables inside the database (JOINs)
type composedService struct {
	ID                  int32     `db:"id"`
	SupMemberID         string    `db:"sup_member_id"`
	Origin              string    `db:"origin"`
	Name                string    `db:"name"`
	Version             string    `db:"version"`
	Release             string    `db:"release"`
	Status              string    `db:"status"`
	Health              string    `db:"health"`
	Group               string    `db:"group"`
	Fqdn                string    `db:"fqdn"`
	Application         string    `db:"application"`
	Environment         string    `db:"environment"`
	Channel             string    `db:"channel"`
	Site                string    `db:"site"`
	PreviousHealth      string    `db:"previous_health"`
	UpdateStrategy      string    `db:"update_strategy"`
	LastEventOccurredAt time.Time `db:"last_event_occurred_at"`
	HealthUpdatedAt     time.Time `db:"health_updated_at"`
}

const (
	selectServiceInternal = `
SELECT id
  , origin
  , name
  , version
  , release
  , status
  , health
  , group_id
  , deployment_id
  , sup_id
  , channel
  , package_ident
  , last_event_occurred_at
  , previous_health
  , update_strategy
  , health_updated_at
FROM service
WHERE name = $1
  AND sup_id IN (
    SELECT id FROM supervisor
    WHERE member_id = $2
  )
`
	selectServiceFromUniqueFields = `
SELECT s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.status AS status
  , s.health AS health
  , sg.name AS group
  , d.app_name AS application
  , d.environment AS environment
  , sup.member_id AS sup_member_id
  , sup.fqdn AS fqdn
  , s.channel as channel
  , sup.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.update_strategy as update_strategy
  , s.health_updated_at as health_updated_at
FROM service AS s
LEFT JOIN service_group AS sg
  ON s.group_id = sg.id
LEFT JOIN deployment AS d
  ON s.deployment_id = d.id
LEFT JOIN supervisor AS sup
  ON s.sup_id = sup.id
WHERE s.name = $1
  AND sup.member_id = $2
`
	selectServiceByServiceGroupID = `
SELECT s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.status AS status
  , s.health AS health
  , sg.name AS group
  , d.app_name AS application
  , d.environment AS environment
  , sup.member_id AS sup_member_id
  , sup.fqdn AS fqdn
  , s.channel as channel
  , sup.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.update_strategy as update_strategy
  , s.health_updated_at as health_updated_at
FROM service AS s
LEFT JOIN service_group AS sg
  ON s.group_id = sg.id
LEFT JOIN deployment AS d
  ON s.deployment_id = d.id
LEFT JOIN supervisor AS sup
  ON s.sup_id = sup.id
 %s
ORDER BY %s %s, sup_member_id ASC
LIMIT $1
OFFSET $2
`
	selectDisconnectedServices = `
SELECT s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.status AS status
  , s.health AS health
  , sg.name AS group
  , d.app_name AS application
  , d.environment AS environment
  , sup.member_id AS sup_member_id
  , sup.fqdn AS fqdn
  , s.channel as channel
  , sup.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.health_updated_at as health_updated_at
FROM service AS s
LEFT JOIN service_group AS sg
  ON s.group_id = sg.id
LEFT JOIN deployment AS d
  ON s.deployment_id = d.id
LEFT JOIN supervisor AS sup
  ON s.sup_id = sup.id
WHERE last_event_occurred_at < now() - ($1 || ' minutes')::interval
`

	deleteDisconnectedServices = `
DELETE
FROM service
WHERE service.last_event_occurred_at < now() - ($1 || ' minutes')::interval
`

	selectServicesHealthCounts = `
SELECT COUNT(*) AS total
  , COUNT(*) FILTER (WHERE s.health = 'CRITICAL') AS critical
  , COUNT(*) FILTER (WHERE s.health = 'UNKNOWN') AS unknown
  , COUNT(*) FILTER (WHERE s.health = 'WARNING') AS warning
  , COUNT(*) FILTER (WHERE s.health = 'OK') AS ok
FROM service AS s
 %s
`

	selectServicesTotalCount = `
SELECT count(*)
  FROM service;
`
)

// GetServicesHealthCounts retrieves the health counts from all services in the database.
// This function accepts a set of filters that can be applied to the SQL query to get the
// health counts of a subset of the services in the database
func (db *Postgres) GetServicesHealthCounts(filters map[string][]string) (*storage.HealthCounts, error) {
	var (
		sHealthCounts         storage.HealthCounts
		WhereConstraints, err = buildWhereConstraintsFromFilters(filters)
	)
	if err != nil {
		return nil, err
	}

	// Formatting our Query with where constraints
	formattedQuery := fmt.Sprintf(selectServicesHealthCounts, WhereConstraints)

	err = db.SelectOne(&sHealthCounts, formattedQuery)
	if err != nil {
		return nil, err
	}

	return &sHealthCounts, nil
}

// GetServices returns a list of services
func (db *Postgres) GetServices(
	sortField string, sortAsc bool,
	page int32, pageSize int32,
	filters map[string][]string,
) ([]*storage.Service, error) {

	// Decrement one to the page since we must start from zero
	page = page - 1

	var (
		services              []*composedService
		offset                = pageSize * page
		sortOrder             = "ASC"
		WhereConstraints, err = buildWhereConstraintsFromFilters(filters)
	)

	if err != nil {
		return nil, err
	}

	if !sortAsc {
		sortOrder = "DESC"
	}

	// Formatting our Query with where constraints, sort field and sort order
	formattedQuery := fmt.Sprintf(selectServiceByServiceGroupID,
		WhereConstraints,
		orderByStatementFromSortField(sortField),
		sortOrder,
	)

	_, err = db.DbMap.Select(&services, formattedQuery, pageSize, offset)
	return convertComposedServicesToStorage(services), err
}

// GetDisconnectedServices returns a list of disconnected services
func (db *Postgres) GetDisconnectedServices(thresholdMinutes int32) ([]*storage.Service, error) {
	var services []*composedService

	_, err := db.DbMap.Select(&services, selectDisconnectedServices, thresholdMinutes)
	return convertComposedServicesToStorage(services), err
}

// DeleteDisconnectedServices deletes any service records where the time
// elapsed since the last event is greater than the specified thresholdMinutes.
// After deleting the services, it also removes all supervisor, service_group,
// and deployment records that are not referenced by any service record because
// the application treats these things as emergent properties of groups of
// services so they shouldn't exist if there are no associated services. This
// also prevents the need for a second cleanup operation to delete these
// things.
func (db *Postgres) DeleteDisconnectedServices(thresholdMinutes int32) ([]*storage.Service, error) {

	tx, err := db.DbMap.Begin()
	if err != nil {
		return nil, errors.Wrap(err, "unable to start DB transaction for DeleteDisconnectedServices")
	}

	// rollback to cancel the transaction if we fail later
	var completed bool
	defer func() {
		if !completed {
			tx.Rollback() //nolint: errcheck
		}
	}()

	// We wish to avoid what the SQL standard specifies "phantom read":
	// > A transaction re-executes a query returning a set of rows that satisfy a
	// > search condition and finds that the set of rows satisfying the condition
	// > has changed due to another recently-committed transaction.
	// A phantom read would be bad here because we use a "query for a set of rows
	// that satisfy a search condition" to find service groups, supervisors, and
	// deployments that we want to delete. To avoid phantom reads in postgres, we
	// need the "repeatable read" isolation level:
	// https://www.postgresql.org/docs/9.6/transaction-iso.html
	//
	// The standard library sql package lets you pass options to BeginTx() but
	// gorp doesn't support this so we do it by Exec as a workaround.
	_, err = tx.Exec(`SET TRANSACTION ISOLATION LEVEL repeatable read`)
	if err != nil {
		return nil, errors.Wrap(err, "unable to configure DB transaction for DeleteDisconnectedServices")
	}

	var services []*composedService
	_, err = tx.Select(&services, selectDisconnectedServices, thresholdMinutes)
	if err != nil {
		return nil, errors.Wrap(err, "unable to list disconnected services")
	}
	_, err = tx.Exec(deleteDisconnectedServices, thresholdMinutes)
	if err != nil {
		return nil, errors.Wrap(err, "unable to delete disconnected services")
	}
	_, err = tx.Exec(deleteSupsWithoutServices)
	if err != nil {
		return nil, errors.Wrap(err, "unable to cleanup unneeded supervisor records")
	}
	_, err = tx.Exec(deleteSvcGroupsWithoutServices)
	if err != nil {
		return nil, errors.Wrap(err, "unable to cleanup unneeded service group records")
	}
	_, err = tx.Exec(deleteDeploymentsWithoutServices)
	if err != nil {
		return nil, errors.Wrap(err, "unable to cleanup unneeded deployment records")
	}

	err = tx.Commit()
	if err != nil {
		return nil, errors.Wrap(err, "unable to commit transaction deleting disconnected services")
	}
	completed = true // don't execute deferred tx.Rollback from above

	return convertComposedServicesToStorage(services), nil
}

func (db *Postgres) GetServicesCount() (int32, error) {
	count, err := db.DbMap.SelectInt(selectServicesTotalCount)
	return int32(count), err
}

// getServiceFromUniqueFields retrieves a service from the db without the need
// of an id, it is based on the unique fields, name and member id
func (db *Postgres) GetServiceFromUniqueFields(name, member string) (*storage.Service, bool) {
	if name == "" || member == "" {
		return nil, false
	}

	var svc composedService
	err := db.SelectOne(&svc, selectServiceFromUniqueFields, name, member)
	if err != nil {
		return nil, false
	}

	return convertComposedServiceToStorage(&svc), true
}

// getServiceFromUniqueFields is used to ingest/update services internally and it
// selects a single service from the database with IDs
func (db *Postgres) getServiceFromUniqueFields(name, member string) (*service, bool) {
	var svc service
	err := db.SelectOne(&svc, selectServiceInternal, name, member)
	if err != nil {
		return nil, false
	}

	return &svc, true
}

// converts an array of composedService to an array of storage.Service
func convertComposedServicesToStorage(svcs []*composedService) []*storage.Service {
	storageSvcs := make([]*storage.Service, len(svcs))
	for i, svc := range svcs {
		storageSvcs[i] = convertComposedServiceToStorage(svc)
	}
	return storageSvcs
}

// converts a composedService to a storage.Service
func convertComposedServiceToStorage(svc *composedService) *storage.Service {
	// field names are the same, so we can cast this
	return (*storage.Service)(svc)
}

// buildWhereConstraintsFromFilters converts the provided filters into SQL 'WHERE' constraints
//
// This function will be useful when we have the search bar and the user start building a set
// of filters like the following one:
//
// **Example:** I want to see all services that:
//            * Belong to the application=cafe-us OR application=cafe-eu
//                        <----      AND     ----->
//            * They are production environments (environment=production)
//                        <----      AND     ----->
//            * They have health=CRITICAL OR health=UNKNOWN
//
// In the case above this function will generate the following WHERE Constraint:
//
// => WHERE application = 'cafe-us' OR application = 'cafe-eu'
//      AND health = 'CRITICAL'     OR health = 'UNKNOWN'
//      AND environment = 'production'
//
func buildWhereConstraintsFromFilters(filters map[string][]string) (string, error) {
	var (
		firstStatement   = true
		WhereConstraints = ""
	)

	for filter, values := range filters {
		if len(values) == 0 {
			continue
		}

		if firstStatement {
			WhereConstraints = "WHERE"
			firstStatement = false
		} else {
			WhereConstraints = WhereConstraints + " AND"
		}

		switch filter {
		case "service_group_id":
			WhereConstraints = WhereConstraints + buildORStatementFromValues("group_id", values)

		case "health":
			WhereConstraints = WhereConstraints + buildORStatementFromValues("health", values)

		default:
			return "", errors.Errorf("invalid filter. (%s:%s)", filter, values)
		}
	}

	return WhereConstraints, nil
}

// buildORStatementFromValues generates SQL 'OR' statement from the provided values of a single filter
//
// This function accepts an array of strings that are coming from a single filter (look at the
// function 'buildWhereConstraintsFromFilters') and builds an SQL OR statement. In the case that
// the array of values is size=1, it will only return a single constraint without any OR statement.
//
// Example 1: A filter with a single value.
//
// field=["application"]
// values=["cafe"]
// ** returns "application = 'cafe'"
//
// Example 2: A filter with multiple values
//
// field="health"
// values=["CRITICAL", "UNKNOWN"]
// ** returns "health = 'CRITICAL' OR health = 'UNKNOWN'"
//
func buildORStatementFromValues(field string, values []string) string {
	var (
		secondStatement = false
		ORConstraint    = ""
	)

	for _, value := range values {
		if secondStatement {
			ORConstraint = ORConstraint + " OR"
		}

		ORConstraint = ORConstraint + fmt.Sprintf(" %s = '%s'", field, value)
		secondStatement = true
	}

	return ORConstraint
}

// orderByStatementFromSortField returns the ORDER BY statement from the provided sort field,
// this function will return the same field if it doesn't require a special ORDER BY statement
// like the field 'health' that requires a CASE Statement
func orderByStatementFromSortField(field string) string {
	switch field {
	case "health":
		return healthOrderByCaseStatement()
	default:
		return field
	}
}

// healthOrderByCaseStatement returns the ORDER BY statement for the health field
func healthOrderByCaseStatement() string {
	return `
CASE WHEN health = 'CRITICAL' THEN 1
     WHEN health = 'UNKNOWN' THEN 2
     WHEN health = 'WARNING' THEN 3
     WHEN health = 'OK' THEN 4
END
`
}
