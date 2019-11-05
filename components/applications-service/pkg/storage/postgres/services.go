package postgres

import (
	"fmt"
	"regexp"
	"strings"
	"time"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/lib/pgutils"
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
	HCStdout            string    `db:"health_check_stdout"`
	HCStderr            string    `db:"health_check_stderr"`
	HCExitStatus        int32     `db:"health_check_exit_status"`
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
	Disconnected        bool      `db:"disconnected"`
	HealthUpdatedAt     time.Time `db:"health_updated_at"`
}

const (
	selectServiceFromUniqueFields = `
SELECT s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.health_check_stdout AS health_check_stdout
  , s.health_check_stderr AS health_check_stderr
  , s.health_check_exit_status AS health_check_exit_status
  , s.health AS health
  , s.service_group_name AS group
  , s.application AS application
  , s.environment AS environment
  , s.supervisor_id AS sup_member_id
  , s.fqdn AS fqdn
  , s.channel as channel
  , s.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.update_strategy as update_strategy
  , s.health_updated_at as health_updated_at
	, s.disconnected as disconnected
	, s.health_check_stdout as health_check_stdout
	, s.health_check_stderr as health_check_stderr
	, s.health_check_exit_status as health_check_exit_status
FROM service_full AS s
WHERE s.name = $1
  AND s.supervisor_id = $2
`
	selectServiceByServiceGroupID = `
SELECT s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.health_check_stdout AS health_check_stdout
  , s.health_check_stderr AS health_check_stderr
  , s.health_check_exit_status AS health_check_exit_status
  , s.health AS health
  , s.service_group_name AS group
  , s.application AS application
  , s.environment AS environment
  , s.supervisor_id AS sup_member_id
  , s.fqdn AS fqdn
  , s.channel as channel
  , s.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.update_strategy as update_strategy
  , s.health_updated_at as health_updated_at
	, s.disconnected as disconnected
	, s.health_check_stdout as health_check_stdout
	, s.health_check_stderr as health_check_stderr
	, s.health_check_exit_status as health_check_exit_status
FROM service_full AS s
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
  , s.health_check_stdout AS health_check_stdout
  , s.health_check_stderr AS health_check_stderr
  , s.health_check_exit_status AS health_check_exit_status
  , s.health AS health
  , s.service_group_name AS group
  , s.application AS application
  , s.environment AS environment
  , s.supervisor_id AS sup_member_id
  , s.fqdn AS fqdn
  , s.channel as channel
  , s.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.health_updated_at as health_updated_at
	, s.disconnected as disconnected
	, s.health_check_stdout as health_check_stdout
	, s.health_check_stderr as health_check_stderr
	, s.health_check_exit_status as health_check_exit_status
FROM service_full AS s
WHERE last_event_occurred_at < now() - ($1 || ' seconds')::interval
`

	deleteDisconnectedServices = `
DELETE
FROM service_full
WHERE service_full.last_event_occurred_at < now() - ($1 || ' seconds')::interval
`

	markDisconnectedServices = `
UPDATE service_full AS s
SET disconnected = true
WHERE last_event_occurred_at < now() - ($1 || ' seconds')::interval
  AND disconnected = false
RETURNING s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.health_check_stdout AS health_check_stdout
  , s.health_check_stderr AS health_check_stderr
  , s.health_check_exit_status AS health_check_exit_status
  , s.health AS health
  , s.service_group_name AS group
  , s.application AS application
  , s.environment AS environment
  , s.supervisor_id AS sup_member_id
  , s.fqdn AS fqdn
  , s.channel as channel
  , s.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.health_updated_at as health_updated_at
  , s.disconnected as disconnected
	, s.health_check_stdout as health_check_stdout
	, s.health_check_stderr as health_check_stderr
	, s.health_check_exit_status as health_check_exit_status
`

	deleteServicesByID = `
   DELETE FROM service_full AS s
    WHERE s.id IN (%s)
RETURNING s.id
  , s.origin AS origin
  , s.name AS name
  , s.version AS version
  , s.release AS release
  , s.health_check_stdout AS health_check_stdout
  , s.health_check_stderr AS health_check_stderr
  , s.health_check_exit_status AS health_check_exit_status
  , s.health AS health
  , s.service_group_name AS group
  , s.application AS application
  , s.environment AS environment
  , s.supervisor_id AS sup_member_id
  , s.fqdn AS fqdn
  , s.channel as channel
  , s.site as site
  , s.last_event_occurred_at as last_event_occurred_at
  , s.previous_health as previous_health
  , s.health_updated_at as health_updated_at
  , s.disconnected as disconnected
	, s.health_check_stdout as health_check_stdout
	, s.health_check_stderr as health_check_stderr
	, s.health_check_exit_status as health_check_exit_status
`

	selectServicesHealthCounts = `
SELECT COUNT(*) AS total
  , COUNT(*) FILTER (WHERE s.health = 'CRITICAL') AS critical
  , COUNT(*) FILTER (WHERE s.health = 'UNKNOWN') AS unknown
  , COUNT(*) FILTER (WHERE s.health = 'WARNING') AS warning
  , COUNT(*) FILTER (WHERE s.health = 'OK') AS ok
  , COUNT(*) FILTER (WHERE s.disconnected ) AS disconnected
FROM service_full AS s
 %s
`

	selectServicesTotalCount = `
SELECT count(*)
  FROM service_full;
`
)

var validFilterFields = []string{
	"origin",
	"service_name",
	"version",
	"channel",
	"buildstamp",
	"application",
	"environment",
	"site",
	"service_full",
	"group_name",
	"status",
}

// GetServicesHealthCounts retrieves the health counts from all services in the database.
// This function accepts a set of filters that can be applied to the SQL query to get the
// health counts of a subset of the services in the database
func (db *Postgres) GetServicesHealthCounts(filters map[string][]string) (*storage.HealthCounts, error) {
	var (
		sHealthCounts         storage.HealthCounts
		whereConstraints, err = buildWhereConstraintsFromFilters(filters, "WHERE", false)
	)
	if err != nil {
		return nil, err
	}

	// Formatting our Query with where constraints
	formattedQuery := fmt.Sprintf(selectServicesHealthCounts, whereConstraints)

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
		whereConstraints, err = buildWhereConstraintsFromFilters(filters, "WHERE", true)
	)

	if err != nil {
		return nil, err
	}

	if !sortAsc {
		sortOrder = "DESC"
	}

	// Formatting our Query with where constraints, sort field and sort order
	formattedQuery := fmt.Sprintf(selectServiceByServiceGroupID,
		whereConstraints,
		orderByStatementFromSortField(sortField),
		sortOrder,
	)

	_, err = db.DbMap.Select(&services, formattedQuery, pageSize, offset)
	return convertComposedServicesToStorage(services), err
}

func (db *Postgres) GetServicesDistinctValues(fieldName, queryFragment string, filters map[string][]string) ([]string, error) {
	// We do not want to filter on the fieldname we are looking up, because a user may want to use multiple values for searching
	delete(filters, fieldName)
	// Pass "AND" for firstKeyword because we build the WHERE with the query fragment
	whereConstraints, err := buildWhereConstraintsFromFilters(filters, "AND", false)
	if err != nil {
		return nil, err
	}

	fieldNameIsValid := false
	for _, valid := range validFilterFields {
		if fieldName == valid {
			fieldNameIsValid = true
			break
		}
	}
	if !fieldNameIsValid {
		return nil, errors.Errorf("field name %q is not valid for filtering, valid values are %v", fieldName, validFilterFields)
	}

	columnName := pq.QuoteIdentifier(columnNameForField(fieldName))
	queryFirst := fmt.Sprintf("SELECT DISTINCT %[1]s from service_full AS t WHERE t.%[1]s ILIKE $1 ",
		columnName,
	)
	querySecond := fmt.Sprintf(" ORDER BY %s ASC LIMIT 100;",
		columnName,
	)
	query := queryFirst + whereConstraints + querySecond
	matcher := fmt.Sprintf("%s%%", pgutils.EscapeLiteralForPG(queryFragment))

	var matches []string
	_, err = db.DbMap.Select(&matches, query, matcher)
	if err != nil {
		return nil, err
	}
	return matches, nil
}

func columnNameForField(fieldName string) string {
	switch fieldName {
	case "service_name", "service":
		return "name"
	case "group_name", "group":
		return "service_group_name_suffix"
	case "buildstamp":
		return "release"
	default:
		return fieldName
	}
}

// GetDisconnectedServices returns a list of disconnected services
func (db *Postgres) GetDisconnectedServices(thresholdSeconds int32) ([]*storage.Service, error) {
	var services []*composedService

	_, err := db.DbMap.Select(&services, selectDisconnectedServices, thresholdSeconds)
	return convertComposedServicesToStorage(services), err
}

func (db *Postgres) MarkDisconnectedServices(thresholdSeconds int32) ([]*storage.Service, error) {
	var services []*composedService

	_, err := db.DbMap.Select(&services, markDisconnectedServices, thresholdSeconds)
	if err != nil {
		return nil, err
	}
	return convertComposedServicesToStorage(services), nil
}

// DeleteDisconnectedServices deletes any service records where the time
// elapsed since the last event is greater than the specified thresholdMinutes.
// After deleting the services, it also removes all supervisor, service_group,
// and deployment records that are not referenced by any service record because
// the application treats these things as emergent properties of groups of
// services so they shouldn't exist if there are no associated services. This
// also prevents the need for a second cleanup operation to delete these
// things.
func (db *Postgres) DeleteDisconnectedServices(thresholdSeconds int32) ([]*storage.Service, error) {
	var services []*composedService
	_, err := db.DbMap.Select(&services, selectDisconnectedServices, thresholdSeconds)
	if err != nil {
		return nil, errors.Wrap(err, "unable to list disconnected services")
	}
	_, err = db.DbMap.Exec(deleteDisconnectedServices, thresholdSeconds)
	if err != nil {
		return nil, errors.Wrap(err, "unable to delete disconnected services")
	}

	return convertComposedServicesToStorage(services), nil
}

func (db *Postgres) DeleteServicesByID(svcIDs []string) ([]*storage.Service, error) {
	var services []*composedService

	// We use a more strict regexp than pgutils, since we know the data must be
	// integers
	r := regexp.MustCompile("^[0-9]*$")
	for _, svcID := range svcIDs {
		if !r.MatchString(svcID) {
			return nil, errors.Errorf("invalid service id %q", svcID)
		}
	}

	query := fmt.Sprintf(deleteServicesByID, strings.Join(svcIDs, ", "))

	_, err := db.DbMap.Select(&services, query)
	if err != nil {
		return nil, errors.Wrap(err, "unable to delete services")
	}
	return convertComposedServicesToStorage(services), nil
}

func (db *Postgres) GetServicesCount() (int32, error) {
	count, err := db.DbMap.SelectInt(selectServicesTotalCount)
	return int32(count), err
}

// GetServiceFromUniqueFields retrieves a service from the db without the need
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
// firstKeyword is added so you can build onto existing constraints that already have a 'WHERE' keyword declared
// to build onto existing constraints pass in an 'AND' keyword or 'OR' depending on your use case.
func buildWhereConstraintsFromFilters(filters map[string][]string, firstKeyword string, includeStatusFilter bool) (string, error) {
	var (
		firstStatement   = true
		whereConstraints = ""
	)

	for filter, values := range filters {
		// We want to ignore the status filter when we are doing health counts, because we want the counts for all
		// of the statuses.
		if len(values) == 0 || (!includeStatusFilter && (filter == "status" || filter == "STATUS")) {
			continue
		}

		if firstStatement { // Let the calling function determine if this starts with WHERE or AND
			whereConstraints = firstKeyword
			firstStatement = false
		} else {
			whereConstraints = whereConstraints + " AND"
		}

		switch filter {
		case "service_group_id":
			whereConstraints = whereConstraints + buildORStatementFromValues("service_group_id", values)

		case "status":
			whereConstraints = whereConstraints + buildHealthStatementFromValues(values)

		case "origin":
			whereConstraints = whereConstraints + buildORStatementFromValues("origin", values)

		case "channel":
			whereConstraints = whereConstraints + buildORStatementFromValues("channel", values)

		case "site":
			whereConstraints = whereConstraints + buildORStatementFromValues("site", values)

		case "version":
			whereConstraints = whereConstraints + buildORStatementFromValues("version", values)

		case "buildstamp":
			whereConstraints = whereConstraints + buildORStatementFromValues("release", values)

		case "application":
			whereConstraints = whereConstraints + buildORStatementFromValues("application", values)

		case "environment":
			whereConstraints = whereConstraints + buildORStatementFromValues("environment", values)

		case "group":
			whereConstraints = whereConstraints + buildORStatementFromValues("s.service_group_name_suffix", values)

		case "service":
			whereConstraints = whereConstraints + buildORStatementFromValues("name", values)

		default:
			return "", errors.Errorf("invalid filter. (%s:%s)", filter, values)
		}
	}
	return whereConstraints, nil
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

		if strings.ContainsAny(value, "?*") { // Wild cards detected, lookout!
			pgWild := strings.Replace(value, "*", "%", -1)
			pgWild = strings.Replace(pgWild, "?", "_", -1)
			ORConstraint = ORConstraint + fmt.Sprintf(" %s LIKE '%s'", field, pgutils.EscapeLiteralForPG(pgWild))
		} else {
			ORConstraint = ORConstraint + fmt.Sprintf(" %s = '%s'", field, pgutils.EscapeLiteralForPG(value))
		}
		secondStatement = true
	}

	return ORConstraint
}

func buildHealthStatementFromValues(values []string) string {
	var (
		secondStatement = false
		ORConstraint    = ""
	)

	for _, value := range values {
		if secondStatement {
			ORConstraint = ORConstraint + " OR"
		}
		if value == "disconnected" {
			ORConstraint = " disconnected"
		} else {
			ORConstraint = ORConstraint + fmt.Sprintf(" %s = '%s'", "health",
				pgutils.EscapeLiteralForPG(strings.ToUpper(value)))
		}

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
