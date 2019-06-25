package postgres

import (
	"fmt"

	_ "github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/applications-service/pkg/storage"
)

const (
	selectService = `
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
FROM service
WHERE name = $1
  AND sup_id IN (
    SELECT id FROM supervisor
    WHERE member_id = $2
  )
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
FROM service AS s
LEFT JOIN service_group AS sg
  ON s.group_id = sg.id
LEFT JOIN deployment AS d
  ON s.deployment_id = d.id
LEFT JOIN supervisor AS sup
  ON s.sup_id = sup.id
 %s
ORDER BY %s %s
LIMIT $1
OFFSET $2
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
		services              []*storage.Service
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
	return services, err
}

// getServiceFromUniqueFields retrieves a service from the db without the need
// of an id, it is based on the unique fields, name and member id
func (db *Postgres) getServiceFromUniqueFields(name, member string) (*service, bool) {
	var svc service
	err := db.SelectOne(&svc, selectService, name, member)
	if err != nil {
		return nil, false
	}

	return &svc, true
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
