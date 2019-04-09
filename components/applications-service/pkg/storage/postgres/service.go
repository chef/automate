package postgres

import (
	"fmt"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
)

const (
	selectService = `
SELECT * FROM service
WHERE origin = $1
  AND name = $2
  AND sup_id IN (
    SELECT id FROM supervisor
    WHERE member_id = $3
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
)

// GetServices returns a list of services
func (db *postgres) GetServices(
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
		sortField,
		sortOrder,
	)

	_, err = db.DbMap.Select(&services, formattedQuery, pageSize, offset)
	return services, err
}

// getServiceFromUniqueFields retreives a service from the db without the need of an id
func (db *postgres) getServiceFromUniqueFields(origin, name, member string) (*service, bool) {
	var svc service
	err := db.SelectOne(&svc, selectService, origin, name, member)
	if err != nil {
		return nil, false
	}

	return &svc, true
}

// buildWhereConstraintsFromFilters converts the provided filters into SQL 'WHERE' constraints
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
			WhereConstraints = WhereConstraints + buildORConstraintsFromValues("group_id", values)

		// TODO @afiune uncomment this code to power the right side panel Health Status Filters
		//case "health":
		//WhereConstraints = WhereConstraints + buildORConstraintsFromValues("health", values)

		default:
			return "", errors.Errorf("invalid filter. (%s:%s)", filter, values)
		}
	}

	return WhereConstraints, nil
}

// buildORConstraintsFromValues converts the provided values from a single filter
// into SQL 'OR' constraints
func buildORConstraintsFromValues(field string, values []string) string {
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
