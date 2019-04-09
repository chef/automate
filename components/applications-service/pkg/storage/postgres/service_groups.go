package postgres

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/lib/pq"
	"github.com/pkg/errors"
)

// Service Group Health Status
// ---------------------------------------------------------------------
// Status    | Criteria
// --------- ------------------------------------------------------------
// DEPLOYING | If one or more services are 'Deploying'
// CRITICAL  | Else if one or more services are 'Critical'
// UNKNOWN   | Else if one or more services are 'Unknown'
// WARNING   | Else if one or more services are 'Warning'
// OK        | Else if all services are 'Ok'
// ---------------------------------------------------------------------
const (
	selectServiceGroupHealth = `
SELECT sg.id
  , sg.deployment_id
  , sg.name as name
  , COUNT(s.health) FILTER (WHERE s.health = 'OK') AS health_ok
  , COUNT(s.health) FILTER (WHERE s.health = 'CRITICAL') AS health_critical
  , COUNT(s.health) FILTER (WHERE s.health = 'WARNING') AS health_warning
  , COUNT(s.health) FILTER (WHERE s.health = 'UNKNOWN') AS health_unknown
  , COUNT(s.health) AS health_total
  , round((COUNT(s.health) FILTER (WHERE s.health = 'OK') / COUNT(s.health)::float) * 100) as percent_ok
  , (
      SELECT array_agg( DISTINCT CONCAT (s.origin, '/', s.name, '/', s.version, '/', s.release) )
      FROM service AS s
      WHERE s.group_id = sg.id
    ) AS releases
FROM service_group AS sg
JOIN service AS s
ON s.group_id = sg.id
GROUP BY sg.id, sg.deployment_id`

	serviceGroupPageSort = `
ORDER BY %s %s
LIMIT $1
OFFSET $2`

	// TODO: Update this query once we understand better the deploying status
	selectServiceGroupsHealthCounts = `
SELECT COUNT(*) AS total
  , COUNT(*) FILTER (
      WHERE health_critical > 0
    ) AS critical
  , COUNT(*) FILTER (
      WHERE health_unknown  > 0
        AND health_critical = 0
    ) AS unknown
  , COUNT(*) FILTER (
      WHERE health_warning  > 0
        AND health_critical = 0
        AND health_unknown  = 0
    ) AS warning
  , COUNT(*) FILTER (
      WHERE health_ok > 0
        AND health_critical = 0
        AND health_warning  = 0
        AND health_unknown  = 0
    ) AS ok
FROM (` + selectServiceGroupHealth + `) AS service_groups_health_counts
`
	selectServiceGroupHealthFilterCRITICAL = `
SELECT * FROM (` + selectServiceGroupHealth + serviceGroupPageSort + `) AS service_groups_health
WHERE health_critical > 0
`
	selectServiceGroupHealthFilterUNKNOWN = `
SELECT * FROM (` + selectServiceGroupHealth + serviceGroupPageSort + `) AS service_groups_health
WHERE health_unknown  > 0
  AND health_critical = 0
`
	selectServiceGroupHealthFilterWARNING = `
SELECT * FROM (` + selectServiceGroupHealth + serviceGroupPageSort + `) AS service_groups_health
WHERE health_warning  > 0
  AND health_critical = 0
  AND health_unknown  = 0
`
	selectServiceGroupHealthFilterOK = `
SELECT * FROM (` + selectServiceGroupHealth + serviceGroupPageSort + `) AS service_groups_health
WHERE health_ok > 0
  AND health_critical = 0
  AND health_warning  = 0
  AND health_unknown  = 0
`
)

// serviceGroupHealth matches the results from the SELECT GroupHealth Query
type serviceGroupHealth struct {
	ID             int32          `db:"id"`
	Releases       pq.StringArray `db:"releases"`
	Name           string         `db:"name"`
	DeploymentID   int32          `db:"deployment_id"`
	HealthOk       int32          `db:"health_ok"`
	HealthCritical int32          `db:"health_critical"`
	HealthWarning  int32          `db:"health_warning"`
	HealthUnknown  int32          `db:"health_unknown"`
	HealthTotal    int32          `db:"health_total"`
	PercentOk      int32          `db:"percent_ok"`
}

// OverallHealth is the logic that calculates the overall health of the service group
func (sgh *serviceGroupHealth) OverallHealth() string {
	if sgh.HealthCritical > 0 {
		return storage.Critical
	}

	if sgh.HealthUnknown > 0 {
		return storage.Unknown
	}

	if sgh.HealthWarning > 0 {
		return storage.Warning
	}

	// We are only ok if we are 100% ok
	return storage.Ok
}

// getServiceFromUniqueFields retrieve a service from the db without the need of an id
func (db *postgres) GetServiceGroups(
	sortField string, sortAsc bool,
	page int32, pageSize int32,
	filters map[string][]string) ([]*storage.ServiceGroupDisplay, error) {

	// Decrement one to the page since we must start from zero
	page = page - 1
	offset := pageSize * page
	var (
		sgHealth    []*serviceGroupHealth
		sortOrder   string
		selectQuery string = selectServiceGroupHealth + serviceGroupPageSort
		err         error
	)

	if sortAsc {
		sortOrder = "ASC"
	} else {
		sortOrder = "DESC"
	}

	for filter, values := range filters {
		if len(values) == 0 {
			continue
		}

		switch filter {
		case "status", "STATUS":
			// @afiune What if the user specify more than one Status Filter?
			// status=["ok", "critical"]
			selectQuery, err = queryFromStatusFilter(values[0])
			if err != nil {
				return nil, err
			}

		default:
			return nil, errors.Errorf("invalid filter. (%s:%s)", filter, values)
		}
	}

	formattedQuery := fmt.Sprintf(selectQuery, sortField, sortOrder)
	_, err = db.DbMap.Select(&sgHealth, formattedQuery, pageSize, offset)
	if err != nil {
		return nil, errors.Wrap(err, "Unable to retrieve service groups from the database")
	}
	sgDisplay := make([]*storage.ServiceGroupDisplay, len(sgHealth))
	for i, sgh := range sgHealth {
		sgDisplay[i] = &storage.ServiceGroupDisplay{
			ID:               sgh.ID,
			Release:          sgh.ReleaseString(),
			Name:             sgh.Name,
			DeploymentID:     sgh.DeploymentID,
			HealthStatus:     sgh.OverallHealth(),
			HealthPercentage: sgh.PercentOk,
			ServicesHealthCounts: storage.HealthCounts{
				Ok:       sgh.HealthOk,
				Critical: sgh.HealthCritical,
				Warning:  sgh.HealthWarning,
				Unknown:  sgh.HealthUnknown,
				Total:    sgh.HealthTotal,
			},
		}
	}

	return sgDisplay, nil
}

// ReleaseString returns the release string of the service group, our criteria are:
// * if the group of services has only one release, it returns it.
// * if it has several releases then it returns the string 'Several'
// * if it has no releases, (which it is impossible but we should protect against it) it
//   will return the string 'Unknown'
func (sgh *serviceGroupHealth) ReleaseString() string {
	switch rNumber := len(sgh.Releases); rNumber {
	case 0:
		return "Unknown"
	case 1:
		return sgh.Releases[0]
	default:
		return fmt.Sprintf("Several (%d)", rNumber)
	}
}

// GetServiceGroupsHealthCounts retrieves the health counts from all service groups in the database
func (db *postgres) GetServiceGroupsHealthCounts() (*storage.HealthCounts, error) {
	var sgHealthCounts storage.HealthCounts
	err := db.SelectOne(&sgHealthCounts, selectServiceGroupsHealthCounts)
	if err != nil {
		return nil, err
	}

	return &sgHealthCounts, nil
}

func queryFromStatusFilter(text string) (string, error) {
	// Make sure that we always have uppercase text
	switch strings.ToUpper(text) {
	case storage.Ok:
		return selectServiceGroupHealthFilterOK, nil
	case storage.Critical:
		return selectServiceGroupHealthFilterCRITICAL, nil
	case storage.Warning:
		return selectServiceGroupHealthFilterWARNING, nil
	case storage.Unknown:
		return selectServiceGroupHealthFilterUNKNOWN, nil
	default:
		return "", errors.Errorf("invalid status filter '%s'", text)
	}
}
