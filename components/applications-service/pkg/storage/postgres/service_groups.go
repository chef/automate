package postgres

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
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
	// @afiune IDEA: Bring back the main query. The explanation why is below.
	mainSelectServiceGroupHealth = `
SELECT *
  ,(CASE WHEN health_critical > 0 THEN '1_CRITICAL'
         WHEN health_unknown  > 0 THEN '2_UNKNOWN'
         WHEN health_warning  > 0 THEN '3_WARNING'
         ELSE '4_OK' END ) as health
FROM (
  SELECT  sg.id
    ,sg.deployment_id
    ,sg.name as name
    ,COUNT(s.health) FILTER (WHERE s.health = 'OK') AS health_ok
    ,COUNT(s.health) FILTER (WHERE s.health = 'CRITICAL') AS health_critical
    ,COUNT(s.health) FILTER (WHERE s.health = 'WARNING') AS health_warning
    ,COUNT(s.health) FILTER (WHERE s.health = 'UNKNOWN') AS health_unknown
    ,COUNT(s.health) AS health_total
    ,round((COUNT(s.health) FILTER (WHERE s.health = 'OK')
          / COUNT(s.health)::float) * 100) as percent_ok
    ,(SELECT array_agg(DISTINCT CONCAT (s.package_ident))
        FROM service AS s
       WHERE s.group_id = sg.id) AS releases
    ,d.app_name as app_name
    ,d.environment as environment
  FROM service_group AS sg
  JOIN service AS s
       ON s.group_id = sg.id
  JOIN deployment as d
       ON sg.deployment_id = d.id

  -- @afiune my idea here is that we can put back the main big select that we moved to be a view
  -- so that you can inject the filters through WHERE statements, these statements can be composed
  -- in the same maner we are building the where statements in the services.go file at:
  -- => https://github.com/chef/automate/blob/master/components/applications-service/pkg/storage/postgres/services.go#L241-L258

  WHERE
  -- Here is where we can add filters specifically for service
    sg.id IN (SELECT DISTINCT group_id FROM service WHERE origin = 'custom' AND name = 'nginx')

  AND
  -- Here is where we can add filters specifically for deployment
    sg.deployment_id IN (SELECT DISTINCT id FROM deployment WHERE app_name = 'demo' AND environment = 'demo')

  AND
  -- Here is where we can add filters specifically for service_group itself
    sg.name = 'nginx.dev'

  GROUP BY sg.id, sg.deployment_id, sg.name, d.app_name, d.environment
) as service_groups_health_calculation
`

	// The service-group queries are based on a view which:
	// 1) Counts health of services (How many ok, critical, warning and unknown) and its total
	// 2) Calculates the percentage of services with an ok health.
	// 3) Provides an array of all service releases within a service group
	// 4) Determines the overall status of the service group

	// TODO: Update this query once we understand better the deploying status
	selectServiceGroupsHealthCounts = `
  SELECT COUNT(*) AS total
  ,COUNT(*) FILTER (
             WHERE health_critical > 0
       ) AS critical
  ,COUNT(*) FILTER (
             WHERE health_unknown  > 0
               AND health_critical = 0
      ) AS unknown
  ,COUNT(*) FILTER (
             WHERE health_warning  > 0
               AND health_critical = 0
               AND health_unknown  = 0
       ) AS warning
  ,COUNT(*) FILTER (
             WHERE health_ok > 0
               AND health_critical = 0
               AND health_warning  = 0
               AND health_unknown  = 0
       ) AS ok
FROM (` + mainSelectServiceGroupHealth + `) AS service_group_health_counts
`

	selectServiceGroupHealthWithPageSort = `
SELECT * FROM (` + mainSelectServiceGroupHealth + `) AS service_group_health
 ORDER BY %s
 LIMIT $1
OFFSET $2
`
	selectServiceGroupHealthFilterCRITICAL = `
SELECT * FROM (` + mainSelectServiceGroupHealth + `) AS service_group_health
 WHERE health_critical > 0
 ORDER BY %s
 LIMIT $1
OFFSET $2
`
	selectServiceGroupHealthFilterUNKNOWN = `
SELECT * FROM (` + mainSelectServiceGroupHealth + `) AS service_group_health
 WHERE health_unknown  > 0
   AND health_critical = 0
 ORDER BY %s
 LIMIT $1
OFFSET $2
`
	selectServiceGroupHealthFilterWARNING = `
SELECT * FROM (` + mainSelectServiceGroupHealth + `) AS service_group_health
 WHERE health_warning  > 0
   AND health_critical = 0
   AND health_unknown  = 0
 ORDER BY %s
 LIMIT $1
OFFSET $2
`
	selectServiceGroupHealthFilterOK = `
SELECT * FROM (` + mainSelectServiceGroupHealth + `) AS service_group_health
 WHERE health_ok > 0
   AND health_critical = 0
   AND health_warning  = 0
   AND health_unknown  = 0
 ORDER BY %s
 LIMIT $1
OFFSET $2
`

	selectServiceGroupsTotalCount = `
SELECT count(*)
  FROM service_group;
`
)

// serviceGroupHealth matches the results from the SELECT GroupHealth Query
type serviceGroupHealth struct {
	ID             int32          `db:"id"`
	Releases       pq.StringArray `db:"releases"`
	Name           string         `db:"name"`
	DeploymentID   int32          `db:"deployment_id"`
	Health         string         `db:"health"`
	HealthOk       int32          `db:"health_ok"`
	HealthCritical int32          `db:"health_critical"`
	HealthWarning  int32          `db:"health_warning"`
	HealthUnknown  int32          `db:"health_unknown"`
	HealthTotal    int32          `db:"health_total"`
	PercentOk      int32          `db:"percent_ok"`
	Application    string         `db:"app_name"`
	Environment    string         `db:"environment"`

	// These fields are not mapped to any database field,
	// they are here just to help us internally
	composedPackages []string
	composedReleases []string
}

// getServiceFromUniqueFields retrieve a service from the db without the need of an id
func (db *Postgres) GetServiceGroups(
	sortField string, sortAsc bool,
	page int32, pageSize int32,
	filters map[string][]string) ([]*storage.ServiceGroupDisplay, error) {

	// Decrement one to the page since we must start from zero
	page = page - 1
	offset := pageSize * page
	var (
		sgHealth    []*serviceGroupHealth
		selectQuery string = selectServiceGroupHealthWithPageSort
		sortOrder   string
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

	// Formatting our Query with sort field and sort order
	formattedQuery := fmt.Sprintf(selectQuery, formatSortFields(sortField, sortOrder))

	_, err = db.DbMap.Select(&sgHealth, formattedQuery, pageSize, offset)
	if err != nil {
		return nil, errors.Wrap(err, "Unable to retrieve service groups from the database")
	}
	sgDisplay := make([]*storage.ServiceGroupDisplay, len(sgHealth))
	for i, sgh := range sgHealth {
		sgDisplay[i] = &storage.ServiceGroupDisplay{
			ID:               sgh.ID,
			Release:          sgh.ReleaseString(),
			Package:          sgh.PackageString(),
			Name:             sgh.Name,
			DeploymentID:     sgh.DeploymentID,
			HealthStatus:     sgh.Health[2:], // Quick trick to remove health order from DB
			HealthPercentage: sgh.PercentOk,
			Application:      sgh.Application,
			Environment:      sgh.Environment,
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

func (db *Postgres) GetServiceGroupsCount() (int32, error) {
	count, err := db.DbMap.SelectInt(selectServiceGroupsTotalCount)
	return int32(count), err
}

// ReleaseString returns the release string of the service group.
//
// Example: From the package_ident:       'core/redis/0.1.0/20200101000000'
//          The release string should be: '0.1.0/20200101000000'
//
// Criteria:
// * if the group of services has only one release, it returns it.
// * if it has several releases then it returns the string 'x releases'
// * if it has no releases, (which it is impossible but we should protect against it) it
//   will return the string 'unknown'
func (sgh *serviceGroupHealth) ReleaseString() string {
	sgh.breakReleases()
	switch rNumber := len(sgh.composedReleases); rNumber {
	case 0:
		return "unknown"
	case 1:
		return sgh.composedReleases[0]
	default:
		return fmt.Sprintf("%d releases", rNumber)
	}
}

// PackageString returns the package name string of the service group.
//
// Example: From the package_ident:            'core/redis/0.1.0/20200101000000'
//          The package name string should be: 'core/redis'
//
// Criteria:
// * if the group of services has only one package name, it returns it.
// * if it has several package names then it returns the string 'x packages'
//   (this case could happen when two services are running a package from different origins)
// * if it has no package name, (which it is impossible but we should protect against it) it
//   will return the string 'unknown'
func (sgh *serviceGroupHealth) PackageString() string {
	sgh.breakReleases()
	switch pNumber := len(sgh.composedPackages); pNumber {
	case 0:
		return "unknown"
	case 1:
		return sgh.composedPackages[0]
	default:
		return fmt.Sprintf("%d packages", pNumber)
	}
}

// breakReleases is an internal function that is being called by PackageString() and
// ReleaseString() to break down the multiple releases that a service group could have.
// The function is designed to run only once for performance purposes, avoiding breaking
// the releases down on each call.
func (sgh *serviceGroupHealth) breakReleases() {
	// If there are no releases to break, stop processing
	if len(sgh.Releases) == 0 {
		return
	}

	// If either one of the composed releases/packages have memebers
	// it means that this function has ran already and therefore we
	// wont continue processing
	// TODO @afiune we could have a bool flag to force the execution
	if len(sgh.composedReleases) != 0 || len(sgh.composedPackages) != 0 {
		return
	}

	// We can break down the releases
	for _, release := range sgh.Releases {

		identFields := strings.Split(release, "/")
		// This error "should" never happen since this will indicate that
		// Habitat sent a malformed package_ident, still we will be covered
		// and will report it as an error in the logs
		if len(identFields) != 4 {
			log.WithFields(log.Fields{
				"func":           "serviceGroupHealth.breakReleases()",
				"release_fields": identFields,
			}).Error("Malformed 'package ident' from database.")

			continue
		}

		var (
			pkg = identFields[0] + "/" + identFields[1]
			rel = identFields[2] + "/" + identFields[3]
		)

		if !inArray(pkg, sgh.composedPackages) {
			sgh.composedPackages = append(sgh.composedPackages, pkg)
		}

		if !inArray(rel, sgh.composedReleases) {
			sgh.composedReleases = append(sgh.composedReleases, rel)
		}
	}
}

// GetServiceGroupsHealthCounts retrieves the health counts from all service groups in the database
func (db *Postgres) GetServiceGroupsHealthCounts() (*storage.HealthCounts, error) {
	var sgHealthCounts storage.HealthCounts
	err := db.SelectOne(&sgHealthCounts, selectServiceGroupsHealthCounts)
	if err != nil {
		return nil, err
	}

	return &sgHealthCounts, nil
}

// ServiceGroupExists returns the name of the service group if it exists
func (db *Postgres) ServiceGroupExists(id string) (string, bool) {
	if id == "" {
		return "", false
	}
	var sgName string
	err := db.SelectOne(&sgName, "SELECT name FROM service_group WHERE id = $1", id)
	if err != nil {
		return "", false
	}

	return sgName, true
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

// formatSortFields returns a customized ORDER BY statement from the provided sort field,
// if the field doesn't require a special ORDER BY it will return the same field
//
// Criteria:
// * When sort by percent_ok, add a second order by group health criticality.
// * When sort by group health criticality, add a second order by percent_ok.
//
// The second ORDER BY added with this function will be static and it will always be ascending
// since we want to always give more priority to the things that are in a critical state
func formatSortFields(field, sort string) string {
	sortField := field + " " + sort

	switch field {
	case "health":
		return sortField + ", percent_ok ASC"
	case "percent_ok":
		return sortField + ", health ASC"
	default:
		return sortField
	}
}

func inArray(value string, array []string) bool {
	for _, v := range array {
		if value == v {
			return true
		}
	}
	return false
}

func (db *Postgres) getServiceGroup(id int32) (*serviceGroup, error) {
	var sg serviceGroup
	err := db.SelectOne(&sg,
		"SELECT id, deployment_id, name FROM service_group WHERE id = $1", id)
	if err != nil {
		return nil, errors.Wrap(err, "unable to retrieve service group from the database")
	}

	return &sg, nil
}
func (db *Postgres) getServiceGroupID(name string) (int32, bool) {
	var gid int32
	err := db.SelectOne(&gid,
		"SELECT id FROM service_group WHERE name = $1",
		name,
	)
	if err != nil {
		return gid, false
	}

	return gid, true
}
