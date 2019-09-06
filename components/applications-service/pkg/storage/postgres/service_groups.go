package postgres

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/pgutils"
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
	// The service-group queries are based on a view which:
	// 1) Counts health of services (How many ok, critical, warning and unknown) and its total
	// 2) Calculates the percentage of services with an ok health.
	// 3) Provides an array of all service releases within a service group
	// 4) Determines the overall status of the service group

	// TODO: Update this query once we understand better the deploying status
	selectServiceGroupsHealthCountsFirst = `
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
	FROM ( `
	selectServiceGroupsHealthCountsSecond = ` ) AS service_group_health_counts `

	selectServiceGroupHealthFirst = `
  SELECT *
         ,(CASE WHEN health_critical > 0 THEN '1_CRITICAL'
                WHEN health_unknown  > 0 THEN '2_UNKNOWN'
                WHEN health_warning  > 0 THEN '3_WARNING'
                ELSE '4_OK' END ) as health
          ,(SELECT array_agg(DISTINCT CONCAT (s_for_releases.package_ident))
                FROM service_full AS s_for_releases
                WHERE s_for_releases.service_group_id = sghc.id `

	selectServiceGroupHealthSecond = `) AS releases
    FROM (SELECT  s.service_group_id as id
      ,s.service_group_name as name
      ,COUNT(s.health) FILTER (WHERE s.health = 'OK') AS health_ok
      ,COUNT(s.health) FILTER (WHERE s.health = 'CRITICAL') AS health_critical
      ,COUNT(s.health) FILTER (WHERE s.health = 'WARNING') AS health_warning
      ,COUNT(s.health) FILTER (WHERE s.health = 'UNKNOWN') AS health_unknown
      ,COUNT(s.health) AS health_total
      ,round((COUNT(s.health) FILTER (WHERE s.health = 'OK')
            / COUNT(s.health)::float) * 100) as percent_ok
      ,s.application as app_name
      ,s.environment as environment
      ,s.service_group_name_suffix as name_suffix
			,COUNT(s.disconnected) FILTER (WHERE s.disconnected = 'true') AS disconnected_count
    FROM service_full AS s
`
	groupByPart = `
  GROUP BY s.service_group_id, s.application, s.environment, s.service_group_name, s.service_group_name_suffix ) as sghc
`
	selectServiceGroupHealthFilterCRITICAL = `
  WHERE
   health_critical > 0
`
	selectServiceGroupHealthFilterUNKNOWN = `
  WHERE
   health_unknown  > 0
   AND health_critical = 0
`
	selectServiceGroupHealthFilterWARNING = `
  WHERE
   health_warning  > 0
   AND health_critical = 0
   AND health_unknown  = 0
`
	selectServiceGroupHealthFilterOK = `
  WHERE
   health_ok > 0
   AND health_critical = 0
   AND health_warning  = 0
   AND health_unknown  = 0
 `
	paginationSorting = `
  ORDER BY %s
  LIMIT $1
  OFFSET $2 `

	selectServiceGroupsTotalCount = `
SELECT COUNT(DISTINCT service_group_id) FROM service_full;
`
)

// serviceGroupHealth matches the results from the SELECT GroupHealth Query
type serviceGroupHealth struct {
	ID             string         `db:"id"`
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
	//We are not returning this just using it for filters
	NameSuffix        string `db:"name_suffix"`
	DisconnectedCount int32  `db:"disconnected_count"`

	// These fields are not mapped to any database field,
	// they are here just to help us internally
	composedPackages []string
	composedReleases []string
}

func (db *Postgres) GetServiceGroups(
	sortField string, sortAsc bool,
	page int32, pageSize int32,
	filters map[string][]string) ([]*storage.ServiceGroupDisplay, error) {

	// Decrement one to the page since we must start from zero
	page = page - 1
	offset := pageSize * page
	var (
		sgHealth  []*serviceGroupHealth
		sortOrder string
		err       error
	)

	if sortAsc {
		sortOrder = "ASC"
	} else {
		sortOrder = "DESC"
	}

	whereQuery, err := formatQueryFilters(filters, true)
	if err != nil {
		return nil, err
	}
	whereQuery = whereQuery + paginationSorting

	// Formatting our Query with sort field and sort order
	formattedQuery := fmt.Sprintf(whereQuery, formatSortFields(sortField, sortOrder))

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
			DisconnectedCount: sgh.DisconnectedCount,
		}
	}

	return sgDisplay, nil
}

func formatQueryFilters(filters map[string][]string, includeStatusFilter bool) (string, error) {
	var (
		err                 error
		statusQuery         string
		whereQuery          string
		selectAllPartsQuery string
		selectQuery         string
	)
	packageWhereQuery := ``
	first := true
	statusFilter := false
	for filter, values := range filters {
		if len(values) == 0 {
			continue
		}
		switch filter {
		case "status", "STATUS":
			// @afiune What if the user specify more than one Status Filter?
			// status=["ok", "critical"]
			if includeStatusFilter {
				// We do not want to include the status filter for the health counts
				// because we want the counts of all statuses (statii? statera? I leave this for the linguists)
				statusFilter = true
				statusQuery, err = queryFromStatusFilter(values[0])
				if err != nil {
					return "", err
				}
			}
		case "environment", "ENVIRONMENT":
			selectQuery, err = queryFromFieldFilter("s.environment", values, first)
			whereQuery = whereQuery + selectQuery

		case "application", "APPLICATION":
			selectQuery, err = queryFromFieldFilter("s.application", values, first)
			whereQuery = whereQuery + selectQuery
		case "group", "GROUP":
			selectQuery, err = queryFromFieldFilter("s.service_group_name_suffix", values, first)
			whereQuery = whereQuery + selectQuery
		case "origin", "ORIGIN":
			selectQuery, err := queryFromFieldFilter("s.origin", values, first)
			whereQuery = whereQuery + selectQuery
			q, err1 := queryFromFieldFilter("s_for_releases.origin", values, false)
			packageWhereQuery = packageWhereQuery + q
			if err != nil || err1 != nil {
				return "", err
			}
		case "service", "SERVICE":
			selectQuery, err = queryFromFieldFilter("s.name", values, first)
			whereQuery = whereQuery + selectQuery
			break
		case "site", "SITE":
			selectQuery, err = queryFromFieldFilter("s.site", values, first)
			whereQuery = whereQuery + selectQuery
		case "channel", "CHANNEL":
			selectQuery, err = queryFromFieldFilter("s.channel", values, first)
			whereQuery = whereQuery + selectQuery
			break
		case "version", "VERSION":
			selectQuery, err := queryFromFieldFilter("s.version", values, first)
			whereQuery = whereQuery + selectQuery
			q, err1 := queryFromFieldFilter("s_for_releases.version", values, false)
			packageWhereQuery = packageWhereQuery + q
			if err != nil || err1 != nil {
				return "", err
			}
		case "buildstamp", "BUILDSTAMP":
			selectQuery, err = queryFromFieldFilter("s.release", values, first)
			whereQuery = whereQuery + selectQuery
		default:
			return "", errors.Errorf("invalid filter. (%s:%s)", filter, values)
		}
		if err != nil {
			return "", err
		}
		if first && filter != "status" && filter != "STATUS" { //If status is first ignore it since it is special and happens outside the inner query.
			first = false
		}
	}
	if statusFilter {
		// The status query has to go outside of the inner query because the health count filters need to be calculated
		// in order to be used in the where clause
		selectAllPartsQuery = selectServiceGroupHealthFirst + packageWhereQuery + selectServiceGroupHealthSecond + whereQuery + groupByPart + statusQuery
	} else {
		selectAllPartsQuery = selectServiceGroupHealthFirst + packageWhereQuery + selectServiceGroupHealthSecond + whereQuery + groupByPart
	}
	return selectAllPartsQuery, nil
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

	// If either one of the composed releases/packages have members
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
func (db *Postgres) GetServiceGroupsHealthCounts(filters map[string][]string) (*storage.HealthCounts, error) {
	var sgHealthCounts storage.HealthCounts
	selectInnerQuery, err := formatQueryFilters(filters, false)
	if err != nil {
		return nil, err
	}
	selectQuery := selectServiceGroupsHealthCountsFirst + selectInnerQuery + selectServiceGroupsHealthCountsSecond
	err = db.SelectOne(&sgHealthCounts, selectQuery)
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
	err := db.SelectOne(&sgName, "SELECT service_group_name FROM service_full WHERE service_group_id = $1 LIMIT 1", id)
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

func queryFromFieldFilter(field string, arr []string, first bool) (string, error) {
	var condition string
	if first {
		condition = ` WHERE `
	} else { // not first, add on an AND before IN part
		condition = ` AND `
	}
	if !pgutils.IsSqlSafe(field) {
		return "", &errorutils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in field: %s", field)}
	}
	condition = condition + fmt.Sprintf(" %s IN (", field)
	if len(arr) > 0 {
		for index, item := range arr {
			condition += fmt.Sprintf("'%s'", pgutils.EscapeLiteralForPG(item))
			if index < len(arr)-1 {
				condition += ","
			}
		}
	} else {
		condition += "''"
	}
	return condition + ")", nil
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
