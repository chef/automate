package pgdb

import (
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func mergeFilters(mergeableFilters []*common.Filter) ([]common.Filter, error) {
	filterMap := make(map[string]common.Filter)

	for _, mf := range mergeableFilters {
		var values []string
		key := mf.Key

		if f, found := filterMap[key]; found {
			if f.Exclude != mf.Exclude {
				return nil, errors.New("Filters are not allowed to be inclusive and exclusive on the same field.")
			}
			values = append(f.Values, mf.Values...)
		} else {
			values = mf.Values
		}

		filterMap[key] = common.Filter{Key: key, Values: values, Exclude: mf.Exclude}
	}

	filters := make([]common.Filter, 0, len(filterMap))
	for _, v := range filterMap {
		filters = append(filters, v)
	}
	return filters, nil
}

func tagFiltersToSQLConditions(tagFilters []common.Filter) ([]string, error) {
	sqlTagConditions := make([]string, len(tagFilters))
	for i, filter := range tagFilters {
		tagKeyFilter := strings.TrimPrefix(filter.Key, "tags:")
		newSqlTagCondition, err := patternMatchTags(tagKeyFilter, filter.Values, TagsTableAbbrev)
		if err != nil {
			return sqlTagConditions, errors.Wrap(err, "tagFiltersToSQLConditions error")
		}
		if filter.Exclude {
			newSqlTagCondition = fmt.Sprintf("NOT %s", newSqlTagCondition)
		}
		sqlTagConditions[i] = newSqlTagCondition
	}
	return sqlTagConditions, nil
}

// Takes a filter map (should be validated for content) and table abbreviation
// and returns WHERE and HAVING SQL conditions
func buildWhereHavingFilter(mergeableFilters []*common.Filter, tableAbbrev string, filterField map[string]string) (whereFilter string, havingFilter string, err error) {
	if len(mergeableFilters) == 0 {
		return "", "", nil
	}

	filters, err := mergeFilters(mergeableFilters)
	if err != nil {
		return "", "", errors.Wrap(err, "buildWhereHavingFilter error")
	}

	var conditions []string
	var tagFilters []common.Filter
	for _, filter := range filters {
		var newCondition string
		var err error
		if strings.HasPrefix(filter.Key, "tags:") {
			tagFilters = append(tagFilters, filter)
			continue
		} else {
			switch filterField[filter.Key] {
			case "":
				return "", "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported filter field: %s", filter.Key)}
			case "source_region", "name":
				newCondition, err = wherePatternMatch(filterField[filter.Key], filter.Values, tableAbbrev)
			case "statechange_timestamp", "last_contact", "last_run ->> 'EndTime'", "last_scan ->> 'EndTime'":
				newCondition, err = whereFieldBetween(filterField[filter.Key], filter.Values, tableAbbrev)
			case "manager_id":
				newCondition, err = whereNodeManagerNodeExists(filterField[filter.Key], filter.Values, tableAbbrev)
			case "project":
				newCondition, err = whereProjectsMatch(filterField[filter.Key], filter.Values, tableAbbrev)
			default:
				newCondition, err = whereFieldIn(filterField[filter.Key], filter.Values, tableAbbrev)
			}
		}

		if err != nil {
			return "", "", errors.Wrap(err, "buildWhereHavingFilter error")
		}

		if filter.Exclude {
			newCondition = fmt.Sprintf("NOT (%s)", newCondition)
		}

		conditions = append(conditions, newCondition)
	}

	if len(tagFilters) > 0 {
		tagConditions, err := tagFiltersToSQLConditions(tagFilters)
		if err != nil {
			return "", "", errors.Wrap(err, "buildWhereHavingFilter error building tags")
		}
		havingFilter = fmt.Sprintf("HAVING (%s)", strings.Join(tagConditions, ") AND ("))
	}
	if len(conditions) > 0 {
		whereFilter = fmt.Sprintf("WHERE (%s)", strings.Join(conditions, " AND "))
	}
	logrus.Debugf("buildWhereHavingFilter, whereFilter=%s, havingFilter=%s", whereFilter, havingFilter)
	return whereFilter, havingFilter, nil
}

// Builds an IN where condition like: j.parent_id IN ('e57605ed-bb8a-49b8-606c-af0e2b31b139')
func whereFieldIn(field string, arr []string, tableAbbrev string) (condition string, err error) {
	if !acceptedJSONFilterFields(field) {
		if !utils.IsSqlSafe(field) {
			return "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in field: %s", field)}
		}
	}
	condition += fmt.Sprintf("%s.%s IN (", tableAbbrev, field)
	if len(arr) > 0 {
		for index, item := range arr {
			condition += fmt.Sprintf("'%s'", utils.EscapeLiteralForPG(item))
			if index < len(arr)-1 {
				condition += ","
			}
		}
	} else {
		condition += "''"
	}
	return condition + ")", nil
}

func acceptedJSONFilterFields(field string) bool {
	switch field {
	case "last_run ->> 'EndTime'":
		return true
	case "last_scan ->> 'EndTime'":
		return true
	case "last_run ->> 'Status'":
		return true
	case "last_scan ->> 'Status'":
		return true
	case "last_run ->> 'PenultimateStatus'":
		return true
	case "last_scan ->> 'PenultimateStatus'":
		return true
	default:
		return false
	}
}

func whereFieldBetween(field string, arr []string, tableAbbrev string) (condition string, err error) {
	if !acceptedJSONFilterFields(field) {
		if !utils.IsSqlSafe(field) {
			return "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in field: %s", field)}
		}
	}
	if len(arr) != 2 {
		return "", &utils.InvalidError{Msg: fmt.Sprintf("Two params requires for whereFieldBetween %d", len(arr))}
	}
	for _, elem := range arr {
		_, err := time.Parse(time.RFC3339, elem)
		if err != nil {
			return "", &utils.InvalidError{Msg: fmt.Sprintf("Invalid timestamp: %s", elem)}
		}
	}
	condition = fmt.Sprintf("%s.%s BETWEEN SYMMETRIC '%s' AND '%s'", tableAbbrev, field, arr[0], arr[1])
	return condition, nil
}

// because of the n:m with the tags table and the ability to pass any number of tag queries
// that are ANDed or ORed, we use string_agg to denormalize the tags into a single string that
// can take multiple AND and OR conditions
func patternMatchTags(field string, arr []string, tableAbbrev string) (condition string, err error) {
	if !utils.IsSqlSafe(field) {
		return "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in field: %s", field)}
	}

	// if no value is provided for the tag, we query for any value
	if len(arr) == 0 {
		arr = append(arr, "")
	}

	valueLikes := make([]string, len(arr))
	for index, item := range arr {
		// concat needed as string_agg returns NULL when no tags exist and LIKE on NULL is not working
		valueLikes[index] = fmt.Sprintf("concat('',string_agg(',' || %s.key || ':' || %s.value, '')) LIKE '%%,%s:%s%%'", tableAbbrev, tableAbbrev, field, item)
	}

	return strings.Join(valueLikes, " OR "), nil
}

func wherePatternMatch(field string, arr []string, tableAbbrev string) (condition string, err error) {
	if !utils.IsSqlSafe(field) {
		return "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in field: %s", field)}
	}
	condition += fmt.Sprintf("%s.%s LIKE ", tableAbbrev, field)

	if len(arr) == 0 {
		condition += "''"
		return condition, nil
	}
	for index, item := range arr {
		item = strings.TrimSuffix(item, "*")
		condition += fmt.Sprintf("'%s%%'", utils.EscapeLiteralForPGPatternMatch(item))
		if index < len(arr)-1 {
			condition += fmt.Sprintf(" OR %s.%s LIKE ", tableAbbrev, field)
		}
	}

	return condition, nil
}

func whereNodeManagerNodeExists(field string, arr []string, tableAbbrev string) (condition string, err error) {
	if !utils.IsSqlSafe(field) {
		return "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in field: %s", field)}
	}

	if len(arr) == 0 {
		condition += fmt.Sprintf("not exists (select 1 from node_managers_nodes nmn where %s.id = nmn.node_id)", tableAbbrev)
		return condition, nil
	}

	for _, item := range arr {
		if !utils.IsSafeUUID(item) {
			return "", &utils.InvalidError{Msg: fmt.Sprintf("Unsupported character found in: %s", item)}
		}
	}

	condition += fmt.Sprintf("exists (select 1 from node_managers_nodes nmn where %s.id = nmn.node_id and nmn.%s = ANY('{%s}'::text[]))", tableAbbrev, field, strings.Join(arr, ","))
	return condition, nil
}

func whereProjectsMatch(_ string, arr []string, tableAbbrev string) (string, error) {
	var condition string

	if len(arr) == 0 {
		condition = fmt.Sprintf("not exists (select 1 from projects p join nodes_projects np on p.id = np.project_id where np.node_id = %s.id)", tableAbbrev)
	} else {
		value, err := pq.Array(arr).Value()
		if err != nil {
			return "", err
		}
		condition = fmt.Sprintf("exists (select 1 from projects p join nodes_projects np on p.id = np.project_id where np.node_id = %s.id and p.project_id = ANY('%s'::text[]))", tableAbbrev, value)
	}

	return condition, nil
}
