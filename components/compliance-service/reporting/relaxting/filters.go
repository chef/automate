package relaxting

import (
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"strings"
	"time"
)

//getFiltersQueryForStatsSummaryControls - builds up an elasticsearch query filter based on the filters map that is passed in
//  arguments: filters - is a map of filters that serve as the source for generated es query filters
//             latestOnly - specifies whether or not we are only interested in retrieving only the latest report
//  return *elastic.BoolQuery
func (backend ES2Backend) getFiltersQueryForStatsSummaryControls(filters map[string][]string, latestOnly bool) *elastic.BoolQuery {
	utils.DeDupFilters(filters)
	logrus.Debugf("????? Called getFiltersQueryForStatsSummaryControls with filters=%+v, latestOnly=%t", filters, latestOnly)
	boolQuery := elastic.NewBoolQuery()

	//boolQuery for nodes level filters
	boolQuery = boolQuery.Must(elastic.NewNestedQuery("nodes", getBoolQueryForNestedNodes(filters)))

	if len(filters["control_name"]) > 0 {
		termQuery := newTermQueryFromFilter("title.lower", filters["control_name"])
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["profile_name"]) > 0 {
		termQuery := newTermQueryFromFilter("profiles.title.lower", filters["profile_name"])
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["control"]) > 0 {
		termQuery := elastic.NewTermsQueryFromStrings("control_id", filters["control"]...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["profile_id"]) > 0 {
		termQuery := elastic.NewTermsQueryFromStrings("profile.profile_id", filters["profile_id"]...)
		boolQuery = boolQuery.Must(termQuery)
	}

	if len(filters["start_time"]) > 0 || len(filters["end_time"]) > 0 {
		endTime := firstOrEmpty(filters["end_time"])
		startTime := firstOrEmpty(filters["start_time"])
		timeRangeQuery := elastic.NewRangeQuery("end_time")
		if len(startTime) > 0 {
			timeRangeQuery.Gte(startTime)
		}
		if len(endTime) > 0 {
			timeRangeQuery.Lte(endTime)
		}

		boolQuery = boolQuery.Must(timeRangeQuery)
	}

	if len(filters["end_time"]) == 0 && len(filters["start_time"]) == 0 {
		// If we don't have an end_time filter, we limit to last 24 hours timeframe
		timeRangeQuery := elastic.NewRangeQuery("end_time")
		timeRangeQuery.Gt(time.Now().Add(-24 * time.Hour).UTC().Format(time.RFC3339))
		boolQuery = boolQuery.Must(timeRangeQuery)
	}

	if latestOnly {
		// only if there is no job_id filter set, do we want the daily latest
		setFlags, err := filterQueryChange(firstOrEmpty(filters["end_time"]), firstOrEmpty(filters["start_time"]))
		if err != nil {
			errors.Errorf("cannot parse the time %v", err)
			return nil
		}
		for _, flag := range setFlags {
			termQuery := elastic.NewTermsQuery(flag, true)
			boolQuery = boolQuery.Must(termQuery)
		}

	}

	// Going through all filters to find the ones prefixed with 'control_tag', e.g. 'control_tag:nist'
	for filterType := range filters {
		if strings.HasPrefix(filterType, "control_tag:") {
			_, tagKey := leftSplit(filterType, ":")
			termQuery := newNestedQueryForControlTags(tagKey, filters[filterType])
			boolQuery = boolQuery.Must(termQuery)
		}
	}

	return boolQuery

}

func getBoolQueryForNestedNodes(filters map[string][]string) *elastic.BoolQuery {

	nestedBoolquery := elastic.NewBoolQuery()

	// These are filter types where we need to use nested nodes term queries for comp-1-control-*
	filterTypes := []string{"environment", "organization", "chef_server", "chef_tags",
		"policy_group", "policy_name", "status", "node_name", "platform", "platform_with_version",
		"role", "recipe", "inspec_version", "ipaddress"}

	for _, filterType := range filterTypes {
		if len(filters[filterType]) > 0 {
			ESFieldName := getESFieldNameForControlIndex(filterType)
			termQuery := newTermQueryFromFilter(ESFieldName, filters[filterType])
			nestedBoolquery = nestedBoolquery.Must(termQuery)
		}
	}

	if len(filters["node_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("nodes.node_uuid", stringArrayToInterfaceArray(filters["node_id"])...)
		nestedBoolquery = nestedBoolquery.Must(termQuery)
	}

	if len(filters["job_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("nodes.job_uuid", stringArrayToInterfaceArray(filters["job_id"])...)
		nestedBoolquery = nestedBoolquery.Must(termQuery)
	}

	return nestedBoolquery

}

// Returns an ElasticSearch nested query to filter reports by control tags
func newNestedQueryForControlTags(tagKey string, tagValues []string) *elastic.NestedQuery {
	refinedValues := make([]string, 0, 0)
	ESFieldPath := "string_tags"
	ESFieldTagKey := "string_tags.key.lower"
	ESFieldTagValues := "string_tags.values.lower"

	// Add ElasticSearch query filter for the control tag key
	tagKey = strings.ToLower(tagKey)
	boolQuery := elastic.NewBoolQuery()
	if containsWildcardChar(tagKey) {
		boolQuery.Must(elastic.NewWildcardQuery(ESFieldTagKey, tagKey))
	} else {
		boolQuery.Must(elastic.NewTermsQuery(ESFieldTagKey, tagKey))
	}

	// Add ElasticSearch query filters for the control tag value(s)
	insideBoolQuery := elastic.NewBoolQuery()
	insideBool := false
	emptyValuesRequested := false
	for _, tagValue := range tagValues {
		tagValue = strings.ToLower(tagValue)
		if containsWildcardChar(tagValue) {
			insideBoolQuery.Should(elastic.NewWildcardQuery(ESFieldTagValues, tagValue))
			insideBool = true
		} else if tagValue == "" {
			emptyValuesRequested = true
		} else {
			refinedValues = append(refinedValues, tagValue)
		}
	}
	// Here we handle control tag value(s) without wildcard characters
	if len(refinedValues) > 0 {
		termQuery := elastic.NewTermsQuery(ESFieldTagValues, stringArrayToInterfaceArray(refinedValues)...)
		insideBoolQuery.Should(termQuery)
		insideBool = true
	}
	// Here we handle the case where we want a tag key with NO values
	if emptyValuesRequested {
		existsQuery := elastic.NewExistsQuery(ESFieldTagValues)
		insideBoolQuery.Should(elastic.NewBoolQuery().MustNot(existsQuery))
		insideBool = true
	}
	if insideBool {
		boolQuery.Must(insideBoolQuery)
	}

	nestedQuery := elastic.NewNestedQuery(ESFieldPath, boolQuery)
	return nestedQuery
}

func getESFieldNameForControlIndex(filterType string) string {
	ESFieldName := filterType
	switch filterType {
	case "chef_server":
		ESFieldName = "nodes.source_fqdn.lower"
	case "inspec_version":
		ESFieldName = "nodes.version.lower"
	case "organization":
		ESFieldName = "nodes.organization_name.lower"
	case "platform":
		ESFieldName = "nodes.platform.name.lower"
	case "platform_with_version":
		ESFieldName = "nodes.platform.full.lower"
	case "recipe":
		ESFieldName = "nodes.recipes.lower"
	case "role":
		ESFieldName = "nodes.roles.lower"
	case "environment":
		ESFieldName = "nodes.environment.lower"
	case "chef_tags":
		ESFieldName = "nodes.chef_tags.lower"
	case "policy_group":
		ESFieldName = "nodes.policy_group.lower"
	case "policy_name":
		ESFieldName = "nodes.policy_name.lower"
	case "node_name":
		ESFieldName = "nodes.node_name.lower"
	}

	return ESFieldName
}
