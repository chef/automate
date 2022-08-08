package relaxting

import (
	"context"
	"encoding/json"
	"fmt"
	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"strings"
	"time"
)

//getAssets Get Total Number of documents from the comp-*-run-info
func (backend ES2Backend) getAssets(ctx context.Context, boolQuery *elastic.BoolQuery) (int32, error) {
	myIndex := mappings.ComplianceRunInfo.Index

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %v", err)
		return 0, err
	}

	countService := elastic.NewCountService(client)

	count, err := countService.Query(boolQuery).Index(myIndex).Do(ctx)
	if err != nil {
		logrus.Errorf("Cannot create client for count assets with error %v", err)
		return 0, err
	}

	return int32(count), nil
}

//getFiltersQueryForAssetFilters Get Filteres query for the assets
func (backend ES2Backend) getFiltersQueryForAssetFilters(filters map[string][]string) *elastic.BoolQuery {
	utils.DeDupFilters(filters)
	boolQuery := elastic.NewBoolQuery()

	filterTypes := reporting.FilterType
	filterTypes = append(filterTypes, []string{"control", "profile_id", "node_id"}...)
	for _, filterType := range filterTypes {
		if len(filters[filterType]) > 0 {
			ESFieldName := getESFieldNameAsset(filterType)
			termQuery := newTermQueryFromFilter(ESFieldName, filters[filterType])
			boolQuery = boolQuery.Must(termQuery)
		}
	}

	// Going through all filters to find the ones prefixed with 'control_tag', e.g. 'control_tag:nist'
	for filterType := range filters {
		if strings.HasPrefix(filterType, "control_tag:") {
			_, tagKey := leftSplit(filterType, ":")
			termQuery := newNestedQueryForControlStringTagAssets(tagKey, filters[filterType])
			boolQuery = boolQuery.Must(termQuery)
		}
	}

	return boolQuery
}

// Returns an ElasticSearch nested query to filter reports by control tags for assets
func newNestedQueryForControlStringTagAssets(tagKey string, tagValues []string) *elastic.NestedQuery {
	refinedValues := make([]string, 0, 0)
	ESFieldPath := "control_tag"
	ESFieldTagKey := "control_tag.key.lower"
	ESFieldTagValues := "control_tag.values.lower"

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

//getESFieldName get the getESFieldNameAsset for assets
func getESFieldNameAsset(filterType string) string {
	ESFieldName := filterType
	switch filterType {
	case "chef_server":
		ESFieldName = "chef_server"
	case "inspec_version":
		ESFieldName = "inspec_version"
	case "organization":
		ESFieldName = "organization"
	case "platform":
		ESFieldName = "platform"
	case "platform_with_version":
		ESFieldName = "platform_with_version"
	case "recipe":
		ESFieldName = "recipe"
	case "role":
		ESFieldName = "role"
	case "environment":
		ESFieldName = "environment"
	case "chef_tags":
		ESFieldName = "chef_tags"
	case "node_id":
		ESFieldName = "node_id"
	case "policy_group":
		ESFieldName = "policy_group"
	case "policy_name":
		ESFieldName = "policy_name"
	case "control":
		ESFieldName = "control_id"
	case "profile_id":
		ESFieldName = "profile_id"
	}

	return ESFieldName
}

//getStartTimeAndEndTimeRangeForAsset gets the range query for date range and config as well
func getStartTimeAndEndTimeRangeForAsset(filters map[string][]string) *elastic.RangeQuery {

	endTime := firstOrEmpty(filters["end_time"])
	startTime := firstOrEmpty(filters["start_time"])

	timeRangeQuery := elastic.NewRangeQuery("last_run")
	if len(startTime) > 0 {
		timeRangeQuery.Gte(startTime)
	}
	if len(endTime) > 0 {
		timeRangeQuery.Lte(endTime)
	}

	return timeRangeQuery

}

//getReachableAssetTimeRangeQuery gets the range query for reachable assets
func getReachableAssetTimeRangeQuery(unreachableConfig int) *elastic.RangeQuery {
	if unreachableConfig > 0 {
		timeRangeQuery := elastic.NewRangeQuery("last_run")
		timeRangeQuery.Gt(time.Now().Add(-24 * time.Hour * time.Duration(unreachableConfig)).UTC().Format(time.RFC3339))
		return timeRangeQuery
	}
	return nil
}

func (backend ES2Backend) getCollectedAssetsCount(ctx context.Context, filtQuery *elastic.BoolQuery) (*AssetSummary, error) {

	name := "AssetCollectedCount"
	myIndex := mappings.ComplianceRunInfo.Index

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Size(0)

	for aggName, agg := range getSummaryAssetAggregation() {
		searchSource.Aggregation(aggName, agg)
	}

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %v", err)
		return nil, err
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", name))
	}

	LogQueryPartMin(myIndex, source, fmt.Sprintf("%s query", name))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(myIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		if searchResult != nil {
			logrus.Error(searchResult.Error)
		}
		logrus.Info(err)
		return nil, err
	}
	logrus.Info(searchResult)

	LogQueryPartMin(myIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", name))

	return getSummaryAssetAggResult(searchResult), nil
}

func (backend ES2Backend) getCollectedAssets(ctx context.Context, filtQuery *elastic.BoolQuery, index string, client *elastic.Client) ([]*reportingapi.Assets, error) {
	name := "AssetCollected"

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"status",
		"first_run",
		"last_run")

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(filtQuery).
		Size(100)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", name))
	}

	LogQueryPartMin(index, source, fmt.Sprintf("%s query", name))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(index).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(ctx)

	return getCollectedAssetsSearchResult(searchResult)
}

func getCollectedAssetsSearchResult(searchResult *elastic.SearchResult) ([]*reportingapi.Assets, error) {

	assets := make([]*reportingapi.Assets, 0)
	if searchResult.TotalHits() > 0 {
		for _, hit := range searchResult.Hits.Hits {
			var asset *reportingapi.Assets
			if hit.Source != nil {
				err := json.Unmarshal(hit.Source, &asset)
				if err != nil {
					return nil, errors.Wrap(err, "Unable to collect assets from database")
				}
				assets = append(assets, asset)
			}

		}
	}

	return assets, nil

}

func getSummaryAssetAggregation() map[string]elastic.Aggregation {

	status := "status"

	passed := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery(status, "passed"))

	skipped := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery(status, "skipped"))

	failed := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery(status, "failed"))

	waived := elastic.NewFilterAggregation().
		Filter(elastic.NewTermQuery(status, "waived"))
	aggs := make(map[string]elastic.Aggregation)
	aggs["failed"] = failed
	aggs["skipped"] = skipped
	aggs["passed"] = passed
	aggs["waived"] = waived

	return aggs
}

func getSummaryAssetAggResult(aggRoot *elastic.SearchResult) *AssetSummary {

	summary := &AssetSummary{}

	singleBucket, found := aggRoot.Aggregations.Filter("failed")
	if found {
		summary.Failed = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("passed")
	if found {
		summary.Passed = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("skipped")
	if found {
		summary.Skipped = int32(singleBucket.DocCount)
	}

	singleBucket, found = aggRoot.Aggregations.Filter("waived")
	if found {
		summary.Waived = int32(singleBucket.DocCount)
	}

	return summary
}
