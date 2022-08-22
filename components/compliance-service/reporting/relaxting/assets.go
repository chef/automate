package relaxting

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting"
	constant "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

//getAssets Get Total Number of documents from the comp-*-run-info
func (backend ES2Backend) getAssets(ctx context.Context, boolQuery *elastic.BoolQuery, msg string) (int32, error) {
	myIndex := mappings.ComplianceRunInfo.Index

	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %v", err)
		return 0, err
	}

	countService := elastic.NewCountService(client)

	scr, err := boolQuery.Source()
	LogQueryPartMin(myIndex, scr, msg)
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
	for _, filterType := range filterTypes {
		if len(filters[filterType]) > 0 {
			ESFieldName := getESFieldNameAsset(filterType)
			termQuery := newTermQueryFromFilter(ESFieldName, filters[filterType])
			boolQuery = boolQuery.Must(termQuery)
		}
	}

	if len(filters["node_id"]) > 0 {
		termQuery := elastic.NewTermsQuery("node_uuid", stringArrayToInterfaceArray(filters["node_id"])...)
		boolQuery = boolQuery.Must(termQuery)
	}

	numberOfProfiles := len(filters["profile_id"])
	numberOfControls := len(filters["control"])
	if numberOfProfiles > 0 || numberOfControls > 0 {
		profileBaseFscIncludes := []string{"profiles.name", "profiles.sha256"}
		controlLevelFscIncludes := []string{"profiles.controls.id"}

		profileAndControlQuery := getProfileAndControlQuery(filters, profileBaseFscIncludes, nil, controlLevelFscIncludes)
		boolQuery = boolQuery.Must(profileAndControlQuery)
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
		ESFieldName = "chef_server.lower"
	case "inspec_version":
		ESFieldName = "inspec_version.lower"
	case "organization":
		ESFieldName = "organization.lower"
	case "platform":
		ESFieldName = "platform.name.lower"
	case "platform_with_version":
		ESFieldName = "platform.full.lower"
	case "recipe":
		ESFieldName = "recipe.lower"
	case "role":
		ESFieldName = "role.lower"
	case "environment":
		ESFieldName = "environment.lower"
	case "chef_tags":
		ESFieldName = "chef_tags.lower"
	case "policy_group":
		ESFieldName = "policy_group.lower"
	case "policy_name":
		ESFieldName = "policy_name.lower"
	}

	return ESFieldName
}

//getStartTimeAndEndTimeRangeForAsset gets the range query for date range and config as well
func getStartTimeAndEndTimeRangeForAsset(filters map[string][]string) *elastic.RangeQuery {

	endTime := firstOrEmpty(filters[reporting.EndTime])
	startTime := firstOrEmpty(filters[reporting.StartTime])

	timeRangeQuery := elastic.NewRangeQuery("last_run")

	// For UnCollectedAssets we need all the records before the range start time
	if len(endTime) == 0 && len(startTime) > 0 {
		timeRangeQuery.Lte(startTime)
		timeRangeQuery.IncludeLower(false)
		return timeRangeQuery
	}

	if len(startTime) > 0 {
		timeRangeQuery.Gte(startTime)
	}
	if len(endTime) > 0 {
		timeRangeQuery.Lte(endTime)
	}

	return timeRangeQuery

}

//getUnReachableAssetTimeRangeQuery gets the range query for reachable assets
func getUnReachableAssetTimeRangeQuery(unreachableConfig int) *elastic.RangeQuery {
	if unreachableConfig > 0 {
		timeRangeQuery := elastic.NewRangeQuery("last_run")
		timeRangeQuery.Lte(time.Now().Add(-24 * time.Hour * time.Duration(unreachableConfig)).UTC().Format(time.RFC3339))
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
		logrus.Errorf("Cannot connect to ElasticSearch for assets count: %v", err)
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

func (backend ES2Backend) getAssetsList(ctx context.Context,
	from int32, size int32,
	filtQuery *elastic.BoolQuery) ([]*reportingapi.Assets, error) {
	index := getRunInfoIndex()
	name := "AssetCollected"
	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch for assests list: %v", err)
		return nil, err
	}

	fsc := elastic.NewFetchSourceContext(true).Include(
		"node_uuid",
		"status",
		"first_run",
		"last_run")

	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(filtQuery).
		Size(int(size)).
		From(int(from)).
		Sort("last_run", false)

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
func (backend ES2Backend) GetAssetSummary(ctx context.Context, filters map[string][]string) (*reportingapi.AssetSummary, error) {

	// get the total number of assets without any date range filters i.e all the assets present
	boolquery := backend.getFiltersQueryForAssetFilters(filters)
	repQuery := boolquery

	totalAssets, err := backend.getAssets(ctx, boolquery, "total_assets")
	if err != nil {
		logrus.Errorf("The error while getting the total assets %v", err)
		return nil, err
	}

	// getting the un-reachable assets as per the unreachable config
	// Todo hardcoding the value for Reachable assets
	unreachableAsset, err := backend.GetUnreachable(ctx, filters)
	if err != nil {
		logrus.Errorf("The error while getting unreachable assets: %v", err)
		return nil, err
	}

	// get the un-reported assets as per the start time and end time present in filters
	reportedQuery := repQuery.Must(getStartTimeAndEndTimeRangeForAsset(filters))
	reported, err := backend.getAssets(ctx, reportedQuery, "reported_assets")
	if err != nil {
		logrus.Errorf("The error while getting the unreported assests %v", err)
		return nil, err
	}
	unreportedAsset := totalAssets - (unreachableAsset + reported)

	// getting the collected assets as per the filters present
	collectedAsset, err := backend.getCollectedAssetsCount(ctx, reportedQuery)
	if err != nil {
		logrus.Errorf("The error while getting the collected assests %v", err)
		return nil, err
	}
	// uncollected := totalAssets - (collectedAsset.Passed + collectedAsset.Failed + collectedAsset.Skipped + collectedAsset.Waived)

	collected := &reportingapi.Collected{
		Passed:  collectedAsset.Passed,
		Failed:  collectedAsset.Failed,
		Skipped: collectedAsset.Skipped,
		Weived:  collectedAsset.Waived,
	}
	uncollected := &reportingapi.Uncollected{
		Unreachable: unreachableAsset,
		Unreported:  unreportedAsset,
	}

	summary := &reportingapi.AssetSummary{
		TotalAssets: totalAssets,
		Collected:   collected,
		Uncollected: uncollected,
	}

	return summary, nil
}

func (backend ES2Backend) getCollectedAssets(ctx context.Context, from int32, size int32, filters map[string][]string, filtQuery *elastic.BoolQuery) ([]*reportingapi.Assets, error) {
	//adding range query for date range
	filtQuery.Must(getStartTimeAndEndTimeRangeForAsset(filters))
	return backend.getAssetsList(ctx, from, size, filtQuery)
}

func (backend ES2Backend) getUnReachableAssets(ctx context.Context, from int32, size int32, filtQuery *elastic.BoolQuery, unReachableConfig int) ([]*reportingapi.Assets, error) {
	//adding range query as per unreachable config
	filtQuery.Must(getUnReachableAssetTimeRangeQuery(unReachableConfig))
	return backend.getAssetsList(ctx, from, size, filtQuery)

}

func (backend ES2Backend) getUnReportedAssets(ctx context.Context, from int32, size int32, filters map[string][]string, filtQuery *elastic.BoolQuery, unreachableConfig int) ([]*reportingapi.Assets, error) {
	//unreported assets are records between the un-reachable config date till range start time
	unReportedAssetsStartDate := time.Now().Add(-24 * time.Hour * time.Duration(unreachableConfig)).UTC().Format(time.RFC3339)
	unReportedAssetsEndDate := firstOrEmpty(filters["start_time"])

	filters[reporting.StartTime] = []string{unReportedAssetsStartDate}
	filters[reporting.EndTime] = []string{unReportedAssetsEndDate}

	filtQuery.Must(getStartTimeAndEndTimeRangeForAsset(filters))

	return backend.getAssetsList(ctx, from, size, filtQuery)
}

func (backend ES2Backend) getUnCollectedAssets(ctx context.Context, from int32, size int32, filters map[string][]string, filtQuery *elastic.BoolQuery) ([]*reportingapi.Assets, error) {
	//adding unCollectedAssetsTimeRangeQuery := as we need all the records before start time only
	filters[reporting.EndTime] = []string{}
	filtQuery.Must(getStartTimeAndEndTimeRangeForAsset(filters))
	return backend.getAssetsList(ctx, from, size, filtQuery)
}

func (backend ES2Backend) GetAsset(ctx context.Context, filters map[string][]string, size int32, from int32, assetsType string) ([]*reportingapi.Assets, error) {

	boolquery := backend.getFiltersQueryForAssetFilters(filters)
	assets := make([]*reportingapi.Assets, 0)
	if assetsType == constant.COLLECTED || len(assetsType) == 0 {
		return backend.getCollectedAssets(ctx, from, size, filters, boolquery)
	}
	if assetsType == constant.UNREACHABLE {
		return backend.getUnReachableAssets(ctx, from, size, boolquery, 10)
	}
	if assetsType == constant.UNCOLLECTED {
		return backend.getUnCollectedAssets(ctx, from, size, filters, boolquery)
	}
	if assetsType == constant.UNREPORTED {
		return backend.getUnReportedAssets(ctx, from, size, filters, boolquery, 10)
	}
	return assets, errors.New("Please provide the valid asset type")
}

func (backend ES2Backend) GetUnreachable(ctx context.Context, filters map[string][]string) (int32, error) {
	boolquery := backend.getFiltersQueryForAssetFilters(filters)
	unreachableQuery := boolquery
	unreachableQuery = unreachableQuery.Must(getUnReachableAssetTimeRangeQuery(10))
	unreachableAsset, err := backend.getAssets(ctx, unreachableQuery, "unreachable_assets")
	if err != nil {
		return 0, err
	}
	return unreachableAsset, nil
}
