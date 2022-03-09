package relaxting

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/external/lib/errorutils"
	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
)

//GetStatsSummary - Report #16
func (backend ES2Backend) GetStatsSummary(filters map[string][]string) (*stats.ReportSummary, error) {
	myName := "GetStatsSummary"
	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	for aggName, agg := range depth.getStatsSummaryAggs() {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		return nil, err
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	return depth.getStatsSummaryResult(searchResult), nil
}

//GetStatsSummaryNodes - Gets summary stats, node centric, aggregate data for the given set of filters
func (backend ES2Backend) GetStatsSummaryNodes(filters map[string][]string) (*stats.NodeSummary, error) {
	myName := "GetStatsSummaryNodes"
	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	for aggName, agg := range depth.getStatsSummaryNodesAggs() {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		return nil, err
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	return depth.getStatsSummaryNodesResult(searchResult), nil
}

//GetStatsSummaryControls - Gets summary stats, control centric, aggregate data for the given set of filters
func (backend ES2Backend) GetStatsSummaryControls(filters map[string][]string) (*stats.ControlsSummary, error) {
	myName := "GetStatsSummaryControls"

	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	for aggName, agg := range depth.getStatsSummaryControlsAggs() {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		return nil, err
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	return depth.getStatsSummaryControlsResult(searchResult), nil
}

//GetStatsFailures - Gets top failures, aggregate data for the given set of filters
func (backend ES2Backend) GetStatsFailures(reportTypes []string, size int, filters map[string][]string) (*stats.Failures, error) {
	myName := "GetStatsFailures"
	var failures *stats.Failures

	for _, reportType := range reportTypes {
		switch reportType {
		case "profile", "control", "environment", "platform":
			break
		default:
			return failures, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid type '%s'", reportType)}
		}
	}

	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return failures, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()
	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	aggs, err := depth.getStatsTopFailuresAggs(size, reportTypes)
	if err != nil {
		return failures, errors.Wrap(err, fmt.Sprintf("%s unable to get aggs", myName))
	}

	for aggName, agg := range aggs {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return failures, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		return failures, err
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	return depth.getStatsTopFailuresResult(searchResult, reportTypes)
}

//GetProfileListWithAggregatedComplianceSummaries - Report #6
//todo - Where in A2 UI is this being called? It now works with deep filtering but not sure if we need it.
func (backend ES2Backend) GetProfileListWithAggregatedComplianceSummaries(
	filters map[string][]string, size int32) ([]*stats.ProfileList, error) {
	myName := "GetProfileListWithAggregatedComplianceSummaries"

	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	for aggName, agg := range depth.getProfileListWithAggregatedComplianceSummariesAggs(filters, size) {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		return nil, err
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	return depth.getProfileListWithAggregatedComplianceSummariesResults(searchResult, filters), nil
}

//GetControlListStatsByProfileID returns the control list for a given profile or profile and a child control
// the data retrieved from this appears at the bottom of the a2 page when you select a profile from list
func (backend ES2Backend) GetControlListStatsByProfileID(profileID string, from int, size int,
	filters map[string][]string, sortField string, sortAsc bool) ([]*stats.ControlStats, error) {
	myName := "GetControlListStatsByProfileID"

	//deep filtering uses the filter contents to determine which Depth will be retrieved from the factory
	// in the case of this func, we may not have the profileID in the filters. We will add it now if not,
	// which will allow the abstract factory to do the right thing.
	// There should only be one profileID in the filters and it must be identical to the profileID being passed in.
	filters["profile_id"] = []string{profileID}

	// Only end_time matters for this call
	filters["start_time"] = []string{}
	depth, err := backend.NewDepth(filters, true)
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Size(0)

	for aggName, agg := range depth.getControlListStatsByProfileIdAggs(size, sortField, sortAsc) {
		searchSource.Aggregation(aggName, agg)
	}

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, fmt.Sprintf("%s unable to get Source", myName))
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query", myName))

	searchResult, err := queryInfo.client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Size(0).
		Do(context.Background())
	if err != nil {
		return nil, err
	}

	LogQueryPartMin(queryInfo.esIndex, searchResult.Aggregations, fmt.Sprintf("%s searchResult aggs", myName))

	return depth.getControlListStatsByProfileIdResults(&backend, searchResult, profileID)
}

//GetUniqueNodesCount: Get the unique nodes count based on the lastTelemetryReportedAt
func (backend ES2Backend) GetUniqueNodesCount(daysSinceLastPost int64, lastTelemetryReportedAt time.Time) (int64, error) {
	client, err := backend.ES2Client()
	if err != nil {
		logrus.Errorf("Cannot connect to ElasticSearch: %s", err)
		return 0, err
	}

	var rangeQueryThreshold *elastic.RangeQuery
	t := time.Now().AddDate(0, 0, -1)
	yesterdayEODTimeStamp := time.Date(t.Year(), t.Month(), t.Day(), 23, 59, 59, t.Nanosecond(), t.Location())
	lastTelemetryReportedDate := lastTelemetryReportedAt.Format("2006-01-02")

	// if daysSinceLastPost >= three months then take the unique nodes count from last three months
	// and if daysSinceLastPost > 15 and < three months then take unique nodes count from lastTelemetryReportedDate to yesterday EOD
	// else take the unique nodes count from last 15 days
	if daysSinceLastPost >= 90 {
		startTimeStamp := yesterdayEODTimeStamp.AddDate(0, 0, -91)
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(startTimeStamp).To(yesterdayEODTimeStamp)
	} else if daysSinceLastPost > 15 {
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(lastTelemetryReportedDate).To(yesterdayEODTimeStamp)
	} else {
		startTimeStamp := yesterdayEODTimeStamp.AddDate(0, 0, -16)
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(startTimeStamp).To(yesterdayEODTimeStamp)
	}
	boolQuery := elastic.NewBoolQuery().
		Must(rangeQueryThreshold)

	count, err := client.Count(mappings.IndexNameComplianceRunInfo).Query(boolQuery).Do(context.Background())
	if err != nil {
		return 0, err
	}
	return count, nil
}
