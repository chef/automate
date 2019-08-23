package relaxting

//GetTrendCompliancyNodes - Report #11
import (
	"context"
	"fmt"
	"sort"
	"time"

	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting/util"
)

type TrendBucket struct {
	StartTime *time.Time
	EndTime   *time.Time
}

// getTrendBuckets provides an array of TrendBucket structs sorted by EndTime
// First bucket has a nil StartTime to indicate that is open
func (backend ES2Backend) getTrendBucketsRoundDown(filters map[string][]string, interval int) ([]TrendBucket, error) {
	interval = interval / 86400
	trendBuckets := make([]TrendBucket, 0)
	endTime := firstOrEmpty(filters["end_time"])
	startTime := firstOrEmpty(filters["start_time"])

	var startTimeAsTime, endTimeAsTime time.Time
	startTimeAsTime, err = time.Parse(time.RFC3339, startTime)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("GetTrendBuckets, cannot parse start_time: %s", err.Error()))
	}

	endTimeAsTime, err = time.Parse(time.RFC3339, endTime)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("GetTrendBuckets, cannot parse end_time: %s", err.Error()))
	}

	bucketEndTime := endTimeAsTime
	for bucketEndTime.After(startTimeAsTime) {
		bucketStartTime := bucketEndTime.AddDate(0, 0, -interval)
		bucketEndTimeCopy := bucketEndTime
		trendBuckets = append(trendBuckets, TrendBucket{StartTime: &bucketStartTime, EndTime: &bucketEndTimeCopy})
		bucketEndTime = bucketStartTime
	}

	// Sort buckets by end_time
	sort.Slice(trendBuckets, func(i, j int) bool {
		return trendBuckets[i].EndTime.Before(*trendBuckets[j].EndTime)
	})

	// Because first bucket to the left has no start time
	if len(trendBuckets) > 0 {
		trendBuckets[0].StartTime = nil
	}

	return trendBuckets, nil
}

//GetTrend get either a nodes or controls trend graph
func (backend ES2Backend) GetTrend(filters map[string][]string, interval int, trendType string) ([]*stats.Trend, error) {
	defer util.TimeTrack(time.Now(), "GetTrend")
	myName := "GetTrend"

	trendStatsBuckets := make([]*stats.Trend, 0) // stores the final array that will be returned to the user

	if trendType != "controls" && trendType != "nodes" {
		return trendStatsBuckets,
			errors.New(fmt.Sprintf("%s supports either trendType of 'controls' or 'nodes' but you passed in %s",
				myName, trendType))
	}

	depth, err := backend.NewDepth(filters, true, true)
	if err != nil {
		return trendStatsBuckets, errors.Wrap(err, fmt.Sprintf("%s unable to get depth level for report", myName))
	}

	queryInfo := depth.getQueryInfo()

	trendBuckets := elastic.NewDateHistogramAggregation().
		Interval("1d").
		Field("end_time")

	for aggName, agg := range depth.getTrendAggs(trendType, filters) {
		trendBuckets.SubAggregation(aggName, agg)
	}

	searchSource := elastic.NewSearchSource().
		Query(queryInfo.filtQuery).
		Aggregation("trend_buckets", trendBuckets).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return trendStatsBuckets, err
	}

	client, err := backend.ES2Client()
	if err != nil {
		return trendStatsBuckets, errors.Wrapf(err, "%s cannot connect to elasticsearch", myName)
	}

	LogQueryPartMin(queryInfo.esIndex, source, fmt.Sprintf("%s query searchSource", myName))

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(queryInfo.esIndex).
		Do(context.Background())

	if err != nil {
		return trendStatsBuckets, errors.Wrapf(err, "%s cannot get result from elasticsearch", myName)
	}

	mapOfTrends, err := depth.getTrendResults(trendType, searchResult)
	if err != nil {
		return trendStatsBuckets, errors.Wrapf(err, "%s cannot get trendResults", myName)
	}

	// only used to store the time intervals
	trendBucketsComputed, err := backend.getTrendBucketsRoundDown(filters, 86400)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("%s returned error: %s", myName, err.Error()))
	}

	if len(trendBucketsComputed) == 0 {
		return trendStatsBuckets, nil
	}

	for _, trendBucket := range trendBucketsComputed {
		indexDate := trendBucket.EndTime
		indexDateAsString := indexDate.Format(time.RFC3339)
		zaStatsBucket := mapOfTrends[indexDateAsString]
		if zaStatsBucket == nil {
			zaStatsBucket = &stats.Trend{
				ReportTime: indexDateAsString,
				Passed:     0,
				Failed:     0,
				Skipped:    0,
			}
		}
		trendStatsBuckets = append(trendStatsBuckets, zaStatsBucket)
	}
	LogQueryPartMin(queryInfo.esIndex, searchResult, fmt.Sprintf("%s query searchResult", myName))

	return trendStatsBuckets, nil
}

func stringArrayToInterfaceArray(stringArrayToConvert []string) []interface{} {
	interfaceArray := make([]interface{}, len(stringArrayToConvert))
	for i, v := range stringArrayToConvert {
		interfaceArray[i] = v
	}
	return interfaceArray
}
