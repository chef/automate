package relaxting

//GetTrendCompliancyNodes - Report #11
import (
	"fmt"
	"sort"
	"time"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting/util"
	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
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
	trendBuckets[0].StartTime = nil

	return trendBuckets, nil
}

//GetTrend get either a nodes or controls trend graph
func (backend ES2Backend) GetTrend(filters map[string][]string, interval int, trendType string) ([]*stats.Trend, error) {
	defer util.TimeTrack(time.Now(), "GetTrend")
	trendStatsBuckets := make([]*stats.Trend, 0) // stores the final array that will be returned to the user

	if trendType != "controls" && trendType != "nodes" {
		return trendStatsBuckets, errors.New(fmt.Sprintf("GetTrends supports either trendType of 'controls' or 'nodes' but you"+
			" passed in %s", trendType))
	}

	endTime := firstOrEmpty(filters["end_time"])
	startTime := firstOrEmpty(filters["start_time"])
	esIndex, err := IndexDates(CompDailySumIndexPrefix, startTime, endTime)
	if err != nil {
		return trendStatsBuckets, err
	}

	logrus.Debugf("esIndex %s", esIndex)
	filtQuery := backend.getFiltersQuery(filters, true, true)

	var passedFilter, failedFilter, skippedFilter elastic.Aggregation
	if trendType == "nodes" {
		passedFilter = elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "passed"))
		failedFilter = elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "failed"))
		skippedFilter = elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("status", "skipped"))
	} else if trendType == "controls" {
		passedFilter = elastic.NewSumAggregation().Field("controls_sums.passed.total")
		failedFilter = elastic.NewSumAggregation().Field("controls_sums.failed.total")
		skippedFilter = elastic.NewSumAggregation().Field("controls_sums.skipped.total")
	}
	trendBuckets := elastic.NewDateHistogramAggregation().
		SubAggregation("passed", passedFilter).
		SubAggregation("failed", failedFilter).
		SubAggregation("skipped", skippedFilter).
		Interval("1d").
		Field("end_time")

	searchSource := elastic.NewSearchSource().
		Query(filtQuery).
		Aggregation("trend_buckets", trendBuckets).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return trendStatsBuckets, err
	}

	client, err := backend.ES2Client()
	if err != nil {
		return trendStatsBuckets, errors.Wrap(err, "GetTrend cannot connect to elasticsearch")
	}

	LogQueryPartMin(esIndex, source, "GetTrend query searchSource")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		Do(context.Background())

	if err != nil {
		return trendStatsBuckets, errors.Wrap(err, "GetTrend cannot get result from elasticsearch")
	}

	trendBucketsAggResult, _ := searchResult.Aggregations.Terms("trend_buckets")
	var mapOfTrends map[string]*stats.Trend
	if trendBucketsAggResult != nil {
		mapOfTrends = make(map[string]*stats.Trend)
		for _, trendBucket := range trendBucketsAggResult.Buckets {

			endTimeAsTime, err := time.Parse(time.RFC3339, *trendBucket.KeyAsString)
			if err != nil {
				logrus.Error("could not parse time")
				return trendStatsBuckets, err
			}
			//endTimeAsString := endTimeAsTime.Format(time.RFC3339)

			//force it to end of day time (UI needs this so that it may render correctly
			trendIndexDate := time.Date(endTimeAsTime.Year(), endTimeAsTime.Month(), endTimeAsTime.Day(), 23, 59, 59, 0, time.UTC)
			trendIndexDateAsString := trendIndexDate.Format(time.RFC3339)

			zaStatsBucket := &stats.Trend{
				ReportTime: trendIndexDateAsString,
				Passed:     0,
				Failed:     0,
				Skipped:    0,
			}

			if trendType == "nodes" {
				if passedResult, found := trendBucket.Aggregations.Filter("passed"); found {
					zaStatsBucket.Passed = int32(passedResult.DocCount)
				}
				if failedResult, found := trendBucket.Aggregations.Filter("failed"); found {
					zaStatsBucket.Failed = int32(failedResult.DocCount)
				}
				if skippedResult, found := trendBucket.Aggregations.Filter("skipped"); found {
					zaStatsBucket.Skipped = int32(skippedResult.DocCount)
				}
			} else if trendType == "controls" {
				if passedResult, found := trendBucket.Aggregations.Sum("passed"); found {
					zaStatsBucket.Passed = int32(*passedResult.Value)
				}
				if failedResult, found := trendBucket.Aggregations.Sum("failed"); found {
					zaStatsBucket.Failed = int32(*failedResult.Value)
				}
				if skippedResult, found := trendBucket.Aggregations.Sum("skipped"); found {
					zaStatsBucket.Skipped = int32(*skippedResult.Value)
				}
			}
			mapOfTrends[trendIndexDateAsString] = zaStatsBucket
		}
	}

	// only used to store the time intervals
	trendBucketsComputed, err := backend.getTrendBucketsRoundDown(filters, 86400)
	if err != nil {
		return nil, errors.New(fmt.Sprintf("GetTrendBuckets returned error: %s", err.Error()))
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
	LogQueryPartMin(esIndex, searchResult, "GetTrend query searchResult")

	return trendStatsBuckets, nil
}

func stringArrayToInterfaceArray(stringArrayToConvert []string) []interface{} {
	interfaceArray := make([]interface{}, len(stringArrayToConvert))
	for i, v := range stringArrayToConvert {
		interfaceArray[i] = v
	}
	return interfaceArray
}
