package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"
)

func (depth *ControlDepth) getTrendAggs(trendType string, filters map[string][]string) map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)

	waivedQuery := elastic.NewTermsQuery("profiles.controls.waived_str", "yes", "yes_run")
	aggs["passed"] = elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermsQuery("profiles.controls.status", "passed")).
			MustNot(waivedQuery))
	aggs["failed"] = elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermsQuery("profiles.controls.status", "failed")).
			MustNot(waivedQuery))
	aggs["skipped"] = elastic.NewFilterAggregation().
		Filter(elastic.NewBoolQuery().
			Must(elastic.NewTermsQuery("profiles.controls.status", "skipped")).
			MustNot(waivedQuery))
	aggs["waived"] = elastic.NewFilterAggregation().
		Filter(waivedQuery)

	return depth.wrap(aggs)
}

func (depth *ControlDepth) getTrendResults(trendType string,
	searchResult *elastic.SearchResult) (map[string]*stats.Trend, error) {
	myName := "ControlDepth::getTrendResults"

	var mapOfTrends map[string]*stats.Trend
	trendBucketsAggResult, _ := searchResult.Aggregations.Terms("trend_buckets")
	if trendBucketsAggResult != nil {
		mapOfTrends = make(map[string]*stats.Trend)
		for _, wrappedTrendBucket := range trendBucketsAggResult.Buckets {
			endTimeAsTime, err := time.Parse(time.RFC3339, *wrappedTrendBucket.KeyAsString)
			if err != nil {
				logrus.Errorf(fmt.Sprintf("%s - could not parse time", myName))
				return mapOfTrends, err
			}

			//force it to end of day time (UI needs this so that it may render correctly
			trendIndexDate := time.Date(endTimeAsTime.Year(), endTimeAsTime.Month(), endTimeAsTime.Day(),
				23, 59, 59, 0, time.UTC)
			trendIndexDateAsString := trendIndexDate.Format(time.RFC3339)

			zaStatsBucket := &stats.Trend{
				ReportTime: trendIndexDateAsString,
				Passed:     0,
				Failed:     0,
				Skipped:    0,
				Waived:     0,
			}

			if trendBucket, found := depth.unwrap(&wrappedTrendBucket.Aggregations); found {
				if passedResult, found := trendBucket.Aggregations.Filter("passed"); found {
					zaStatsBucket.Passed = int32(passedResult.DocCount)
				}
				if failedResult, found := trendBucket.Aggregations.Filter("failed"); found {
					zaStatsBucket.Failed = int32(failedResult.DocCount)
				}
				if skippedResult, found := trendBucket.Aggregations.Filter("skipped"); found {
					zaStatsBucket.Skipped = int32(skippedResult.DocCount)
				}
				if waivedResult, found := trendBucket.Aggregations.Filter("waived"); found {
					zaStatsBucket.Waived = int32(waivedResult.DocCount)
				}
				mapOfTrends[trendIndexDateAsString] = zaStatsBucket
			}
		}
	}
	return mapOfTrends, nil
}
