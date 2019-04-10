package relaxting

import (
	"fmt"
	"time"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/olivere/elastic"
	"github.com/sirupsen/logrus"
)

func (depth *ProfileDepth) getTrendAggs(trendType string,
	filters map[string][]string) map[string]elastic.Aggregation {
	aggs := make(map[string]elastic.Aggregation)

	if trendType == "nodes" {
		aggs["passed"] = elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.status", "passed"))
		aggs["failed"] = elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.status", "failed"))
		aggs["skipped"] = elastic.NewFilterAggregation().Filter(elastic.NewTermQuery("profiles.status", "skipped"))
	} else if trendType == "controls" {
		aggs["passed"] = elastic.NewSumAggregation().Field("profiles.controls_sums.passed.total")
		aggs["failed"] = elastic.NewSumAggregation().Field("profiles.controls_sums.failed.total")
		aggs["skipped"] = elastic.NewSumAggregation().Field("profiles.controls_sums.skipped.total")
	}
	return depth.wrap(aggs)
}

func (depth *ProfileDepth) getTrendResults(trendType string,
	searchResult *elastic.SearchResult) (map[string]*stats.Trend, error) {
	myName := "ProfileDepth::getTrendResults"

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
			}

			if trendBucket, found := depth.unwrap(&wrappedTrendBucket.Aggregations); found {
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
	}
	return mapOfTrends, nil
}
