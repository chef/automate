package elastic

import (
	"context"
	"time"

	elastic "gopkg.in/olivere/elastic.v6"
)

//GetUniqueNodesCount: Get the unique nodes count based on the lastTelemetryReportedAt
func (es Backend) GetUniqueNodesCount(daysSinceLastPost int64, lastTelemetryReportedAt time.Time) (int64, error) {

	var rangeQueryThreshold *elastic.RangeQuery
	t := time.Now().AddDate(0, 0, -1)
	yesterdayEODTimeStamp := time.Date(t.Year(), t.Month(), t.Day(), 23, 59, 59, t.Nanosecond(), t.Location())
	lastTelemetryReportedDate := lastTelemetryReportedAt.Format("2006-01-02")

	if daysSinceLastPost > 15 {
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(lastTelemetryReportedDate).To(yesterdayEODTimeStamp)
	} else {
		startTimeStamp := yesterdayEODTimeStamp.AddDate(0, 0, -16)
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(startTimeStamp).To(yesterdayEODTimeStamp)
	}
	boolQuery := elastic.NewBoolQuery().
		Must(rangeQueryThreshold)

	count, err := es.client.Count("node-1-run-info").Query(boolQuery).Do(context.Background())
	if err != nil {
		return 0, err
	}
	return count, nil
}
