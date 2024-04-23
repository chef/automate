package elastic

import (
	"context"
	"time"

	elastic "github.com/olivere/elastic/v7"
)

//GetUniqueNodesCount: Get the unique nodes count based on the lastTelemetryReportedAt
func (es Backend) GetUniqueNodesCount(daysSinceLastPost int64, lastTelemetryReportedAt time.Time) (int64, error) {

	var rangeQueryThreshold *elastic.RangeQuery
	t := time.Now()
	startDayTimeStamp := time.Date(t.Year(), t.Month(), t.Day(), 0, 0, 0, t.Nanosecond(), t.Location())
	lastTelemetryReportedDate := lastTelemetryReportedAt.Format("2006-01-02")

	// if daysSinceLastPost >= three months then take the unique nodes count from last three months
	// and if daysSinceLastPost > 30 and < three months then take unique nodes count from lastTelemetryReportedDate to yesterday EOD
	// else take the unique nodes count from last 30 days
	if daysSinceLastPost >= 90 {
		startTimeStamp := startDayTimeStamp.AddDate(0, 0, -91)
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(startTimeStamp).To(time.Now())
	} else if daysSinceLastPost > 30 {
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(lastTelemetryReportedDate).To(time.Now())
	} else {
		startTimeStamp := startDayTimeStamp.AddDate(0, 0, -31)
		rangeQueryThreshold = elastic.NewRangeQuery("last_run").From(startTimeStamp).To(time.Now())
	}
	boolQuery := elastic.NewBoolQuery().
		Must(rangeQueryThreshold)

	count, err := es.client.Count("node-1-run-info").Query(boolQuery).Do(context.Background())
	if err != nil {
		return 0, err
	}
	return count, nil
}
