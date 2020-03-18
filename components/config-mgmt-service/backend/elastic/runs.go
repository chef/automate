package elastic

import (
	"context"
	"encoding/json"
	"fmt"
	"math"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

// GetRun - Get a node's last run
//
// @param [run_id]   The id of the node's last run
// @param [last_ccr] The time of the node's last CCR, used for index determination
// @return           The run object
//
// The ES query we use is:
// {
//   "query":{
//     "bool":{
//       "must": {
//         "term":{
//           "run_id":"5ad11e7e-c185-4b80-8a16-167e257b30d1"
//         }
//       }
//     }
//   }
// }
func (es Backend) GetRun(runID string, endTime time.Time) (backend.Run, error) {
	var run backend.Run
	boolQuery := elastic.NewBoolQuery().Must(elastic.NewTermQuery("run_id", runID))
	var index string
	// If time is default time (was not passed in) use wildcard query
	if endTime.IsZero() || endTime.Equal(time.Unix(0, 0)) {
		index = IndexConvergeHistory
	} else { // otherwise use last ccr time to determine index
		index = IndexConvergeHistoryBase + endTime.Format("2006.01.02")
	}

	searchResult, err := es.client.Search().
		Query(boolQuery).
		Index(index).
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return run, err
	}

	if searchResult.Hits.TotalHits == 0 {
		return run, errors.New(errors.RunNotFound, "Invalid ID")
	}

	source := searchResult.Hits.Hits[0].Source
	err = json.Unmarshal(*source, &run)
	if err != nil {
		log.WithFields(log.Fields{
			"object": source,
		}).WithError(err).Debug("Unable to unmarshal the run object")
		return run, err
	}

	return run, nil
}

func (es Backend) GetTimeseriCheckinCounts(startTime, endTime time.Time) ([]backend.CheckInPeroid, error) {
	var (
		dateHistoTag   = "dateHisto"
		mainQuery      = elastic.NewBoolQuery()
		eventTypeItems = "items"
	)

	rangeQuery, _ := newRangeQuery(startTime.Format(time.RFC3339),
		endTime.Format(time.RFC3339), "end_time")

	mainQuery = mainQuery.Must(rangeQuery)

	bucketHist := elastic.NewDateHistogramAggregation().Field("end_time").
		Interval("1d").
		MinDocCount(0). // needed to return empty buckets
		ExtendedBounds(
			startTime.Format(time.RFC3339), endTime.Format(time.RFC3339)). // needed to return empty buckets
		Format("yyyy-MM-dd'T'HH:mm:ssZ").
		TimeZone(getTimezoneSoStartAtMidnight(startTime)). // needed start the buckets at the beginning of the day.
		SubAggregation(eventTypeItems,
			elastic.NewTermsAggregation().Field("entity_uuid"))

	searchResult, err := es.client.Search().
		Index("converge-history-*").
		Query(mainQuery).
		Aggregation(dateHistoTag, bucketHist).
		Do(context.Background())
	if err != nil {
		return []backend.CheckInPeroid{}, err
	}

	numberOfHours := getNumberOfHoursBetween(startTime, endTime)
	numberOfDays := int(math.Ceil(float64(numberOfHours) / 24.0))
	checkInPeroids := make([]backend.CheckInPeroid, numberOfDays)
	dateHistoRes, found := searchResult.Aggregations.DateHistogram(dateHistoTag)
	if !found {
		for index := 0; index < numberOfDays; index++ {
			checkInPeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			checkInPeroids[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
		}
		return checkInPeroids, nil
	}

	if len(dateHistoRes.Buckets) != numberOfDays {
		return []backend.CheckInPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			numberOfDays, len(dateHistoRes.Buckets))
	}

	for index, bucket := range dateHistoRes.Buckets {
		item, found := bucket.Aggregations.Terms(eventTypeItems)
		if found {
			checkInPeroids[index].CheckInCount = len(item.Buckets)
		}

		checkInPeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		checkInPeroids[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return checkInPeroids, nil
}

func getNumberOfHoursBetween(start, end time.Time) int {

	return int(math.Ceil(end.Sub(start).Hours()))
}

func getTimezoneSoStartAtMidnight(start time.Time) string {
	if start.Hour() < 12 {
		return fmt.Sprintf("-%02d:00", start.Hour())
	}

	return fmt.Sprintf("+%02d:00", (24 - start.Hour()))
}

// GetRunsCounts returns a RunsCounts object that contains the number of success, failure, total runs for a node
func (es Backend) GetRunsCounts(filters map[string][]string, nodeID string, start string,
	end string) (backend.RunsCounts, error) {
	var ns = *new(backend.RunsCounts)

	mainQuery := newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQuery(start, end, RunFieldTimestamp)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	nodeIDQuery := elastic.NewBoolQuery()
	nodeIDQuery = nodeIDQuery.Must(elastic.NewTermsQuery(backend.Id, nodeID))
	mainQuery = mainQuery.Must(nodeIDQuery)

	var searchTerm = "status"

	statusRunsBuckets, err := es.getAggregationBucket(mainQuery, IndexConvergeHistory, searchTerm)

	// Return an error request to elastic search failed
	if err != nil {
		return ns, err
	}

	var totalRuns int64
	for _, bucket := range statusRunsBuckets {
		switch bucket.Key {
		case "success":
			ns.Success = bucket.DocCount
		case "failure":
			ns.Failure = bucket.DocCount
		}

		totalRuns += bucket.DocCount
	}

	ns.Total = totalRuns

	return ns, nil
}

// GetRuns - get a collection of abridged runs
func (es Backend) GetRuns(nodeID string, page int, perPage int, filters map[string][]string, start string, end string) ([]backend.AbridgedConverge, error) {
	var runs []backend.AbridgedConverge
	var r backend.AbridgedConverge

	// Decrement one to the page since we must start from zero
	page = page - 1
	startPage := perPage * page

	filters["entity_uuid"] = []string{nodeID}
	mainQuery := newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQuery(start, end, RunFieldTimestamp)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	sortAscending := false
	searchResult, err := es.client.Search().
		Query(mainQuery).
		Index(IndexConvergeHistory). // search in indexes "converge-history-*"
		Sort("end_time", sortAscending).
		From(startPage).Size(perPage). // take documents from {start} to {perPage}
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			err := json.Unmarshal(*hit.Source, &r)
			if err != nil {
				log.WithError(err).Error("Error unmarshalling the node object")
			} else {
				runs = append(runs, r)
			}
		}
	}

	return runs, nil
}

func (es Backend) GetRunsPageByCursor(ctx context.Context, nodeID string, start time.Time,
	end time.Time, filters map[string][]string, cursorEndTime time.Time,
	cursorID string, pageSize int, ascending bool) ([]backend.Run, error) {

	filters["entity_uuid"] = []string{nodeID}
	mainQuery := newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQueryTime(start, end, RunFieldEndTimestamp)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	searchService := es.client.Search().
		Query(mainQuery).
		Index(IndexConvergeHistory).
		Size(pageSize).
		Sort(RunFieldEndTimestamp, ascending).
		Sort(RunFieldID, ascending)

	if !cursorEndTime.IsZero() && cursorID != "" {
		// the date has to be in milliseconds
		milliseconds := cursorEndTime.UnixNano() / int64(time.Millisecond)
		searchService = searchService.SearchAfter(milliseconds, cursorID)
	}

	searchResult, err := searchService.Do(ctx)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	var runs []backend.Run
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Run
		for _, hit := range searchResult.Hits.Hits {
			var run backend.Run
			err := json.Unmarshal(*hit.Source, &run)
			if err != nil {
				log.WithError(err).Error("Error unmarshalling the node object")
			} else {
				runs = append(runs, run)
			}
		}
	}

	return runs, nil
}

// GetDateOfOldestConvergeIndices - Find the date of the oldest converge history index.
// If there is no converge history indices returns false on the second return value.
func (es Backend) GetDateOfOldestConvergeIndices() (time.Time, bool, error) {
	indiceNames, err := es.getAllConvergeIndiceNames()
	if err != nil {
		return time.Time{}, true, err
	}

	indiceDates := []time.Time{}
	for _, indexName := range indiceNames {
		dateString := strings.Replace(indexName, mappings.ConvergeHistory.Index+"-", "", -1)
		date, err := time.Parse(mappings.TimeseriesDateFmt, dateString)
		if err != nil {
			return time.Time{}, true, err
		}

		indiceDates = append(indiceDates, date)
	}

	if len(indiceDates) == 0 {
		return time.Time{}, false, nil
	}

	oldestIndexDate := indiceDates[0]

	for _, indexDate := range indiceDates[1:] {
		if indexDate.Before(oldestIndexDate) {
			oldestIndexDate = indexDate
		}
	}

	return oldestIndexDate, true, nil
}

func (es Backend) getAllConvergeIndiceNames() ([]string, error) {
	res, err := es.client.IndexGetSettings().Index(IndexConvergeHistory).Do(context.Background())
	if err != nil {
		return []string{}, err
	}

	names := make([]string, 0, len(res))
	for name := range res {
		names = append(names, name)
	}

	return names, nil
}
