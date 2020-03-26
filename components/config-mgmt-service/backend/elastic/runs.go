package elastic

import (
	"context"
	"encoding/json"
	"fmt"
	"math"
	"strconv"
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

// GetDeletedCountsTimeSeries - creates a daily time series of the total amount of nodes that have
// been deleted within or before the time range. The time series is between the startTime and endTime provided.
// The startTime and endTime must atleast be 24 hours apart. When greater than 24 hours, it must be in
// multiples 24 hour blocks.
// A node is deleted if exists = false. The time the node was deleted is the "timestamp"
func (es Backend) GetDeletedCountsTimeSeries(
	startTime, endTime time.Time, filters map[string][]string) ([]backend.CountPeroid, error) {
	var (
		aggTag = "date_range"
	)
	copiedFilters := map[string][]string{}
	for index, element := range filters {
		copiedFilters[index] = element
	}
	copiedFilters["exists"] = []string{"false"}
	mainQuery := newBoolQueryFromFilters(copiedFilters)

	deletedNodePeroids := make([]backend.CountPeroid, getNumberOf24hBetween(startTime, endTime))

	dateRangeAgg := elastic.NewDateRangeAggregation().Field(backend.Timestamp).
		Format("yyyy-MM-dd'T'HH:mm:ssZ")

	for index := 0; index < len(deletedNodePeroids); index++ {
		end := startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)

		dateRangeAgg.AddUnboundedFromWithKey(strconv.Itoa(index), end.Format(time.RFC3339))
	}

	searchResult, err := es.client.Search().
		Index(IndexNodeState).
		Query(mainQuery).
		Aggregation(aggTag, dateRangeAgg).
		Do(context.Background())
	if err != nil {
		return []backend.CountPeroid{}, err
	}

	rangeAggRes, found := searchResult.Aggregations.Range(aggTag)
	if !found {
		// This case is if there are no runs for the entire range of the time series
		// We are creating the buckets manually with zero check-ins
		for index := 0; index < len(deletedNodePeroids); index++ {
			deletedNodePeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			deletedNodePeroids[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			deletedNodePeroids[index].Count = 0
		}
		return deletedNodePeroids, nil
	}

	if len(rangeAggRes.Buckets) != len(deletedNodePeroids) {
		return []backend.CountPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(deletedNodePeroids), len(rangeAggRes.Buckets))
	}

	for index, bucket := range rangeAggRes.Buckets {
		deletedNodePeroids[index].Count = int(bucket.DocCount)
		deletedNodePeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		deletedNodePeroids[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return deletedNodePeroids, nil
}

// GetCreateCountsTimeSeries - creates a daily time series of the total amount of nodes that have
// been created within or before the time range. The time series is between the startTime and endTime provided.
// The startTime and endTime must atleast be 24 hours apart. When greater than 24 hours, it must be in
// multiples 24 hour blocks.
func (es Backend) GetCreateCountsTimeSeries(startTime, endTime time.Time,
	filters map[string][]string) ([]backend.CountPeroid, error) {
	var (
		aggTag    = "date_range"
		mainQuery = newBoolQueryFromFilters(filters)
	)

	createNodePeroids := make([]backend.CountPeroid, getNumberOf24hBetween(startTime, endTime))

	// This is using the custom_search_aggs_bucket_date_range because it need to set the missing option
	dateRangeAgg := NewDateRangeAggregation().Field(backend.Created).
		Format("yyyy-MM-dd'T'HH:mm:ssZ").
		Missing(time.Time{}.Format(time.RFC3339)) // count all nodes that do not have a create date

	for index := 0; index < len(createNodePeroids); index++ {
		to := startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)

		dateRangeAgg.AddUnboundedFromWithKey(strconv.Itoa(index), to.Format(time.RFC3339))
	}

	searchResult, err := es.client.Search().
		Index(IndexNodeState).
		Query(mainQuery).
		Aggregation(aggTag, dateRangeAgg).
		Do(context.Background())
	if err != nil {
		return []backend.CountPeroid{}, err
	}

	rangeAggRes, found := searchResult.Aggregations.Range(aggTag)
	if !found {
		// This case is if there are no runs for the entire range of the time series
		// We are creating the buckets manually with zero check-ins
		for index := 0; index < len(createNodePeroids); index++ {
			createNodePeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			createNodePeroids[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			createNodePeroids[index].Count = 0
		}
		return createNodePeroids, nil
	}

	if len(rangeAggRes.Buckets) != len(createNodePeroids) {
		return []backend.CountPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(createNodePeroids), len(rangeAggRes.Buckets))
	}

	for index, bucket := range rangeAggRes.Buckets {
		createNodePeroids[index].Count = int(bucket.DocCount)
		createNodePeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		createNodePeroids[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return createNodePeroids, nil
}

// GetCheckinCountsTimeSeries create a daily time series of unique nodes that have reported a runs
// between the startTime and endTime provided.
// The startTime and endTime must atleast be 24 hours apart. When greater than 24 hours, it must be in
// multiples 24 hour blocks.
func (es Backend) GetCheckinCountsTimeSeries(startTime, endTime time.Time,
	filters map[string][]string) ([]backend.CountPeroid, error) {
	var (
		dateHistoTag = "dateHisto"
		mainQuery    = newBoolQueryFromFilters(filters)
		nodeID       = "node_id"
	)

	// Filters the runs down to the needed time range
	rangeQuery, _ := newRangeQuery(startTime.Format(time.RFC3339),
		endTime.Format(time.RFC3339), backend.RunEndTime)

	mainQuery = mainQuery.Must(rangeQuery)

	bucketHist := elastic.NewDateHistogramAggregation().Field(backend.RunEndTime).
		Interval("24h"). // do not use "1d" because daylight savings time could have 23 or 25 hours
		MinDocCount(0).  // needed to return empty buckets
		ExtendedBounds(
			startTime.Format(time.RFC3339), endTime.Format(time.RFC3339)). // needed to return empty buckets
		Format("yyyy-MM-dd'T'HH:mm:ssZ").
		TimeZone(getTimezoneWithStartOfDayAtUtcHour(startTime)). // needed start the buckets at the beginning of the current hour.
		SubAggregation(nodeID,
			elastic.NewCardinalityAggregation().Field(backend.Id)) // count how many unique nodes are in this bucket

	searchResult, err := es.client.Search().
		Index(IndexConvergeHistory).
		Query(mainQuery).
		Aggregation(dateHistoTag, bucketHist).
		Do(context.Background())
	if err != nil {
		return []backend.CountPeroid{}, err
	}

	checkInPeroids := make([]backend.CountPeroid, getNumberOf24hBetween(startTime, endTime))
	dateHistoRes, found := searchResult.Aggregations.DateHistogram(dateHistoTag)
	if !found {
		// This case is if there are no runs for the entire range of the time series
		// We are creating the buckets manually with zero check-ins
		for index := 0; index < len(checkInPeroids); index++ {
			checkInPeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			checkInPeroids[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			checkInPeroids[index].Count = 0
		}
		return checkInPeroids, nil
	}

	if len(dateHistoRes.Buckets) != len(checkInPeroids) {
		return []backend.CountPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(checkInPeroids), len(dateHistoRes.Buckets))
	}

	for index, bucket := range dateHistoRes.Buckets {
		item, found := bucket.Aggregations.Cardinality(nodeID)
		if found {
			checkInPeroids[index].Count = int(*item.Value)
		} else {
			checkInPeroids[index].Count = 0
		}

		checkInPeroids[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		checkInPeroids[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return checkInPeroids, nil
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

// The number of 24 hour blocks between the start and end times.
func getNumberOf24hBetween(start, end time.Time) int {
	numberOfHours := int(math.Ceil(end.Sub(start).Hours()))
	return int(math.Ceil(float64(numberOfHours) / 24.0))
}

// Get the timezone that has the start of the day (midnight) for the UTC time provided
// Elasticsearch starts each bucket at the beginning of the day for the time zone used.
// Using this time zone allows us to create elasticsearch buckets 24 hours from the time provided.
func getTimezoneWithStartOfDayAtUtcHour(dateTime time.Time) string {
	if dateTime.Hour() < 12 {
		return fmt.Sprintf("-%02d:00", dateTime.Hour())
	}

	return fmt.Sprintf("+%02d:00", (24 - dateTime.Hour()))
}
