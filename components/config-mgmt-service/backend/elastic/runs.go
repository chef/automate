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
// The startTime and endTime must at least be 24 hours apart. When greater than 24 hours, it must be in
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

	deletedNodePeriods := make([]backend.CountPeroid, getNumberOf24hBetween(startTime, endTime))

	dateRangeAgg := elastic.NewDateRangeAggregation().Field(backend.Timestamp).
		Format("yyyy-MM-dd'T'HH:mm:ssZ")

	for index := 0; index < len(deletedNodePeriods); index++ {
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
		for index := 0; index < len(deletedNodePeriods); index++ {
			deletedNodePeriods[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			deletedNodePeriods[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			deletedNodePeriods[index].Count = 0
		}
		return deletedNodePeriods, nil
	}

	if len(rangeAggRes.Buckets) != len(deletedNodePeriods) {
		return []backend.CountPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(deletedNodePeriods), len(rangeAggRes.Buckets))
	}

	for index, bucket := range rangeAggRes.Buckets {
		deletedNodePeriods[index].Count = int(bucket.DocCount)
		deletedNodePeriods[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		deletedNodePeriods[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return deletedNodePeriods, nil
}

// GetCreateCountsTimeSeries - creates a daily time series of the total amount of nodes that have
// been created within or before the time range. The time series is between the startTime and endTime provided.
// The startTime and endTime must at least be 24 hours apart. When greater than 24 hours, it must be in
// multiples 24 hour blocks.
func (es Backend) GetCreateCountsTimeSeries(startTime, endTime time.Time,
	filters map[string][]string) ([]backend.CountPeroid, error) {
	var (
		aggTag    = "date_range"
		mainQuery = newBoolQueryFromFilters(filters)
	)

	createNodePeriods := make([]backend.CountPeroid, getNumberOf24hBetween(startTime, endTime))

	// This is using the custom_search_aggs_bucket_date_range because it need to set the missing option
	dateRangeAgg := NewDateRangeAggregation().Field(backend.Created).
		Format("yyyy-MM-dd'T'HH:mm:ssZ").
		Missing(time.Time{}.Format(time.RFC3339)) // count all nodes that do not have a create date

	for index := 0; index < len(createNodePeriods); index++ {
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
		for index := 0; index < len(createNodePeriods); index++ {
			createNodePeriods[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			createNodePeriods[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			createNodePeriods[index].Count = 0
		}
		return createNodePeriods, nil
	}

	if len(rangeAggRes.Buckets) != len(createNodePeriods) {
		return []backend.CountPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(createNodePeriods), len(rangeAggRes.Buckets))
	}

	for index, bucket := range rangeAggRes.Buckets {
		createNodePeriods[index].Count = int(bucket.DocCount)
		createNodePeriods[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		createNodePeriods[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return createNodePeriods, nil
}

// GetCheckinCountsTimeSeries create a daily time series of unique nodes that have reported a runs
// between the startTime and endTime provided.
// The startTime and endTime must at least be 24 hours apart. When greater than 24 hours, it must be in
// multiples 24 hour blocks.
func (es Backend) GetCheckinCountsTimeSeries(startTime, endTime time.Time,
	filters map[string][]string) ([]backend.CountPeroid, error) {
	var (
		dateHistoTag = "dateHisto"
		mainQuery    = newBoolQueryFromFilters(filters)
		innerAggTag  = "inneragg"
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
		SubAggregation(innerAggTag,
			elastic.NewCardinalityAggregation().Field(backend.Id)) // count how many unique nodes are in this bucket

	searchResult, err := es.client.Search().
		Index(IndexConvergeHistory).
		Query(mainQuery).
		Aggregation(dateHistoTag, bucketHist).
		Do(context.Background())
	if err != nil {
		return []backend.CountPeroid{}, err
	}

	checkInPeriods := make([]backend.CountPeroid, getNumberOf24hBetween(startTime, endTime))
	dateHistoRes, found := searchResult.Aggregations.DateHistogram(dateHistoTag)
	if !found {
		// This case is if there are no runs for the entire range of the time series
		// We are creating the buckets manually with zero check-ins
		for index := 0; index < len(checkInPeriods); index++ {
			checkInPeriods[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			checkInPeriods[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			checkInPeriods[index].Count = 0
		}
		return checkInPeriods, nil
	}

	if len(dateHistoRes.Buckets) != len(checkInPeriods) {
		return []backend.CountPeroid{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(checkInPeriods), len(dateHistoRes.Buckets))
	}

	for index, bucket := range dateHistoRes.Buckets {
		item, found := bucket.Aggregations.Cardinality(innerAggTag)
		if found {
			checkInPeriods[index].Count = int(*item.Value)
		} else {
			checkInPeriods[index].Count = 0
		}

		checkInPeriods[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
		checkInPeriods[index].End = startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
	}

	return checkInPeriods, nil
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

// This function provides the status of runs for each 24-hour duration. For multiple runs in one
// 24-hour duration, the most recent failed run will be returned. If there are no failed runs the
// most recent successful run will be returned. If no runs are found in the 24-hour duration, the
// status will be "missing" and no run will be returned.
func (es Backend) GetNodeRunsDailyStatusTimeSeries(nodeID string, startTime,
	endTime time.Time) ([]backend.RunDurationStatus, error) {
	var (
		endDateAggTag = "sort_descending_on_end_time"
		statusAggTag  = "status_agg"
		IDAggTag      = "id_agg"
		outerAggTag   = "date_histogram_on_end_time"
		mainQuery     = elastic.NewBoolQuery()
		missingTag    = "missing"
	)

	// Filter for Runs for only one node
	mainQuery = mainQuery.Must(elastic.NewBoolQuery().Must(elastic.NewTermsQuery(backend.Id, nodeID)))

	// Filters the runs down to the needed time range
	rangeQuery, _ := newRangeQuery(startTime.Format(time.RFC3339),
		endTime.Format(time.RFC3339), backend.RunEndTime)

	mainQuery = mainQuery.Must(rangeQuery)

	// for each 24-hour bucket create sub buckets for each status
	// for each status find the most recent run.
	innerAgg := elastic.NewTermsAggregation().Field(backend.StatusTag)
	innerAgg.SubAggregation(endDateAggTag,
		elastic.NewTermsAggregation().Field(backend.RunEndTime).OrderByTerm(false).Size(1).
			SubAggregation(IDAggTag, elastic.NewTermsAggregation().Field(backend.RunIDTag)))

	bucketHist := elastic.NewDateHistogramAggregation().Field(backend.RunEndTime).
		Interval("24h"). // do not use "1d" because daylight savings time could have 23 or 25 hours
		MinDocCount(0).  // needed to return empty buckets
		ExtendedBounds(
			startTime.Format(time.RFC3339), endTime.Format(time.RFC3339)). // needed to return empty buckets
		Format("yyyy-MM-dd'T'HH:mm:ssZ").
		TimeZone(getTimezoneWithStartOfDayAtUtcHour(startTime)). // needed start the buckets at the beginning of the current hour.
		SubAggregation(statusAggTag, innerAgg)

	searchResult, err := es.client.Search().
		Index(IndexConvergeHistory).
		Query(mainQuery).
		Aggregation(outerAggTag, bucketHist).
		Do(context.Background())
	if err != nil {
		return []backend.RunDurationStatus{}, err
	}

	LogQueryPartMin(IndexConvergeHistory, searchResult.Aggregations,
		"GetNodeDailyStatusTimeSeries response")

	runStatusDurations := make([]backend.RunDurationStatus, getNumberOf24hBetween(startTime, endTime))
	dateHistoRes, outerAggfound := searchResult.Aggregations.DateHistogram(outerAggTag)
	if !outerAggfound {
		// This case is if there are no runs for the entire range of the time series
		// We are creating the buckets manually with missing status
		for index := 0; index < len(runStatusDurations); index++ {
			runStatusDurations[index].Start = startTime.Add(time.Hour * 24 * time.Duration(index))
			runStatusDurations[index].End = startTime.Add(time.Hour * 24 *
				time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)
			runStatusDurations[index].Status = missingTag
		}
		return runStatusDurations, nil
	}

	if len(dateHistoRes.Buckets) != len(runStatusDurations) {
		return []backend.RunDurationStatus{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(runStatusDurations), len(dateHistoRes.Buckets))
	}

	for index, dailyBucket := range dateHistoRes.Buckets {
		start := startTime.Add(time.Hour * 24 * time.Duration(index))
		end := startTime.Add(time.Hour * 24 *
			time.Duration(index)).Add(time.Hour * 24).Add(-time.Millisecond)

		runStatusDurations[index].Start = start
		runStatusDurations[index].End = end

		statusAgg, statusAggFound := dailyBucket.Aggregations.Terms(statusAggTag)
		if !statusAggFound || len(statusAgg.Buckets) == 0 {
			// no runs were found
			runStatusDurations[index].Status = missingTag
			continue
		}

		statuses := collectLatestStatusBuckets(statusAgg.Buckets, endDateAggTag, IDAggTag)
		if len(statuses) == 0 {
			runStatusDurations[index].Status = missingTag
			continue
		}

		selectedStatus := selectPriorityStatusRun(statuses)

		runStatusDurations[index].Status = selectedStatus.Status
		runStatusDurations[index].RunID = selectedStatus.RunID
	}

	return runStatusDurations, nil
}

// Select the most recent 'failure' run over all 'success' runs
// If there are no 'failure' runs select the most recent 'success' run
func selectPriorityStatusRun(latestStatusBucketCollection []LatestStatusBucket) LatestStatusBucket {
	for _, latestStatusBucket := range latestStatusBucketCollection {
		if latestStatusBucket.Status == "failure" {
			return latestStatusBucket
		}
	}

	return latestStatusBucketCollection[0]
}

type LatestStatusBucket struct {
	Date   float64
	Status string
	RunID  string
}

// "buckets":[
// 	{
// 			"key":"failure",
// 			"doc_count":1,
// 			"sort_descending_on_end_time":{
// 				"doc_count_error_upper_bound":0,
// 				"sum_other_doc_count":0,
// 				"buckets":[
// 						{
// 							"key":1584183779000,
// 							"key_as_string":"2020-03-14T11:02:59.000Z",
// 							"doc_count":1,
// 							"id_agg":{
// 									"doc_count_error_upper_bound":0,
// 									"sum_other_doc_count":0,
// 									"buckets":[
// 										{
// 												"key":"3",
// 												"doc_count":1
// 										}
// 									]
// 							}
// 						}
// 				]
// 			}
// 	},
// 	{
// 			"key":"successful",
// 			"doc_count":1,
// 			"sort_descending_on_end_time":{
// 				"doc_count_error_upper_bound":0,
// 				"sum_other_doc_count":0,
// 				"buckets":[
// 						{
// 							"key":1584187379000,
// 							"key_as_string":"2020-03-14T12:02:59.000Z",
// 							"doc_count":1,
// 							"id_agg":{
// 									"doc_count_error_upper_bound":0,
// 									"sum_other_doc_count":0,
// 									"buckets":[
// 										{
// 												"key":"4",
// 												"doc_count":1
// 										}
// 									]
// 							}
// 						}
// 				]
// 			}
// 	}
// ]
func collectLatestStatusBuckets(statusBuckets []*elastic.AggregationBucketKeyItem,
	endDateAggTag, IDAggTag string) []LatestStatusBucket {
	latestStatusBucketCollection := make([]LatestStatusBucket, 0)
	for _, statusBucket := range statusBuckets {
		status := statusBucket.Key.(string)
		endDateAgg, endDateAggFound := statusBucket.Aggregations.Terms(endDateAggTag)
		if !endDateAggFound {
			log.Errorf("End Date aggregation not found for status %q", status)
			continue
		}

		if len(endDateAgg.Buckets) != 1 {
			log.Errorf("End Date aggregation did not have one entry for status %q", status)
			continue
		}

		latestRunEndDateBucket := endDateAgg.Buckets[0]

		runIDAgg, runIDFound := latestRunEndDateBucket.Aggregations.Terms(IDAggTag)
		if !runIDFound {
			log.Errorf("Run ID not found for status %q", status)
			continue
		}

		date := latestRunEndDateBucket.Key.(float64)

		if len(runIDAgg.Buckets) != 1 {
			log.Errorf("Run ID aggregation did not have one entry for status %q and date %f", status, date)
			continue
		}

		latestRunIDBucket := runIDAgg.Buckets[0]
		runID := latestRunIDBucket.Key.(string)

		latestStatusBucketCollection = append(latestStatusBucketCollection,
			LatestStatusBucket{
				Date:   date,
				RunID:  runID,
				Status: status,
			})
	}

	return latestStatusBucketCollection
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
