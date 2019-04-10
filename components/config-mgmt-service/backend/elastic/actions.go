package elastic

import (
	"context"
	"encoding/json"
	"fmt"
	"math"
	"strconv"
	"time"

	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
)

// GetPolicyCookbooks Returns most recent results for policy name and list of cookbook locks given a revision id
func (es Backend) GetPolicyCookbooks(revisionID string) (backend.PolicyCookbooks, error) {
	var policyAction backend.Action
	var policyData map[string]interface{}
	var policyCookbooks backend.PolicyCookbooks
	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(elastic.NewTermQuery("revision_id", revisionID))

	searchResult, err := es.client.Search().
		Query(boolQuery).
		Index(IndexAction).
		Sort(ActionFieldTimestamp, false). //Want more recent first (ascending = false)
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return policyCookbooks, err
	}

	if searchResult.Hits.TotalHits == 0 {
		return policyCookbooks, errors.New(errors.ActionNotFound, fmt.Sprintf("No policy action found for revision ID: %s", revisionID))
	}

	source := searchResult.Hits.Hits[0].Source
	err = json.Unmarshal(*source, &policyAction)
	if err != nil {
		log.WithFields(log.Fields{
			"object": source,
		}).WithError(err).Debug("Unable to unmarshal the policy action object")
		return policyCookbooks, err
	}
	err = json.Unmarshal([]byte(policyAction.Data), &policyData)
	if err != nil {
		log.WithFields(log.Fields{
			"object": policyAction.Data,
		}).WithError(err).Debug("Unable to unmarshal the action data object for a policy action")
		return policyCookbooks, err
	}
	cookbookLocksData := policyData["cookbook_locks"].(map[string]interface{})
	policyCookbookLocks := make([]backend.PolicyCookbookLock, 0, len(cookbookLocksData))
	for cookbook, cData := range cookbookLocksData {
		cDataMap := cData.(map[string]interface{})
		cl := backend.PolicyCookbookLock{
			CookbookName: cookbook,
			PolicyID:     EmptyStringIfNil(cDataMap["identifier"]),
		}
		policyCookbookLocks = append(policyCookbookLocks, cl)
	}
	policyCookbooks.PolicyName = EmptyStringIfNil(policyData["name"])
	policyCookbooks.CookbookLocks = policyCookbookLocks
	return policyCookbooks, nil
}

// GetActions Returns a filtered list of actions
// {
//   "query": {
//     "bool": {
//       "must": {
//         "range": {
//           "recorded_at": {
//             "format": "yyyy-MM-dd||yyyy-MM-dd-HH:mm:ss||yyyy-MM-dd'T'HH:mm:ssZ",
//             "from": "2018-01-27",
//             "include_lower": true,
//             "include_upper": true,
//             "to": "2018-02-02"
//           }
//         }
//       }
//     }
//   },
//   "search_after": [
//     1517601486000,
//     "43ae5aae-355a-4473-b1b8-fea53ccae000"
//   ],
//   "size": 5,
//   "sort": [
//     {
//       "recorded_at": {
//         "order": "desc"
//       }
//     },
//     {
//       "id": {
//         "order": "desc"
//       }
//     }
//   ]
// }
// cursorDate - date where this page will start from. Format: RFC3339 (2006-01-02T15:04:05Z07:00)
// for the first page leave empty
// cursorID - the action ID where this page will start.
// pageSize - the size of each page
// ascending - should the page be
func (es Backend) GetActions(filters map[string][]string, start, end time.Time,
	pageSize int, cursorDate time.Time, cursorID string, ascending bool) ([]backend.Action, int64, error) {
	var (
		actions   []backend.Action
		mainQuery = newBoolQueryFromFilters(filters)
	)

	rangeQuery, ok := newRangeQueryTime(start, end, ActionFieldTimestamp)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	searchService := es.client.Search().
		Query(mainQuery).
		Index(IndexAction).
		Size(pageSize).
		Sort(ActionFieldTimestamp, ascending).
		Sort(ActionFieldID, ascending)

	if cursorDate.Unix() != 0 && cursorID != "" {
		milliseconds := cursorDate.UnixNano() / int64(time.Millisecond)
		searchService = searchService.SearchAfter(milliseconds, cursorID) // the date has to be in milliseconds
	}

	searchResult, err := searchService.Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return actions, 0, err
	}

	for _, hit := range searchResult.Hits.Hits {
		var action backend.Action
		err := json.Unmarshal(*hit.Source, &action)
		if err != nil {
			log.WithFields(log.Fields{
				"object": hit.Source,
			}).WithError(err).Debug("Error unmarshalling the action object")
		} else {
			actions = append(actions, action)
		}
	}

	// sort actions, because if ascending = true they will not be in the descending order.
	if ascending {
		es.reverseActions(actions)
	}

	return actions, searchResult.Hits.TotalHits, nil
}

// GetActionEventTypeCounts - counts the number of action event types
func (es Backend) GetActionEventTypeCounts(filters map[string][]string, start, end time.Time) (map[string]int64, error) {
	return es.getActionCounts("entity_type", filters, start, end)
}

// GetActionEventTaskCounts - counts the number of action event tasks
func (es Backend) GetActionEventTaskCounts(filters map[string][]string, start, end time.Time) (map[string]int64, error) {
	return es.getActionCounts("task", filters, start, end)
}

func (es Backend) getActionCounts(countTerm string, filters map[string][]string, start time.Time, end time.Time) (map[string]int64, error) {
	counts := map[string]int64{}

	mainQuery := newBoolQueryFromFilters(filters)
	rangeQuery, ok := newRangeQueryTime(start, end, ActionFieldTimestamp)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	buckets, err := es.getAggregationBucket(mainQuery, IndexAction, countTerm)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	for _, bucket := range buckets {
		counts[bucket.Key.(string)] = bucket.DocCount
	}

	return counts, nil
}

// GetEventString event_type filters
// start = "2018-01-20T00:00:00-08:00" RFC3339 format
// end = "2018-01-26T23:59:59-08:00" RFC3339 format
// The time zone is needed to start the time bucketing on the hour requested.
// Bucketing every 3 hours the buckets start at the beginning of the day.
// 0 - 3, 3 - 6, 6 - 9, 9 - 12, 12 - 15, 15 - 18, 18 - 21, 21 - 0
// One can not have 3 hour buckets start at 02:00 or any time within the above buckets
// This can be fixed by setting the time zone. The bucket will start at the
// beginning of the day in that timezone.
// The Elastic Search request
// {
//   "aggregations": {
//     "dateHisto": {
//       "aggregations": {
//         "items": {
//           "terms": {
//             "field": "entity_type"
//           }
//         }
//       },
//       "date_histogram": {
//         "extended_bounds": {
//           "max": "2018-01-26T23:59:59-08:00",
//           "min": "2018-01-20T00:00:00-08:00"
//         },
//         "field": "recorded_at",
//         "format": "yyyy-MM-dd'T'HH:mm:ssZ",
//         "interval": "3h",
//         "min_doc_count": 0,
//         "time_zone": "-08:00"
//       }
//     }
//   },
//   "query": {
//     "bool": {
//       "must": [
//         {
//           "term": {
//             "task": "delete"
//           }
//         },
//         {
//           "range": {
//             "recorded_at": {
//               "format": "yyyy-MM-dd||yyyy-MM-dd-HH:mm:ss||yyyy-MM-dd'T'HH:mm:ssZ",
//               "from": "2018-01-20T00:00:00-08:00",
//               "include_lower": true,
//               "include_upper": true,
//               "to": "2018-01-26T23:59:59-08:00"
//             }
//           }
//         }
//       ]
//     }
//   }
// }
// bucketSizeInHours - 24 must be divisible by this number
// For example 1, 2, 3, 4, 6, 8, 12, and 24 are valid. Where
// 5, 7, 9, 10, 11, and 13 are not valid values.
func (es Backend) GetEventString(filters map[string][]string, startDate, endDate string, timezone string,
	bucketSizeInHours int, eventAction string) (backend.EventString, error) {
	var (
		bucketSize        = strconv.Itoa(bucketSizeInHours) + "h"
		eventTypeItems    = "items"
		dateHistoTag      = "dateHisto"
		timeFormatRFC3339 = "yyyy-MM-dd'T'HH:mm:ssZ"
		mainQuery         = newBoolQueryFromFilters(filters)
	)

	loc, err := time.LoadLocation(timezone)
	if err != nil {
		return backend.EventString{}, err
	}

	startTime, err := es.createStartTime(startDate, loc)
	if err != nil {
		return backend.EventString{}, err
	}

	endTime, err := es.createEndTime(endDate, loc)
	if err != nil {
		return backend.EventString{}, err
	}

	endTimeAdjustedForDaylightSavings := adjustEndTimeForDaylightSavings(startTime, endTime, bucketSizeInHours)

	mainQuery = mainQuery.Must(elastic.NewTermQuery("task", eventAction))

	rangeQuery, _ := newRangeQuery(
		startTime.Format(time.RFC3339),
		endTimeAdjustedForDaylightSavings.Format(time.RFC3339),
		ActionFieldTimestamp)
	mainQuery = mainQuery.Must(rangeQuery)

	bucketHist := elastic.NewDateHistogramAggregation().Field("recorded_at").
		Interval(bucketSize).
		MinDocCount(0). // needed to return empty buckets
		ExtendedBounds(
			startTime.Format(time.RFC3339), endTimeAdjustedForDaylightSavings.Format(time.RFC3339)). // needed to return empty buckets
		Format(timeFormatRFC3339).
		TimeZone(timezone). // needed start the buckets at the beginning of the day.
		SubAggregation(eventTypeItems,
			elastic.NewTermsAggregation().Field("entity_type"))

	searchResult, err := es.client.Search().
		Index(IndexAction).
		Query(mainQuery).
		Aggregation(dateHistoTag, bucketHist).
		Do(context.Background())

	if err != nil {
		return backend.EventString{}, err
	}

	dateHistoRes, found := searchResult.Aggregations.DateHistogram(dateHistoTag)
	numberOfBuckets := getTotalNumberOfBucketsAllowed(bucketSizeInHours, startTime, endTime)

	// When no actions are in Elasticsearch 'found' is false.
	// Here we are creating the empty buckets to return
	if !found {
		return backend.EventString{
			EventAction:      eventAction,
			EventsCollection: make([]backend.EventCollection, numberOfBuckets),
		}, nil
	}

	// If for some reason there are less buckets returned then we expected
	if len(dateHistoRes.Buckets) < numberOfBuckets {
		log.WithFields(log.Fields{
			"numberOfBucketsReturned": len(dateHistoRes.Buckets),
			"numberOfBucketsExpected": numberOfBuckets,
			"bucketSizeInHours":       bucketSizeInHours,
			"startDate":               startDate,
			"endDate":                 endDate,
			"timezone":                timezone,
		}).Warn("The number of buckets returned is less than expected")
		numberOfBuckets = len(dateHistoRes.Buckets)
	}

	eventsCollection := make([]backend.EventCollection, numberOfBuckets)
	for index := 0; index < numberOfBuckets; index++ {
		bucket := dateHistoRes.Buckets[index]

		item, found := bucket.Aggregations.Terms(eventTypeItems)
		if !found {
			return backend.EventString{}, errors.NewBackendError("Item '%s' not found", eventTypeItems)
		}

		if len(item.Buckets) > 0 {
			eventsCollection[index].EventsCount = make([]backend.EventCount, len(item.Buckets))
			for c, buc := range item.Buckets {
				eventsCollection[index].EventsCount[c].Name = buc.Key.(string)
				eventsCollection[index].EventsCount[c].Count = buc.DocCount
			}
		}
	}

	return backend.EventString{
		EventAction:      eventAction,
		EventsCollection: eventsCollection,
	}, nil
}

// This corrects the problem when the start or end of daylight saving is within the request date range.
// We want to allways return the number of buckets that equals 24 hours in a day.
// When daylight savings start we want to add an hour
// When daylight savings end we want to remove an hour.
// This is only for request with bucket sizes of 1 and 2 hours.
// With requests higher than 2 hours elasticsearch handles the problem.
// There is a special case with bucket size of 2 hours with the start of daylight savings, where an extra bucket is
// returned from Elasticsearch. This bucket is removed.
func adjustEndTimeForDaylightSavings(start, end time.Time, bucketSizeInHours int) time.Time {
	startUpdate := time.Date(start.Year(), start.Month(), start.Day(),
		0, 0, 0, 0, start.Location())
	endUpdate := time.Date(end.Year(), end.Month(), end.Day(),
		0, 0, 0, 0, end.Location()).AddDate(0, 0, 1)
	numberOfHoursDiff := endUpdate.Sub(startUpdate).Hours()

	days := int(math.Round(numberOfHoursDiff / 24.0))
	expectedNumberOfHours := days * 24

	expectedActualDiff := expectedNumberOfHours - int(numberOfHoursDiff)

	if bucketSizeInHours <= 2 && expectedActualDiff > 0 {
		end = end.Add(time.Hour * time.Duration(expectedActualDiff))
	}

	return end
}

func getTotalNumberOfBucketsAllowed(bucketSizeInHours int, start, end time.Time) int {
	numberOfDaysBetween := getNumberOfDaysBetween(start, end)
	return (24 / bucketSizeInHours) * numberOfDaysBetween
}

func getNumberOfDaysBetween(start, end time.Time) int {
	startUpdate := time.Date(start.Year(), start.Month(), start.Day(),
		0, 0, 0, 0, start.Location())
	endUpdate := time.Date(end.Year(), end.Month(), end.Day(),
		0, 0, 0, 0, end.Location()).AddDate(0, 0, 1)
	return int(math.Round(endUpdate.Sub(startUpdate).Hours() / 24.0))
}

func (es Backend) createStartTime(startDate string, loc *time.Location) (time.Time, error) {
	startTime, err := time.ParseInLocation("2006-01-02", startDate, loc)
	if err != nil {
		return startTime, err
	}

	// At the beginning of the day
	return time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location()), nil
}

func (es Backend) createEndTime(endDate string, loc *time.Location) (time.Time, error) {
	endTime, err := time.ParseInLocation("2006-01-02", endDate, loc)
	if err != nil {
		return endTime, err
	}

	// At the end of the day
	return time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		23, 59, 59, 0, endTime.Location()), nil
}

func (es Backend) reverseActions(actions []backend.Action) {
	length := len(actions)
	for i := length/2 - 1; i >= 0; i-- {
		opp := length - 1 - i
		actions[i], actions[opp] = actions[opp], actions[i]
	}
}
