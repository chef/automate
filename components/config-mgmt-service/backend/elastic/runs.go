package elastic

import (
	"context"
	"encoding/json"
	"strings"
	"time"

	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"

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
