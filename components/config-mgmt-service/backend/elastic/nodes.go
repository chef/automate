package elastic

import (
	"context"
	"encoding/json"
	"fmt"
	"reflect"
	"sort"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/config-mgmt-service/params"
)

var (
	getInventoryNodesFieldsToFetch []string
)

func init() {
	inventoryNodeType := reflect.TypeOf(backend.InventoryNode{})
	for i := 0; i < inventoryNodeType.NumField(); i++ {
		getInventoryNodesFieldsToFetch = append(getInventoryNodesFieldsToFetch, inventoryNodeType.Field(i).Tag.Get("json"))
	}
}

// NodeExists - Does a node with entity_uuid == 'nodeID' exists
func (es Backend) NodeExists(nodeID string, filters map[string][]string) (bool, error) {
	filters["exists"] = []string{"true"}
	filters["entity_uuid"] = []string{nodeID}
	filtersQuery := newBoolQueryFromFilters(filters)

	searchResult, err := es.client.Search().
		Query(filtersQuery).
		Index(IndexNodeState).
		Do(context.Background())
	if err != nil {
		return false, err
	}

	return searchResult.Hits.TotalHits > 0, nil
}

// GetInventoryNodes - Collect inventory nodes from elasticsearch. This function allows
// non-random access pagination and custom selection of how the docs are sorted.
//
// TODO(ssd) 2019-04-23: The sortField argument is ignored in this
// call. The linter pointed out that it was previously ignored and an
// attempt to not ignore it caused multiple tests to fail.
func (es Backend) GetInventoryNodes(ctx context.Context, start time.Time,
	end time.Time, filters map[string][]string, cursorDate time.Time,
	cursorID string, pageSize int, _ string,
	ascending bool) ([]backend.InventoryNode, error) {

	mainQuery := newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQueryTime(start, end, backend.CheckIn)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	fetchSource := elastic.NewFetchSourceContext(true).Include(getInventoryNodesFieldsToFetch...)

	searchService := es.client.Search().
		Query(mainQuery).
		Index(IndexNodeState).
		Size(pageSize).
		Sort(backend.CheckIn, ascending).
		Sort(NodeFieldID, ascending).
		FetchSourceContext(fetchSource)

	if cursorDate.Unix() != 0 && cursorID != "" {
		milliseconds := cursorDate.UnixNano() / int64(time.Millisecond)
		searchService = searchService.SearchAfter(milliseconds, cursorID) // the date has to be in milliseconds
	}

	searchResult, err := searchService.Do(ctx)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	var nodes []backend.InventoryNode
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			var n backend.InventoryNode
			err := json.Unmarshal(*hit.Source, &n)
			if err != nil {
				log.WithError(err).Error("Error unmarshalling the node object")
			} else {
				nodes = append(nodes, n)
			}
		}
	}

	return nodes, nil
}

func (es Backend) GetNodesPageByCursor(ctx context.Context, start time.Time,
	end time.Time, filters map[string][]string, cursorValue interface{},
	cursorID string, pageSize int, sortField string,
	ascending bool) ([]backend.Node, error) {

	mainQuery := newBoolQueryFromFilters(filters)

	if sortField == "" {
		sortField = backend.CheckIn
	}

	rangeQuery, ok := newRangeQueryTime(start, end, backend.CheckIn)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	searchService := es.client.Search().
		Query(mainQuery).
		Index(IndexNodeState).
		Size(pageSize).
		Sort(sortField, ascending).
		Sort(NodeFieldID, ascending)

	if cursorValue != nil && cursorID != "" {
		switch v := cursorValue.(type) {
		case time.Time:
			// the date has to be in milliseconds
			milliseconds := v.UnixNano() / int64(time.Millisecond)
			searchService = searchService.SearchAfter(milliseconds, cursorID)
		case string:
			// strings have to be lowercase
			lower := strings.ToLower(v)
			searchService = searchService.SearchAfter(lower, cursorID)
		default:
			searchService = searchService.SearchAfter(cursorValue, cursorID)
		}
	}

	searchResult, err := searchService.Do(ctx)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	var nodes []backend.Node
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			var n backend.Node
			err := json.Unmarshal(*hit.Source, &n)
			if err != nil {
				log.WithError(err).Error("Error unmarshalling the node object")
			} else {
				nodes = append(nodes, n)
			}
		}
	}

	return nodes, nil
}

// GetNodes This function implements pagination that is being handle by
// elasticsearch by passing the `from` & `size` parameters
//
// @param [order]    The sort order asc or desc
// @param [page]     The page number to return
// @param [perPage]  Number of nodes to return
// @param [sort]     The field to sort on
// @param [filters]  The filters to apply to our ES query
// @return           An array of backend.Node
//
// The ES query we use is:
// {
//   "sort": [{$sort:{"order":$order}}],
//   "from": $start,
//   "size": $perPage,
//   "query":{
//     "bool":{
//       "filter":{
//         "term":{
//           "exists":"true"
//         }
//       }
//     }
//   }
// }
func (es Backend) GetNodes(page int, perPage int, sortField string,
	ascending bool, filters map[string][]string, startDate, endDate string) ([]backend.Node, error) {
	// Decrement one to the page since we must start from zero
	startPage := perPage * (page - 1)

	// Adding the exists = true filter to the list of filters, because nodes
	// have documents that persist in elasticsearch to hold historical data
	// even after the node no longer exists
	filters["exists"] = []string{"true"}

	mainQuery := newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQuery(startDate, endDate, NodeCheckin)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	searchResult, err := es.client.Search().
		Query(mainQuery).
		Index(IndexNodeState).
		Sort(sortField, ascending).    // sort by 'sortField', in 'ascending' [true, false]
		From(startPage).Size(perPage). // take documents from {start} to {perPage}
		Do(context.Background())
	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	var nodes []backend.Node
	if searchResult.Hits.TotalHits > 0 {
		// Iterate through every Hit and unmarshal the Source into a backend.Node
		for _, hit := range searchResult.Hits.Hits {
			var n backend.Node
			err := json.Unmarshal(*hit.Source, &n)
			if err != nil {
				log.WithError(err).Error("Error unmarshalling the node object")
			} else {
				nodes = append(nodes, n)
			}
		}
	}

	return nodes, nil
}

// GetNodesCounts - get the number of successful, failure, and missing nodes
func (es Backend) GetNodesCounts(filters map[string][]string,
	startDate string, endDate string) (backend.NodesCounts, error) {
	var ns = *new(backend.NodesCounts)

	// Adding the exists = true filter to the list of filters, because nodes
	// have documents that persist in elasticsearch to hold historical data
	// even after the node no longer exists
	filters["exists"] = []string{"true"}

	mainQuery := newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQuery(startDate, endDate, NodeCheckin)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	searchTerm := "status"

	statusNodesBuckets, err := es.getAggregationBucket(mainQuery, IndexNodeState, searchTerm)
	if err != nil {
		return ns, err
	}

	for _, bucket := range statusNodesBuckets {
		switch bucket.Key {
		case "success":
			ns.Success = bucket.DocCount
		case "failure":
			ns.Failure = bucket.DocCount
		case "missing":
			ns.Missing = bucket.DocCount
		}
	}

	// Compute the total number of nodes
	ns.ComputeTotalNodes()

	return ns, nil
}

// GetNodeMetadataCounts - For each type of field provided return distinct values the amount for each.
// For example, if the 'platform' field is requested 'windows' 10, 'redhat' 5, and 'ubuntu' 8
// could be returned. The number next to each represents the number of nodes with that type of platform.
//
// Filters of the same type as the aggregation are removed. For example, if there is a filter of
// type platform:'windows' and the type 'platform' is requested the filter platform:'windows' will be removed.
func (es Backend) GetNodeMetadataCounts(filters map[string][]string,
	types []string, startDate, endDate string) ([]backend.TypeCount, error) {
	var aggregationTerm = "inner_filter"

	searchSource := createTypeAggs(filters, types, startDate, endDate, aggregationTerm)

	source, _ := searchSource.Source()
	LogQueryPartMin(IndexNodeState, source, "GetNodeMetadataCounts request")

	searchResult, err := es.client.Search().
		SearchSource(searchSource).
		Index(IndexNodeState).
		Do(context.Background())
	if err != nil {
		return []backend.TypeCount{}, err
	}

	LogQueryPartMin(IndexNodeState, searchResult.Aggregations, "GetNodeMetadataCounts response")

	// no matching nodes found
	if searchResult.TotalHits() == 0 {
		fieldCountCollection := make([]backend.TypeCount, len(types))
		for index, fieldType := range types {
			fieldCountCollection[index].Values = []backend.ValueCount{}
			fieldCountCollection[index].Type = fieldType
		}
		return fieldCountCollection, nil
	}

	fieldCountCollection := make([]backend.TypeCount, len(types))
	for index, searchTerm := range types {
		outerAgg, found := searchResult.Aggregations.Terms(searchTerm)
		if !found {
			return nil, errors.NewBackendError("Aggregation term %q not found", searchTerm)
		}
		terms := make([]backend.ValueCount, 0)
		for _, bucket := range outerAgg.Buckets {
			filterCounts, found := bucket.Aggregations.Filter(aggregationTerm)
			if !found {
				return []backend.TypeCount{},
					errors.NewBackendError("Aggregation term %q not found", aggregationTerm)
			}

			// Are all the found values filtered out
			if filterCounts.DocCount > 0 {
				terms = append(terms, backend.ValueCount{
					Count: int(filterCounts.DocCount),
					Value: bucket.Key.(string),
				})
			}
		}
		fieldCountCollection[index].Values = terms
		fieldCountCollection[index].Type = searchTerm
	}

	return fieldCountCollection, nil
}

// GetAttribute Get request for the attribute using the Doc ID
func (es Backend) GetAttribute(nodeID string) (backend.NodeAttribute, error) {
	var nodeAttribute backend.NodeAttribute

	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(elastic.NewTermQuery("entity_uuid", nodeID))

	getResult, err := es.client.Search().
		Query(boolQuery).
		Index(IndexNodeAttribute).
		Do(context.Background())

	if err != nil {
		return nodeAttribute, err
	}

	if getResult.Hits.TotalHits == 0 {
		return nodeAttribute, errors.New(errors.NodeAttributeNotFound, "No attributes found")
	}

	source := getResult.Hits.Hits[0].Source // We only want one attribute
	err = json.Unmarshal(*source, &nodeAttribute)
	if err != nil {
		log.WithFields(log.Fields{
			"object": source,
		}).WithError(err).Debug("Unable to unmarshal the node attributes")
		return nodeAttribute, err
	}

	return nodeAttribute, nil
}

func (es Backend) GetErrors(size int32, filters map[string][]string) ([]*backend.ChefErrorCount, error) {
	// Return the top 10 most-frequently occurring combinations of Chef Infra
	// error type (class) and error message.
	//
	// Elasticsearch's clustered design makes this query kind of awkward. Though
	// this query could be done with a nested terms aggregation, that has some
	// accuracy issues when clustered:
	// https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-terms-aggregation.html#search-aggregations-bucket-terms-aggregation-approximate-counts
	//
	// One option is to concatenate the fields on ingest into a single field so
	// we can do a single term aggregation on that. But that approach is pretty
	// ugly and requires some unappealing tradeoffs when we want to fetch/display
	// the data.
	//
	// Contrarily, the composite aggregation matches our use case here more
	// closely, but it doesn't provide any helpful ordering and forces you to
	// scroll large result sets. So we fetch all pages of the result and sort
	// them here.
	//
	// The curl version of this query is kinda like this:
	// curl -X GET "localhost:10141/node-state/_search?pretty" -H 'Content-Type: application/json' -d'
	// {
	//   "size": 10,
	//   "query": {
	//     "bool": {
	//       "must": [
	//         { "match": { "status": "failure" } }
	//       ]
	//     }
	//   },
	//   "aggs": {
	//     "group_by_error_type_and_message": {
	//       "composite": {
	//         "sources": [
	//           { "error_type": {"terms": { "field": "error_type" } } },
	//           { "error_message": { "terms": { "field": "error_message" } } }
	//         ]
	//       }
	//     }
	//   }
	// }
	// '
	queryWithFilters := newBoolQueryFromFilters(filters)
	queryWithFilters.Must(elastic.NewTermQuery("status", "failure"))

	agg := elastic.NewCompositeAggregation()
	agg.Sources(
		elastic.NewCompositeAggregationTermsValuesSource("error_type").Field("error_type"),
		elastic.NewCompositeAggregationTermsValuesSource("error_message").Field("error_message"),
	)
	agg.Size(1000)

	var allResultsCollected bool
	var totalQueriesRan int
	chefErrs := []*backend.ChefErrorCount{}

	for !allResultsCollected {
		// Don't hammer elastic too bad if there is a bug or other unforeseen
		// condition.
		if totalQueriesRan > 50 {
			return nil, fmt.Errorf("attempted too many queries to fetch top Chef error counts")
		}

		result, err := es.client.Search().
			Query(queryWithFilters).
			Index(IndexNodeState).
			Aggregation("group_by_error_type_and_message", agg).
			Size(0).
			Do(context.Background())

		if err != nil {
			return nil, err
		}

		totalQueriesRan++

		aggs := result.Aggregations

		c, aggFound := aggs.Composite("group_by_error_type_and_message")
		if !aggFound {
			return nil, fmt.Errorf("elasticsearch result for 'group_by_error_type_and_message' query did not contain the expected aggregation information")
		}

		if len(c.AfterKey) == 0 {
			allResultsCollected = true
		} else {
			agg.AggregateAfter(c.AfterKey)
		}

		for _, chefErrItem := range c.Buckets {
			errorType, ok := chefErrItem.Key["error_type"].(string)
			if !ok {
				return nil, fmt.Errorf("invalid elasticsearch response for 'group_by_error_type_and_message' aggregation query")
			}
			errorMessage, ok := chefErrItem.Key["error_message"].(string)
			if !ok {
				return nil, fmt.Errorf("invalid elasticsearch response for 'group_by_error_type_and_message' aggregation query")
			}

			chefErrs = append(chefErrs, &backend.ChefErrorCount{
				Count:   int32(chefErrItem.DocCount),
				Type:    errorType,
				Message: errorMessage,
			})
		}
	}

	sort.Slice(chefErrs, func(i, j int) bool {
		if chefErrs[i].Count != chefErrs[j].Count {
			// sort more errors first to get descending order
			return chefErrs[i].Count > chefErrs[j].Count
		} else {
			return chefErrs[i].Type < chefErrs[j].Type
		}
	})

	if size == 0 {
		size = 10
	}

	if size > 0 && int32(len(chefErrs)) > size {
		chefErrs = chefErrs[:size]
	}

	return chefErrs, nil
}

func (es Backend) GetMissingNodeDurationCounts(durations []string) ([]backend.CountedDuration, error) {
	var (
		aggTag = "MissingNodeDurationCounts"
	)
	filters := map[string][]string{
		"exists": {"true"},
	}
	mainQuery := newBoolQueryFromFilters(filters)

	dateRangeAgg := elastic.NewDateRangeAggregation().Field(backend.CheckIn)

	for _, duration := range durations {
		dateRangeAgg.AddUnboundedFromWithKey(duration, "now-"+duration)
	}

	searchResult, err := es.client.Search().
		Index(IndexNodeState).
		Query(mainQuery).
		Aggregation(aggTag, dateRangeAgg).
		Do(context.Background())
	if err != nil {
		return []backend.CountedDuration{}, err
	}

	rangeAggRes, found := searchResult.Aggregations.Range(aggTag)
	if !found {
		// This case is if there are no nodes for all the durations
		// We are creating the buckets manually with zero count
		countedDurations := make([]backend.CountedDuration, len(durations))
		for index, duration := range durations {
			countedDurations[index] = backend.CountedDuration{
				Duration: duration,
				Count:    0,
			}
		}
		return countedDurations, nil
	}

	if len(rangeAggRes.Buckets) != len(durations) {
		return []backend.CountedDuration{}, errors.NewBackendError(
			"The number of buckets found is incorrect expected %d actual %d",
			len(durations), len(rangeAggRes.Buckets))
	}

	countedDurations := make([]backend.CountedDuration, len(rangeAggRes.Buckets))
	for index, duration := range durations {
		countedDurations[index].Count = findMatchingDurationCount(duration, rangeAggRes.Buckets)
		countedDurations[index].Duration = duration
	}

	return countedDurations, nil
}

func findMatchingDurationCount(key string,
	buckets []*elastic.AggregationBucketRangeItem) int32 {
	for _, bucket := range buckets {
		if key == bucket.Key {
			return int32(bucket.DocCount)
		}
	}
	return 0
}

// For each field create an aggregation that counts the number of each distinct value. Apply the
// filters on each field aggregation separately to allow the removal of the filter for the field being counted.
// {
// 	"aggregations":{
// 		 "platform":{
// 				"aggregations":{
// 					 "inner_filter":{
// 							"filter":{
// 								 "bool":{
// 										"filter":[
// 											 {
// 													"bool":{
// 														 "should":{
// 																"terms":{
// 																	 "status":[
// 																			"failure"
// 																	 ]
// 																}
// 														 }
// 													}
// 											 },
// 											 {
// 													"bool":{
// 														 "should":{
// 																"terms":{
// 																	 "exists":[
// 																			"true"
// 																	 ]
// 																}
// 														 }
// 													}
// 											 }
// 										]
// 								 }
// 							}
// 					 }
// 				},
// 				"terms":{
// 					 "field":"platform"
// 				}
// 		 },
// 		 "status":{
// 				"aggregations":{
// 					 "inner_filter":{
// 							"filter":{
// 								 "bool":{
// 										"filter":[
// 											 {
// 													"bool":{
// 														 "should":{
// 																"terms":{
// 																	 "platform.lower":[
// 																			"windows"
// 																	 ]
// 																}
// 														 }
// 													}
// 											 },
// 											 {
// 													"bool":{
// 														 "should":{
// 																"terms":{
// 																	 "exists":[
// 																			"true"
// 																	 ]
// 																}
// 														 }
// 													}
// 											 }
// 										]
// 								 }
// 							}
// 					 }
// 				},
// 				"terms":{
// 					 "field":"status"
// 				}
// 		 }
// 	}
// }
func createTypeAggs(filters map[string][]string,
	types []string, startDate, endDate string, aggregationTerm string) *elastic.SearchSource {
	searchSource := elastic.NewSearchSource()
	for _, fieldType := range types {
		// Copy the filters
		localFilters := map[string][]string{}
		for index, element := range filters {
			localFilters[index] = element
		}

		// We only want to count nodes that are not deleted
		localFilters[backend.ExistsTag] = []string{"true"}

		// remove the filter for the current search term
		delete(localFilters, params.ConvertParamToNodeStateBackendLowerFilter(fieldType))

		mainQuery := newBoolQueryFromFilters(localFilters)

		rangeQuery, ok := newRangeQuery(startDate, endDate, NodeCheckin)

		if ok {
			mainQuery = mainQuery.Must(rangeQuery)
		}

		agg := elastic.NewTermsAggregation().
			Field(fieldType).
			SubAggregation(aggregationTerm,
				elastic.NewFilterAggregation().Filter(mainQuery))

		searchSource = searchSource.Aggregation(fieldType, agg)
	}

	return searchSource
}
