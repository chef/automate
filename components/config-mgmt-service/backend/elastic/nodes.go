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
	ascending bool, filters map[string][]string) ([]backend.Node, error) {
	// Decrement one to the page since we must start from zero
	page = page - 1
	start := perPage * page

	// Adding the exists = true filter to the list of filters, because nodes
	// have documents that persist in elasticsearch to hold historical data
	// even after the node no longer exists
	filters["exists"] = []string{"true"}

	filtersQuery := newBoolQueryFromFilters(filters)

	searchResult, err := es.client.Search().
		Query(filtersQuery).
		Index(IndexNodeState).
		Sort(sortField, ascending). // sort by 'sortField', in 'ascending' [true, false]
		From(start).Size(perPage).  // take documents from {start} to {perPage}
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
func (es Backend) GetNodesCounts(filters map[string][]string) (backend.NodesCounts, error) {
	var ns = *new(backend.NodesCounts)

	// Adding the exists = true filter to the list of filters, because nodes
	// have documents that persist in elasticsearch to hold historical data
	// even after the node no longer exists
	filters["exists"] = []string{"true"}

	boolQuery := newBoolQueryFromFilters(filters)

	searchTerm := "status"

	statusNodesBuckets, err := es.getAggregationBucket(boolQuery, IndexNodeState, searchTerm)
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
