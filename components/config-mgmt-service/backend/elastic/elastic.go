//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/olivere/elastic"

	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/lib/stringutils"
)

const (
	// Elasticsearch indexes
	IndexNodeState           = "node-state"
	IndexNodeAttribute       = "node-attribute"
	IndexConvergeHistoryBase = "converge-history-"
	IndexConvergeHistory     = "converge-history-*"
	IndexAction              = "actions-*"

	// Elasticsearch fields we use within this package
	RunFieldTimestamp    = "start_time"
	CheckinTimestamp     = "checkin"
	ActionFieldTimestamp = "recorded_at"
	ActionFieldID        = "id"
	NodeFieldID          = "entity_uuid"
	SuggestionSize       = 10
	SuggestionQuerySize  = 100
)

// Backend
//
// The elasticsearch backend model that implements
// the 'backend.Client' interface
type Backend struct {
	Url    string `json:"url"`
	client *elastic.Client
}

// Initialize an Elastic instance
//
// This verifies the connectivity with Elasticsearch; if we couldn't
// connect, we do not start the service and print an error message
func New(url string) *Backend {
	es := Default()
	es.Url = url

	// Create a new elastic Client
	esClient, err := elastic.NewClient(
		// In the future we will need to create a custom http.Client to pass headers.
		// => (ent? user? token?)
		//elastic.SetHttpClient(httpClient),
		elastic.SetURL(es.Url),
		elastic.SetSniff(false),
	)

	if err != nil {
		fmt.Printf("Could not create elasticsearch client from '%s': %s\n", es.Url, err)
		os.Exit(1)
	}

	es.client = esClient
	return &es
}

func Default() Backend {
	return Backend{Url: "http://localhost:9200"}
}

// newBoolQueryFromFilters Creates an `elastic.BoolQuery` that injects Term Queries from
// the provided filters.
//
// Example of the generated elasticsearch query:
//   "filter":{
//       "bool":{
//          "must":[
//             {
//                "term":{
//                   "environment": ["hola", "adios"]
//                }
//             },
//          ]
//       }
//    }
func newBoolQueryFromFilters(filters map[string][]string) *elastic.BoolQuery {
	boolQuery := elastic.NewBoolQuery()
	for field, values := range filters {
		if field == backend.Project {
			projectsQuery := elastic.NewBoolQuery()
			if stringutils.SliceContains(values, authzConstants.UnassignedProjectID) {
				emptyProjectQuery := elastic.NewBoolQuery()
				emptyProjectQuery.MustNot(elastic.NewExistsQuery(field))
				projectsQuery.Should(emptyProjectQuery)
			}

			assignedProjectIds := stringutils.SliceFilter(values, func(projectId string) bool {
				return projectId != authzConstants.UnassignedProjectID
			})

			if len(assignedProjectIds) > 0 {
				projectMatchQuery := elastic.NewTermsQuery(field, stringArrayToInterfaceArray(assignedProjectIds)...)
				projectsQuery.Should(projectMatchQuery)
			}
			boolQuery = boolQuery.Filter(projectsQuery)
			continue
		}
		// We don't know how many values will end up here
		// Decided appending values might be faster than removing them.
		refinedValues := make([]string, 0, 0)
		for _, value := range values {
			// Determine if the filters contain any wildcards
			if strings.Contains(value, "*") || strings.Contains(value, "?") {
				wildQuery := elastic.NewWildcardQuery(field, value)
				boolQuery = boolQuery.Must(wildQuery)
			} else {
				refinedValues = append(refinedValues, value)
			}
		}
		// Even if there is a wildcard value found, we still want to narrow down by any other values.
		// This would probably negate anything found with wildcards but using should brings back extra results
		if len(refinedValues) > 0 {
			termQuery := elastic.NewTermsQuery(field, stringArrayToInterfaceArray(refinedValues)...)
			boolQuery = boolQuery.Must(termQuery)
		}
	}
	return boolQuery
}

// newRangeQuery Creates an `elastic.RangeQuery` from
// the provided start time and end time.
//
// Example of the generated elasticsearch query:
// {
//     "query": {
//         "range" : {
//             "start_time" : {
//                 "gte": "2017-09-01-07:58:06", // start
//                 "lte": "2017-09-01-07:58:08", // end
//                 "format": "yyyy-MM-dd||yyyy-MM-dd-HH:mm:ss"
//             }
//         }
//     }
// }
func newRangeQuery(start string, end string, fieldTime string) (*elastic.RangeQuery, bool) {
	var ok = false

	rangeQuery := elastic.NewRangeQuery(fieldTime).
		Format("yyyy-MM-dd||yyyy-MM-dd-HH:mm:ss||yyyy-MM-dd'T'HH:mm:ssZ")

	if start != "" {
		ok = true
		rangeQuery = rangeQuery.Gte(start)
	}
	if end != "" {
		ok = true
		rangeQuery = rangeQuery.Lte(end)
	}
	return rangeQuery, ok
}

func newRangeQueryTime(startTime time.Time, endTime time.Time, fieldTime string) (*elastic.RangeQuery, bool) {

	var start, end string
	if !startTime.IsZero() {
		start = startTime.Format(time.RFC3339)
	}

	if !endTime.IsZero() {
		end = endTime.Format(time.RFC3339)
	}

	return newRangeQuery(start, end, fieldTime)
}

// stringArrayToInterfaceArray Converts an array of strings into an array of interfaces
func stringArrayToInterfaceArray(array []string) []interface{} {
	interfaceArray := make([]interface{}, len(array))
	for i, v := range array {
		interfaceArray[i] = v
	}
	return interfaceArray
}

func (es Backend) getAggregationBucket(boolQuery *elastic.BoolQuery, indexName string, searchTerm string) ([]*elastic.AggregationBucketKeyItem, error) {
	var aggregationTerm = "counts"

	agg := elastic.NewFilterAggregation().
		Filter(boolQuery).
		SubAggregation(aggregationTerm,
			elastic.NewTermsAggregation().Field(searchTerm).
				Size(1000)) // Set the maximum number of results returned. Without this line only 10 items will be returned

	searchSource := elastic.NewSearchSource().
		Aggregation(searchTerm, agg)

	// Search Elasticsearch body
	// {
	// 	"aggregations":{
	// 		 "organization_name":{
	// 				"aggregations":{
	// 					 "counts":{
	// 							"terms":{
	// 								 "field":"[searchTerm]",
	// 								 "size":1000
	// 							}
	// 					 }
	// 				},
	// 				"filter":{
	// 					 "bool":{
	// 							"must":{
	// 								 "terms":{
	// 										"exists":[
	// 											 "true"
	// 										]
	// 								 }
	// 							}
	// 					 }
	// 				}
	// 		 }
	// 	}
	// }
	searchResult, err := es.client.Search().
		SearchSource(searchSource).
		Index(indexName).
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	if searchResult.TotalHits() == 0 {
		return []*elastic.AggregationBucketKeyItem{}, nil
	}

	// Now that we have executed the Search(), we need to extract the buckets
	// from the aggregations, but we need to go two levels deep:
	//
	// {
	//   "_shards": {...},
	//   "hits": {...},
	//   "aggregations": {
	//       "status": {                 <- First aggs
	//           "doc_count": X,
	//           "status_counts": {      <- Second aggs
	//               "buckets": [...]
	//           }
	//       }
	//   }
	// }
	// First aggregation `searchTerm`
	statusResult, found := searchResult.Aggregations.Terms(searchTerm)
	if !found {
		return nil, errors.NewBackendError("Aggregation term '%s' not found", searchTerm)
	}

	// Second aggregation `status_counts` (tag)
	statusCounts, found := statusResult.Aggregations.Terms(aggregationTerm)
	if !found {
		return nil, errors.NewBackendError("Aggregation term '%s' not found", aggregationTerm)
	}

	return statusCounts.Buckets, nil
}

// GetListForField - collect a set of unique field values
//
// Elasticsearch query example, with "organization_name":
// {
// 	"query":
// 		{ "bool":
// 			{"filter":
// 				{"term":
// 					{"exists": "true"}
// 				}
// 			}
// 		},
// 	"aggs":
// 		{"grouping":
// 			{"terms":
// 				{"field": "organization_name"}
// 			}
// 		}
// 	}
func (es Backend) GetListForField(searchTerm string, filters map[string][]string) ([]string, error) {
	fieldValues := make([]string, 0)

	filters["exists"] = []string{"true"}
	mainQuery := newBoolQueryFromFilters(filters)

	fieldValueBuckets, err := es.getAggregationBucket(mainQuery, IndexNodeState, searchTerm)
	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	for _, bucket := range fieldValueBuckets {
		fieldValues = append(fieldValues, bucket.Key.(string))
	}

	return fieldValues, nil
}

// EmptyStringIfNil asserts an interface as a string, and if that fails it returns empty string
func EmptyStringIfNil(attribute interface{}) string {
	if v, ok := attribute.(string); ok {
		return v
	} // captures the nil case, too
	return ""
}
