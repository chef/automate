//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package elastic

import (
	"context"
	"encoding/json"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"

	olivere "github.com/olivere/elastic/v7"

	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	wrapper "github.com/chef/automate/components/config-mgmt-service/backend/elastic/wrapper"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/lib/stringutils"
)

const (
	// Elasticsearch indexes
	IndexNodeState           = "node-state"
	IndexNodeAttribute       = "node-attribute"
	IndexConvergeHistoryBase = "converge-history-"
	IndexConvergeHistory     = "converge-history-*"

	// Elasticsearch fields we use within this package
	RunFieldTimestamp    = "start_time"
	RunFieldEndTimestamp = "end_time"
	ActionFieldTimestamp = "recorded_at"
	ActionFieldID        = "id"
	NodeFieldID          = "entity_uuid"
	RunFieldID           = "run_id"
	NodeCheckin          = "checkin"
	SuggestionSize       = 10
	SuggestionQuerySize  = 100
)

// Backend
//
// The elasticsearch backend model that implements
// the 'backend.Client' interface
type Backend struct {
	Url string `json:"url"`
	// client  *olivere.Client
	client2 wrapper.ElasticClient
}

// Initialize an Elastic instance
//
// This verifies the connectivity with Elasticsearch; if we couldn't
// connect, we do not start the service and print an error message
func New(url string) *Backend {
	es := Default()
	es.Url = url

	es.client2 = wrapper.NewElasticClient(url)
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
func (es Backend) newBoolQueryFromFilters(filters map[string][]string) wrapper.BoolQuery {
	boolQuery := es.client2.NewBoolQuery()
	for field, values := range filters {
		filterQuery := es.client2.NewBoolQuery()
		refinedValues := make([]string, 0, 0)
		if field == backend.Project {
			if stringutils.SliceContains(values, authzConstants.UnassignedProjectID) {
				emptyProjectQuery := es.client2.NewBoolQuery()
				emptyProjectQuery.MustNot(olivere.NewExistsQuery(field))
				filterQuery = filterQuery.Should(emptyProjectQuery)
			}

			refinedValues = stringutils.SliceFilter(values, func(projectId string) bool {
				return projectId != authzConstants.UnassignedProjectID
			})
		} else {
			for _, value := range values {
				// Determine if the filters contain any wildcards
				if strings.Contains(value, "*") || strings.Contains(value, "?") {
					wildQuery := olivere.NewWildcardQuery(field, value)
					filterQuery = filterQuery.Should(wildQuery)
				} else {
					refinedValues = append(refinedValues, value)
				}
			}
		}

		// Even if there is a wildcard value found, we still want to narrow down by any other values.
		// This would probably negate anything found with wildcards but using should brings back extra results
		if len(refinedValues) > 0 {
			termQuery := olivere.NewTermsQuery(field, stringArrayToInterfaceArray(refinedValues)...)
			filterQuery = filterQuery.Should(termQuery)
		}
		boolQuery = boolQuery.Filter(filterQuery)
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
func newRangeQuery(start string, end string, fieldTime string) (*olivere.RangeQuery, bool) {
	var ok = false

	rangeQuery := olivere.NewRangeQuery(fieldTime).
		Format("yyyy-MM-dd||yyyy-MM-dd-HH:mm:ss||yyyy-MM-dd'T'HH:mm:ssX||yyyy-MM-dd'T'HH:mm:ssXXX")

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

func newRangeQueryTime(startTime time.Time, endTime time.Time, fieldTime string) (*olivere.RangeQuery, bool) {

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

func (es Backend) getAggregationBucket(
	boolQuery wrapper.BoolQuery, indexName string, searchTerm string) ([]wrapper.AggregationBucketKeyItem, error) {
	var aggregationTerm = "counts"

	agg := olivere.NewFilterAggregation().
		Filter(boolQuery).
		SubAggregation(aggregationTerm,
			es.client2.NewTermsAggregation().Field(searchTerm).
				Size(1000)) // Set the maximum number of results returned. Without this line only 10 items will be returned

	searchSource := es.client2.NewSearchSource().
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
	searchResult, err := es.client2.Search().
		SearchSource(searchSource).
		Index(indexName).
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	if searchResult.TotalHits() == 0 {
		return []wrapper.AggregationBucketKeyItem{}, nil
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
	statusResult, found := searchResult.Aggregations().Terms(searchTerm)
	if !found {
		return nil, errors.NewBackendError("Aggregation term '%s' not found", searchTerm)
	}

	// Second aggregation `status_counts` (tag)
	statusCounts, found := statusResult.Terms(aggregationTerm)
	if !found {
		return nil, errors.NewBackendError("Aggregation term '%s' not found", aggregationTerm)
	}

	return statusCounts.Buckets(), nil
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
	mainQuery := es.newBoolQueryFromFilters(filters)

	fieldValueBuckets, err := es.getAggregationBucket(mainQuery, IndexNodeState, searchTerm)
	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	for _, bucket := range fieldValueBuckets {
		fieldValues = append(fieldValues, bucket.Key().(string))
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

func LogQueryPartMin(indices string, partToPrint interface{}, name string) {
	part, err := json.Marshal(partToPrint)
	if err != nil {
		log.Errorf("%s", err)
	}
	stringPart := string(part)
	if stringPart == "null" {
		stringPart = ""
	} else {
		stringPart = "\n" + stringPart
	}
	log.Debugf("\n------------------ %s-(start)--[%s]---------------%s \n------------------ %s-(end)-----------------------------------\n",
		name, indices, stringPart, name)
}
