package elastic

import (
	"context"
	"fmt"
	"strings"

	"github.com/olivere/elastic"
	"github.com/schollz/closestmatch"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/config-mgmt-service/backend"
)

// GetSuggestions - get a collection of suggestions
func (es Backend) GetSuggestions(term string, text string, filters map[string][]string) ([]backend.Suggestion, error) {
	if backend.SuggestionFieldArray(term) {
		return es.getArrayAggSuggestions(term, text, filters)
	}

	return es.getAggSuggestions(term, text, filters)
}

// {
// 	"aggregations":{
// 		 "myagg":{
// 				"aggregations":{
// 					 "distinct":{
// 							"top_hits":{
// 								 "_source":false,
// 								 "size":1
// 							}
// 					 },
// 					 "mymaxscore":{
// 							"max":{
// 								 "script":{
// 										"source":"_score"
// 								 }
// 							}
// 					 }
// 				},
// 				"terms":{
// 					 "field":"platform",
// 					 "order":[
// 							{
// 								 "mymaxscore":"desc"
// 							}
// 					 ],
// 					 "size":100
// 				}
// 		 }
// 	},
// 	"query":{
// 		 "bool":{
// 				"filter":{
// 					 "bool":{
// 							"should":{
// 								 "terms":{
// 										"exists":[
// 											 "true"
// 										]
// 								 }
// 							}
// 					 }
// 				},
// 				"must":[
// 					 {
// 							"type":{
// 								 "value":"node-state"
// 							}
// 					 },
// 					 {
// 							"match":{
// 								 "platform.engram":{
// 										"operator":"or",
// 										"query":"kk"
// 								 }
// 							}
// 					 },
// 					 {
// 							"match":{
// 								 "platform.engram":{
// 										"operator":"and",
// 										"query":"kk"
// 								 }
// 							}
// 					 }
// 				],
// 				"should":[
// 					 {
// 							"term":{
// 								 "platform.lower":{
// 										"boost":200,
// 										"value":"kk"
// 								 }
// 							}
// 					 },
// 					 {
// 							"prefix":{
// 								 "platform.lower":{
// 										"boost":100,
// 										"value":"kk"
// 								 }
// 							}
// 					 }
// 				]
// 		 }
// 	},
// 	"size":0
// }
func (es Backend) getAggSuggestions(term string, text string, filters map[string][]string) ([]backend.Suggestion, error) {
	myagg := "myagg"
	filters["exists"] = []string{"true"}
	boolQuery := newBoolQueryFromFilters(filters)
	typeQuery := elastic.NewTypeQuery(IndexNodeState)
	boolQuery = boolQuery.Must(typeQuery)
	lowerText := strings.ToLower(text)

	// return all unless text has at least 2 chars
	if len(text) >= 2 {
		// Any(or) of the text words can match
		matchQuery := elastic.NewMatchQuery(fmt.Sprintf("%s.engram", term), text).Operator("or")
		boolQuery = boolQuery.Must(matchQuery)
		// All(or) of the text words need to match to get a score boost from this condition
		matchQuery = elastic.NewMatchQuery(fmt.Sprintf("%s.engram", term), text).Operator("and")
		boolQuery = boolQuery.Must(matchQuery)
		// Give a score boost to values that equal the suggested text
		termQuery := elastic.NewTermQuery(fmt.Sprintf("%s.lower", term), lowerText).Boost(200)
		boolQuery = boolQuery.Should(termQuery)
		// Give a score boost to values that start with the suggested text
		prefixQuery := elastic.NewPrefixQuery(fmt.Sprintf("%s.lower", term), lowerText).Boost(100)
		boolQuery = boolQuery.Should(prefixQuery)
	}
	//aggs
	aggs := elastic.NewTermsAggregation().Field(term).Size(SuggestionQuerySize).Order("mymaxscore", false)
	distinctAgg := elastic.NewTopHitsAggregation().Size(1).FetchSource(false)
	aggs.SubAggregation("distinct", distinctAgg)
	scoreScript := elastic.NewScript("_score")
	maxScoreAgg := elastic.NewMaxAggregation().Script(scoreScript)
	aggs.SubAggregation("mymaxscore", maxScoreAgg)

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation(myagg, aggs).
		Size(0)

	searchResult, err := es.client.Search().
		SearchSource(searchSource).
		Index(IndexNodeState).
		FilterPath(
			"took",
			"hits.total",
			"aggregations.myagg.buckets.key",
			"aggregations.myagg.buckets.distinct.hits.hits._score",
			"aggregations.myagg.buckets.distinct.hits.hits._source").
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	aggResult, ok := searchResult.Aggregations.Terms(myagg)
	suggs := make([]backend.Suggestion, 0)
	if ok {
		for _, reportBucket := range aggResult.Buckets {
			distinct, _ := reportBucket.Aggregations.TopHits("distinct")
			for _, hit := range distinct.Hits.Hits {
				oneSugg := backend.Suggestion{Text: string(reportBucket.KeyNumber), Score: float32(*hit.Score)}
				suggs = append(suggs, oneSugg)
			}
		}
	}
	return suggs, nil
}

// {
// 	"aggregations":{
// 		 "myagg":{
// 				"aggregations":{
// 					 "typed_hits":{
// 							"top_hits":{
// 								 "_source":{
// 										"includes":[
// 											 "attributes"
// 										]
// 								 }
// 							}
// 					 }
// 				},
// 				"terms":{
// 					 "field":"attributes",
// 					 "include":".*yum.*",
// 					 "size":100
// 				}
// 		 }
// 	},
// 	"query":{
// 		 "bool":{
// 				"filter":{
// 					 "bool":{
// 							"should":{
// 								 "terms":{
// 										"exists":[
// 											 "true"
// 										]
// 								 }
// 							}
// 					 }
// 				},
// 				"must":[
// 					 {
// 							"type":{
// 								 "value":"node-state"
// 							}
// 					 },
// 					 {
// 							"match":{
// 								 "attributes.engram":{
// 										"query":"yum"
// 								 }
// 							}
// 					 }
// 				]
// 		 }
// 	},
// 	"size":0
// }
func (es Backend) getArrayAggSuggestions(term string, text string, filters map[string][]string) ([]backend.Suggestion, error) {
	typeQuery := elastic.NewTypeQuery(IndexNodeState)
	filters["exists"] = []string{"true"}
	boolQuery := newBoolQueryFromFilters(filters)

	boolQuery = boolQuery.Must(typeQuery)

	// return all unless text has at least 2 chars
	if len(text) >= 2 {
		matchQuery := elastic.NewMatchQuery(fmt.Sprintf("%s.engram", term), text)
		boolQuery = boolQuery.Must(matchQuery)
	}

	// "aggregations": {
	//   "myagg": {
	//     "terms": {
	//       "field": "recipes",
	//       "include": ".*yum-.*",
	//       "size": 100
	//     },
	//     "aggs": {
	//       "typed_hits": {
	//         "top_hits": {
	//           "_source": {
	//             "includes": [
	//               "recipes"
	//             ]
	//           }
	//         }
	//       }
	//     }
	//   }
	// }
	outerAggs := elastic.NewTermsAggregation().Field(term).Size(100)
	if len(text) >= 2 {
		outerAggs = outerAggs.Include(".*" + text + ".*")
	}

	//     "aggs": {
	//       "typed_hits": {
	//         "top_hits": {
	//           "_source": {
	//             "includes": [
	//               "recipes"
	//             ]
	//           }
	//         }
	//       }
	innerAggs := elastic.NewTopHitsAggregation().FetchSourceContext(
		elastic.NewFetchSourceContext(true).Include(term))
	outerAggs.SubAggregation("typed_hits", innerAggs)
	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation("myagg", outerAggs).
		Size(0)

	searchResult, err := es.client.Search().
		SearchSource(searchSource).
		Index(IndexNodeState).
		FilterPath(
			"took",
			"hits.total",
			"aggregations.myagg.buckets.key").
		Do(context.Background())

	if err != nil {
		return nil, err
	}

	log.WithFields(log.Fields{"millis": searchResult.TookInMillis}).Debug("Search query in milliseconds")

	aggResult, _ := searchResult.Aggregations.Terms("myagg")

	suggs := make([]string, 0)
	if aggResult != nil {
		for _, bucket := range aggResult.Buckets {
			// When elasticsearch find a match in an array it returns that whole array and includes other values in that array as buckets.
			// Because of this we will filter any buckets that do not contain any of the search string text
			bucketname := string(bucket.KeyNumber)
			if strings.Contains(bucketname, text) {
				suggs = append(suggs, bucketname)
			}
		}
	}

	// When matching array fields, elasticsearch returns the entire array with the score at the array level. We love arrays because they are small and work really well for filtering.
	// Once aggregated to get rid of duplicates, the final scoring is done on doc_count, which is irrelevant for suggestion type matching.
	// To workaround the scoring issue of the aggregated matches, we are using a go library to ngram suggest the best matches in the array.
	if len(text) >= 2 {
		// Choose a set of bag sizes, more is more accurate but slower
		bagSizes := []int{2, 3}
		// Create a closestmatch object
		cm := closestmatch.New(suggs, bagSizes)
		suggs = cm.ClosestN(text, SuggestionSize)
	}

	finalSuggs := make([]backend.Suggestion, 0)
	for i, sug := range suggs {
		oneSugg := backend.Suggestion{Text: sug, Score: float32(len(suggs) - i)}
		finalSuggs = append(finalSuggs, oneSugg)
	}

	return finalSuggs, nil
}
