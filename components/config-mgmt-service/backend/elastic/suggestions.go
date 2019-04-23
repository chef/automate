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
func (es Backend) GetSuggestions(term string, text string) ([]backend.Suggestion, error) {
	if backend.SuggestionFieldArray(term) {
		return es.getArrayAggSuggestions(term, text)
	}

	return es.getAggSuggestions(term, text)
}

func (es Backend) getAggSuggestions(term string, text string) ([]backend.Suggestion, error) {
	myagg := "myagg"
	typeQuery := elastic.NewTypeQuery(IndexNodeState)
	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(typeQuery)
	boolQuery = boolQuery.Must(elastic.NewTermsQuery("exists", "true"))
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

func (es Backend) getArrayAggSuggestions(term string, text string) ([]backend.Suggestion, error) {
	typeQuery := elastic.NewTypeQuery(IndexNodeState)
	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(typeQuery)
	boolQuery = boolQuery.Must(elastic.NewTermsQuery("exists", "true"))

	// return all unless text has at least 2 chars
	if len(text) >= 2 {
		matchQuery := elastic.NewMatchQuery(fmt.Sprintf("%s.engram", term), text)
		boolQuery = boolQuery.Must(matchQuery)
	}
	// multiplying the size by 10 as elasticsearch will sort array aggregations by doc_count. Will trim it back to size once we match it again in go
	aggs := elastic.NewTermsAggregation().Field(term).Size(SuggestionQuerySize)
	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation("myagg", aggs).
		Size(0)

	//// Sample search sent to ElasticSearch when suggesting roles:
	//{
	//	"query": {
	//		"bool": {
	//			"must": [
	//				{
	//					"type": {
	//						"value": "myagg"
	//					}
	//				},
	//				{
	//					"match": {
	//						"roles.engram": {
	//							"query": "base"
	//						}
	//					}
	//				}
	//			]
	//		}
	//	},
	//	"aggregations": {
	//		"myagg": {
	//			"terms": {
	//				"field": "roles",
	//				"size": 100
	//			}
	//		}
	//	},
	//	"size": 0
	//}
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
