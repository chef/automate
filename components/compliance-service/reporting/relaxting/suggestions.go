package relaxting

import (
	"encoding/json"
	"fmt"
	"sort"

	"strings"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/compliance-service/utils"
	elastic "github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/schollz/closestmatch"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
)

// GetSuggestions - Report #12
func (backend ES2Backend) GetSuggestions(typeParam string, filters map[string][]string, text string, size32 int32) ([]*reportingapi.Suggestion, error) {
	client, err := backend.ES2Client()
	size := int(size32)
	if err != nil {
		logrus.Error("Cannot connect to ElasticSearch")
		return nil, err
	}

	var SUGGESTIONS_TYPES = map[string]string{
		"environment":  "environment",
		"platform":     "platform.name",
		"node":         "node_name",
		"role":         "roles",
		"recipe":       "recipes",
		"profile":      "profiles.title",
		"control":      "profiles.controls.title",
		"organization": "organization_name",
		"chef_server":  "source_fqdn",
		"chef_tags":    "chef_tags",
		"policy_group": "policy_group",
		"policy_name":  "policy_name",
	}

	target, ok := SUGGESTIONS_TYPES[typeParam]
	if !ok {
		return nil, utils.ProcessInvalid(nil, fmt.Sprintf("Invalid suggestion type '%s'", typeParam))
	}

	// Not filtering the type we are suggesting on. Otherwise, we would only get what we filter on
	switch typeParam {
	case "node":
		{
			filters["node_id"] = []string{}
			filters["node_name"] = []string{}
		}
	case "profile":
		filters["profile_id"] = []string{}
	default:
		filters[typeParam] = []string{}
	}

	suggs := make([]*reportingapi.Suggestion, 0)
	if typeParam == "profile" {
		suggs, err = backend.getProfileSuggestions(client, typeParam, target, text, size, filters)
	} else if typeParam == "control" {
		suggs, err = backend.getControlSuggestions(client, typeParam, target, text, size, filters)
	} else if typeParam == "recipe" || typeParam == "role" {
		suggs, err = backend.getArrayAggSuggestions(client, typeParam, target, text, size, filters)
	} else {
		suggs, err = backend.getAggSuggestions(client, typeParam, target, text, size, filters)
	}

	if err != nil {
		return nil, err
	}

	// sort suggestion desc by score and asc by text when suggestions have the same score
	sort.Slice(suggs, func(i, j int) bool {
		if suggs[i].Score == suggs[j].Score {
			switch strings.Compare(strings.ToLower(suggs[i].Text), strings.ToLower(suggs[j].Text)) {
			case -1:
				return true
			case 1:
				return false
			}
		}
		return suggs[i].Score > suggs[j].Score
	})
	logrus.Debugf("Score sorted suggestions: %+v", suggs)

	// return only the number of requested suggestions based on the size param
	if len(suggs) > size {
		return suggs[0:size], nil
	}
	return suggs, nil
}

func (backend ES2Backend) getAggSuggestions(client *elastic.Client, typeParam string, target string, text string, size int, filters map[string][]string) ([]*reportingapi.Suggestion, error) {
	//here, we base our decision on which index set (det or summary) to use for querying suggestions
	// the reason this is necessary is in the where a control has already been added to the query and therefore
	// the query needs to dive down to the control depth.. requiring the detailed indices.
	// in other words, only use summary here if there are no controls in the filter
	esIndex, err := GetEsIndex(filters, len(filters["control"]) == 0, true)
	if err != nil {
		return nil, errors.Wrap(err, "getAggSuggestions unable to get index dates")
	}

	boolQuery := backend.getFiltersQuery(filters, true)
	lowerText := strings.ToLower(text)

	if len(text) >= 2 {
		// Any(or) of the text words can match
		matchQuery := elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text).Operator("or")
		boolQuery = boolQuery.Must(matchQuery)
		// All(or) of the text words need to match to get a score boost from this condition
		matchQuery = elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text).Operator("and")
		boolQuery = boolQuery.Should(matchQuery)
		// Give a score boost to values that equal the suggested text
		termQuery := elastic.NewTermQuery(fmt.Sprintf("%s.lower", target), lowerText).Boost(100)
		boolQuery = boolQuery.Should(termQuery)
		// Give a score boost to values that start with the suggested text
		prefixQuery := elastic.NewPrefixQuery(fmt.Sprintf("%s.lower", target), lowerText).Boost(100)
		boolQuery = boolQuery.Should(prefixQuery)
	}
	// Create a term aggregation in order to group all the documents that have the same
	// value and order by score. This is important for suggestions accuracy
	aggs := elastic.NewTermsAggregation().Field(target).Size(size).Order("mymaxscore", false)
	scoreScript := elastic.NewScript("_score")
	maxScoreAgg := elastic.NewMaxAggregation().Script(scoreScript)
	aggs.SubAggregation("mymaxscore", maxScoreAgg)

	// For node suggestions, we need to return the node ID field as well. We are collecting that with a top_hits aggregation
	if target == "node_name" {
		fsc := elastic.NewFetchSourceContext(true).Include("node_uuid")
		topHatAgg := elastic.NewTopHitsAggregation().Size(1)
		topHatAgg.FetchSourceContext(fsc)
		aggs.SubAggregation("mytophit", topHatAgg)
	}

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation("myagg", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getAggSuggestions unable to complete search")
	}
	LogQueryPartMin(esIndex, source, "getAggSuggestions query")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"aggregations.myagg.buckets.key",
			"aggregations.myagg.buckets.mytophit.hits.hits._source",
			"aggregations.myagg.buckets.mymaxscore.value").
		Do(context.Background())

	if err != nil {
		logrus.Error("getAggSuggestions search failed")
		return nil, err
	}
	// Sample search sent to ElasticSearch for a node_name suggestion
	//{
	//	"aggregations": {
	//		"myagg": {

	LogQueryPartMin(esIndex, searchResult, "getAggSuggestions - searchResult")
	logrus.Debugf("Search query took %d milliseconds\n", searchResult.TookInMillis)
	aggResult, _ := searchResult.Aggregations.Terms("myagg")
	type SummarySource struct {
		NodeID string `json:"node_uuid"`
	}
	suggs := make([]*reportingapi.Suggestion, 0)
	if aggResult != nil {
		for _, myaggBucket := range aggResult.Buckets {
			mytophit, _ := myaggBucket.Aggregations.TopHits("mytophit")
			mymaxscore, _ := myaggBucket.Aggregations.Max("mymaxscore")
			if mymaxscore == nil {
				continue // protect the code below from trouble
			}
			oneSugg := reportingapi.Suggestion{Text: string(myaggBucket.KeyNumber), Score: float32(*mymaxscore.Value)}
			if mytophit != nil {
				for _, hit := range mytophit.Hits.Hits {
					if target == "node_name" {
						var p SummarySource
						if hit.Source != nil {
							err := json.Unmarshal(*hit.Source, &p)
							if err == nil {
								oneSugg.Id = p.NodeID
							}
						}
					}
				}
			}
			suggs = append(suggs, &oneSugg)
		}
	}
	return suggs, nil
}

func (backend ES2Backend) getArrayAggSuggestions(client *elastic.Client, typeParam string, target string, text string, size int, filters map[string][]string) ([]*reportingapi.Suggestion, error) {
	//here, we base our decision on which index set (det or summary) to use for querying suggestions
	// the reason this is necessary is in the where a control has already been added to the query and therefore
	// the query needs to dive down to the control depth.. requiring the detailed indices.
	// in other words, only use summary here if there are no controls in the filter
	esIndex, err := GetEsIndex(filters, len(filters["control"]) == 0, true)
	if err != nil {
		return nil, errors.Wrap(err, "getArrayAggSuggestions unable to get index dates")
	}

	boolQuery := backend.getFiltersQuery(filters, true)

	// We don't filter unless the text has at least 2 chars
	if len(text) >= 2 {
		matchQuery := elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text)
		boolQuery = boolQuery.Must(matchQuery)
	}
	// multiplying the size by 50 as elasticsearch will sort array aggregations by doc_count. Will trim it back to size once we match it again in go
	aggs := elastic.NewTermsAggregation().Field(target).Size(size * 50)
	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		Aggregation("myagg", aggs).
		Size(0)

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getArrayAggSuggestions unable to get Source")
	}
	LogQueryPartMin(esIndex, source, "getArrayAggSuggestions query")
	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"aggregations.myagg.buckets.key").
		Do(context.Background())

	if err != nil {
		logrus.Error("getArrayAggSuggestions search failed")
		return nil, err
	}

	LogQueryPartMin(esIndex, searchResult, "getArrayAggSuggestions - searchResult")
	logrus.Debugf("Search query took %d milliseconds\n", searchResult.TookInMillis)

	aggResult, _ := searchResult.Aggregations.Terms("myagg")
	suggs := make([]string, 0)
	if aggResult != nil {
		for _, bucket := range aggResult.Buckets {
			suggs = append(suggs, string(bucket.KeyNumber))
		}
	}
	//// Sample search sent to ElasticSearch when suggesting roles:
	//{
	//	"query": {
	//		"bool": {

	// When matching array fields, elasticsearch returns the entire array with the score at the array level. We love arrays because they are small and work really well for filtering.
	// Once aggregated to get rid of duplicates, the final scoring is done on doc_count, which is irrelevant for suggestion type matching.
	// To workaround the scoring issue of the aggregated matches, we are using a go library to ngram suggest the best matches in the array.
	if len(text) >= 2 {
		// Choose a set of bag sizes, more is more accurate but slower
		bagSizes := []int{2, 3}
		// Create a closestmatch object
		cm := closestmatch.New(suggs, bagSizes)
		suggs = cm.ClosestN(text, size)
	}

	finalSuggs := make([]*reportingapi.Suggestion, 0)
	for i, sug := range suggs {
		oneSugg := reportingapi.Suggestion{Text: sug, Score: float32(len(suggs) - i)}
		finalSuggs = append(finalSuggs, &oneSugg)
	}

	return finalSuggs, nil
}

func (backend ES2Backend) getProfileSuggestions(client *elastic.Client, typeParam string, target string, text string, size int, filters map[string][]string) ([]*reportingapi.Suggestion, error) {
	//the reason we may use summary index here is because we always throw away the current profile filter when
	// getting a suggestion for profile.. if we didn't then we'd only ever see the filter that's in our filter!
	esIndex, err := GetEsIndex(filters, len(filters["control"]) == 0, true)
	if err != nil {
		return nil, errors.Wrap(err, "getProfileSuggestions unable to get index dates")
	}

	boolQuery := backend.getFiltersQuery(filters, true)
	lowerText := strings.ToLower(text)

	var innerQuery elastic.Query
	if len(text) >= 2 {
		innerBoolQuery := elastic.NewBoolQuery()
		innerBoolQuery.Must(elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text).Operator("or"))
		innerBoolQuery.Should(elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text).Operator("and"))
		innerBoolQuery.Should(elastic.NewTermQuery(fmt.Sprintf("%s.lower", target), lowerText).Boost(100))
		innerBoolQuery.Should(elastic.NewPrefixQuery(fmt.Sprintf("%s.lower", target), lowerText).Boost(100))
		innerQuery = innerBoolQuery
	} else {
		innerQuery = elastic.NewExistsQuery(target)
	}

	hit := elastic.NewInnerHit().Size(size)
	boolQuery = boolQuery.Must(elastic.NewNestedQuery("profiles", innerQuery).InnerHit(hit))

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		FetchSource(false).
		Size(size * 50) // Multiplying size to ensure that same profile with multiple versions is not limiting our suggestions to a lower number
		// ^ Because we can't sort by max_score of the inner hits: https://discuss.elastic.co/t/nested-objects-hits-inner-hits-and-sorting/32565

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getProfileSuggestions unable to get Source")
	}

	LogQueryPartMin(esIndex, source, "getProfileSuggestions query")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._score",
			"hits.hits.inner_hits.profiles.hits.hits._source.sha256",
			"hits.hits.inner_hits.profiles.hits.hits._source.title",
			"hits.hits.inner_hits.profiles.hits.hits._source.version",
			"hits.hits.inner_hits.profiles.hits.hits._score").
		Do(context.Background())

	//// Sample search sent to ElasticSearch when suggesting controls:
	//{
	//	"_source": false,
	//...

	if err != nil {
		logrus.Error("getProfileSuggestions search failed")
		return nil, err
	}

	logrus.Debugf("Search query took %d milliseconds\n", searchResult.TookInMillis)

	type ProfileSource struct {
		Sha256  string `json:"sha256"`
		Title   string `json:"title"`
		Version string `json:"version"`
	}

	// Using this to avoid duplicate controls in the suggestions
	addedProfiles := make(map[string]bool)

	suggs := make([]*reportingapi.Suggestion, 0)
	if searchResult != nil {
		for _, hit := range searchResult.Hits.Hits {
			if hit != nil {
				for _, inner_hit := range hit.InnerHits {
					if inner_hit != nil {
						for _, hit2 := range inner_hit.Hits.Hits {
							var c ProfileSource
							if hit2.Source != nil {
								err := json.Unmarshal(*hit2.Source, &c)
								if err == nil {
									if !addedProfiles[c.Sha256] {
										oneSugg := reportingapi.Suggestion{Id: c.Sha256, Text: c.Title, Version: c.Version, Score: float32(*hit2.Score)}
										suggs = append(suggs, &oneSugg)
										addedProfiles[c.Sha256] = true
									}
								}
							}
						}
					}
				}
			}
		}
	}

	return suggs, nil
}

func (backend ES2Backend) getControlSuggestions(client *elastic.Client, typeParam string, target string, text string, size int, filters map[string][]string) ([]*reportingapi.Suggestion, error) {
	esIndex, err := GetEsIndex(filters, false, true)
	if err != nil {
		return nil, errors.Wrap(err, "getControlSuggestions unable to get index dates")
	}

	boolQuery := backend.getFiltersQuery(filters, true)

	var innerQuery elastic.Query
	if len(text) >= 2 {
		innerBoolQuery := elastic.NewBoolQuery()
		innerBoolQuery.Must(elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text).Operator("or"))
		innerBoolQuery.Should(elastic.NewMatchQuery(fmt.Sprintf("%s.engram", target), text).Operator("and"))
		innerBoolQuery.Should(elastic.NewTermQuery(target, text).Boost(100))
		innerBoolQuery.Should(elastic.NewPrefixQuery(target, text).Boost(100))

		innerQuery = innerBoolQuery
	} else {
		innerQuery = elastic.NewExistsQuery(target)
	}

	fsc := elastic.NewFetchSourceContext(true).
		Include("profiles.controls.id").
		Include("profiles.controls.title")
	hit := elastic.NewInnerHit().Size(size)
	hit.FetchSourceContext(fsc)

	boolQuery = boolQuery.Must(elastic.NewNestedQuery("profiles.controls", innerQuery).InnerHit(hit))

	searchSource := elastic.NewSearchSource().
		Query(boolQuery).
		FetchSource(false).
		Size(size * 50) // Multiplying size to ensure that same profile with multiple versions is not limiting our suggestions to a lower number
		// ^ Because we can't sort by max_score of the inner hits: https://discuss.elastic.co/t/nested-objects-hits-inner-hits-and-sorting/32565

	source, err := searchSource.Source()
	if err != nil {
		return nil, errors.Wrap(err, "getControlSuggestions unable to get Source")
	}

	LogQueryPartMin(esIndex, source, "getControlSuggestions query")

	searchResult, err := client.Search().
		SearchSource(searchSource).
		Index(esIndex).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._score",
			"hits.hits.inner_hits").
		Do(context.Background())

	// {code}

	if err != nil {
		logrus.Error("getControlSuggestions search failed")
		return nil, err
	}

	logrus.Debugf("Search query took %d milliseconds\n", searchResult.TookInMillis)

	type ControlSource struct {
		ID    string `json:"id"`
		Title string `json:"title"`
	}

	// Using this to avoid duplicate controls in the suggestions
	addedControls := make(map[string]bool)

	suggs := make([]*reportingapi.Suggestion, 0)
	if searchResult != nil {
		for _, hit := range searchResult.Hits.Hits {
			if hit != nil {
				for _, inner_hit := range hit.InnerHits {
					if inner_hit != nil {
						for _, hit2 := range inner_hit.Hits.Hits {
							var c ControlSource
							if hit2.Source != nil {
								err := json.Unmarshal(*hit2.Source, &c)
								if err == nil && c.ID != "" {
									if !addedControls[c.ID] {
										oneSugg := reportingapi.Suggestion{Id: c.ID, Text: c.Title, Score: float32(*hit2.Score)}
										suggs = append(suggs, &oneSugg)
										addedControls[c.ID] = true
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return suggs, nil
}
