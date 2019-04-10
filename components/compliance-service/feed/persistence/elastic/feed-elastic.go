//
//  Author:: Gina Peers <gpeers@chef.io>, Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package elastic

import (
	"context"
	"encoding/json"
	"math"
	"strconv"
	"strings"
	"time"

	olivere "github.com/olivere/elastic"
	"github.com/sirupsen/logrus"

	feedErrors "github.com/chef/automate/components/compliance-service/feed/errors"
	"github.com/chef/automate/components/compliance-service/feed/util"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

const (
	DefaultSortField        = "pub_timestamp"
	PublishedTimestampField = "pub_timestamp"
	DefaultAscending        = true
	FeedEntryID             = "entity_uuid"

	FeedConsumerNetworkIndexName    = "comp-1-feed-consumer-networks"
	FeedConsumerNetworkDocType      = "feed_consumer_network"
	NetworkProducerIdsField         = "producer_ids"
	FeedConsumerNetworkIndexMapping = `{
                        				"mappings" : {
                            				"feed_consumer_network" : {
                                				"properties" : {
												  "id" : 			{ "type" : "keyword" },
											      "producer_ids" : {
												    "type" : "text",
												    "fields" : {
												      "keyword" : {
													    "type" : "keyword",
													    "ignore_above" : 256
												      }
												    }
											      },
												  "producer" : {
													"type" : "nested",
													"properties" : {
													  "id" : {
														"type" : "text",
														"fields" : {
														  "keyword" : {
															"type" : "keyword",
															"ignore_above" : 256
														  }
														}
													  },
													  "name" : {
														"type" : "text",
														"fields" : {
														  "keyword" : {
															"type" : "keyword",
															"ignore_above" : 256
														  }
														}
													  },
													  "object_type" : {
														"type" : "text",
														"fields" : {
														  "keyword" : {
															"type" : "keyword",
															"ignore_above" : 256
														  }
														}
													  },
													  "p_tags" : {
														"type" : "nested",
														"properties" : {
														  "tag" : { "type" : "keyword" }
														}
													  }
													}
												  },
												  "created" :		{ "type" : "date" }
                                				}
                            				}
                        				}
										}`
)

//-----------  ELASTICSEARCH FINDER  -------------//

type Finder struct {
	UserID     string
	ProducerID []string
	FeedType   string
	Size       int
	Before     int64
	After      int64
	Start      int64
	End        int64
	Filters    []string
	Cursor     string
	sort       []string
	StartDate  string
	EndDate    string
}

func NewFinder() *Finder {
	return &Finder{}
}

// Sort specifies one or more sort orders.
// Use a dash (-) to make the sort order descending.
// Example: "name" or "-year".
func (f *Finder) Sort(sort ...string) *Finder {
	if f.sort == nil {
		f.sort = make([]string, 0)
	}
	f.sort = append(f.sort, sort...)
	return f
}

// Find feed consumer networks. Networks returned depend upon the filter criteria
// supplied in the Finder. When no criteria are supplied, all networks are returned
// UserID -> networks belonging to the specified user (user id matches network's feed consumer id)
func (f *Finder) FindNetworks(client *olivere.Client) ([]*util.FeedConsumerNetwork, error) {
	search := client.Search().Index(FeedConsumerNetworkIndexName)
	search = f.queryNetworks(search)
	//search = f.aggs(search)
	//search = f.sorting(search)
	//search = f.paginate(search)

	sr, err := search.Do(context.Background())
	if err != nil {
		return nil, err
	}

	var networks []*util.FeedConsumerNetwork

	if sr.Hits.TotalHits > 0 {
		networks, err = f.decodeNetworks(sr)
		if err != nil {

			return nil, err
		}
	}

	return networks, nil
}

// Sets up search service query which finds all entries or, given a user id and/or
// a feed type, returns a list of entries customized for this user (only feed entries
// from consumers this user cares about, only from the feed type specified). Note:
// currently, we only have the event feed type available, so the type check isn't yet
// implemented.
// TODO: implement feed type check (future)
func (f *Finder) queryEntries(service *olivere.SearchService) *olivere.SearchService {
	// return all feed entries, regardless of feed consumer network or feed type
	if f.UserID == "" && f.Filters == nil {
		service = service.Query(olivere.NewMatchAllQuery())
		return service
	}

	// only return feed entries from this user's network of producers
	if f.UserID != "" && f.Filters == nil {
		strBody := `{
				  "query": {
						"terms": {
						  "producer_id": {
							  "index": "` + FeedConsumerNetworkIndexName + `",
							  "type": "` + FeedConsumerNetworkDocType + `",
							  "id": "` + f.UserID + `",
							  "path": "` + NetworkProducerIdsField + `"
						  }
						}
				  }
				}`

		return service.Source(strBody)
	}

	filters, err := util.FormatFilters(f.Filters)
	if err != nil {
		logrus.Warnf("Couldn't format filters; error: %v", err)
		return nil
	}

	var mainQuery = newBoolQueryFromFilters(filters)

	rangeQuery, ok := newRangeQueryTime(util.MillisecondsToTime(f.Start), util.MillisecondsToTime(f.End), DefaultSortField)

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	return service.Query(mainQuery)
}

// Sets up search service query which finds all networks or, given a user id, finds the
// network(s) of feed entry producers this user (feed consumer) cares about.
func (f *Finder) queryNetworks(service *olivere.SearchService) *olivere.SearchService {
	// return all networks
	if f.UserID == "" {
		service = service.Query(olivere.NewMatchAllQuery())
		return service
	}

	q := olivere.NewBoolQuery()

	if f.UserID != "" {
		q = q.Must(olivere.NewTermQuery("id", f.UserID))
	}

	return service.Query(q)
}

// decodeFeedConsumerNetworks takes a search result and deserializes the feed consumer networks.
func (f *Finder) decodeNetworks(res *olivere.SearchResult) ([]*util.FeedConsumerNetwork, error) {
	if res == nil || res.TotalHits() == 0 {
		return nil, nil
	}

	var networks []*util.FeedConsumerNetwork
	for _, hit := range res.Hits.Hits {
		network := new(util.FeedConsumerNetwork)
		if err := json.Unmarshal(*hit.Source, network); err != nil {
			logrus.WithFields(logrus.Fields{"error": err, "object": hit.Source}).Error("Error unmarshalling the feed consumer network object")
			return nil, err
		}
		// TODO Add Score here, e.g.: entry.Score = *hit.Score
		networks = append(networks, network)
	}
	return networks, nil
}

// decodeFeedEntries takes a search result and deserializes the feed entries.
func (f *Finder) decodeFeedEntries(res *olivere.SearchResult) ([]*util.FeedEntry, int64, error) {
	if res == nil || res.TotalHits() == 0 {
		return nil, 0, nil
	}

	var entries []*util.FeedEntry
	for _, hit := range res.Hits.Hits {
		entry := new(util.FeedEntry)
		if err := json.Unmarshal(*hit.Source, entry); err != nil {
			logrus.WithFields(logrus.Fields{"error": err, "object": hit.Source}).Error("Error unmarshalling the feed entry object(s)")
			return nil, 0, err
		}
		logrus.Debugf("FeedEntry: %s", entry.ToString())
		// TODO Add Score here, e.g.: entry.Score = *hit.Score
		entries = append(entries, entry)
	}
	return entries, res.TotalHits(), nil
}

// By default, feed entries are sorted by published timestamp, most recent entries first.
func (f *Finder) sorting(service *olivere.SearchService) *olivere.SearchService {
	if len(f.sort) == 0 {
		// Sort by score by default
		//service = service.Sort("_score", false)
		// sort by published timestamp (ascending) by default
		service = service.Sort(DefaultSortField, DefaultAscending)
		return service
	}

	// Sort by fields; prefix of "-" means: descending sort order.
	for _, s := range f.sort {
		s = strings.TrimSpace(s)

		var field string
		var asc bool

		if strings.HasPrefix(s, "-") {
			field = s[1:]
			asc = false
		} else {
			field = s
			asc = true
		}

		// Maybe check for permitted fields to sort

		service = service.Sort(field, asc)
	}
	return service
}

//-----------  ELASTICSEARCH FEED STORE  -------------//

type ElasticFeedStore struct {
	Conn   *relaxting.ES2Backend
	client *olivere.Client
}

func (efs ElasticFeedStore) CreateFeedConsumerNetwork(network *util.FeedConsumerNetwork) (bool, error) {
	ctx := context.Background()
	client, err := efs.Conn.ES2Client()

	if err != nil {
		logrus.Warn("Could not create feed consumer network; connection to persistent store could not be obtained")
		return false, err
	}

	logrus.Debug("Checking for index...")
	exists, err := client.IndexExists(FeedConsumerNetworkIndexName).Do(ctx)

	if err != nil {
		logrus.Warn("Could not create feed consumer network; error determining whether or not Elasticsearch index exists")
		return false, err
	}

	// if the index doesn't exist, create a new one
	if !exists {
		logrus.Debug("Index does not exist... creating new index...")

		createIndex, err := client.CreateIndex(FeedConsumerNetworkIndexName).BodyString(FeedConsumerNetworkIndexMapping).Do(ctx)

		if err != nil {
			logrus.Warnf("Could not create feed consumer network; error creating Elasticsearch index: %s", err.Error())
			return false, err
		}

		if !createIndex.Acknowledged {
			logrus.Warn("Could not create feed consumer network; Elasticsearch create index operation not acknowledged")
			return false, err
		}
	}

	logrus.Debugf("Adding new document with id %s to index...", network.ID)
	// index feed entry (using JSON serialization)
	put1, err := client.Index().
		Index(FeedConsumerNetworkIndexName).
		Type(FeedConsumerNetworkDocType).
		Id(network.ID).
		BodyJson(network).
		Do(ctx)

	if err != nil {
		logrus.Warn("Could not create feed consumer network; Elasticsearch index create operation not acknowledged")
		return false, err
	}

	logrus.Debugf("Indexed feed consumer network %v to index %v, type %v ", put1.Id, put1.Index, put1.Type)

	return true, nil
}

func (efs ElasticFeedStore) CreateFeedEntry(entry *util.FeedEntry) (bool, error) {
	ctx := context.Background()
	client, err := efs.Conn.ES2Client()

	if err != nil {
		logrus.Warn("Could not create feed entry; connection to persistent store could not be obtained")
		return false, err
	}

	logrus.Debug("Checking for index...")
	exists, err := client.IndexExists(mappings.IndexNameFeeds).Do(ctx)

	if err != nil {
		logrus.Warn("Could not create feed entry; error determining whether or not Elasticsearch index exists")
		return false, err
	}

	// if the index doesn't exist, return an error
	if !exists {
		logrus.Warn("Could not create feed entry; Elasticsearch feeds index does not exist")
		return false, err
	}

	logrus.Debug("Adding new document to index...")
	// index feed entry (using JSON serialization)
	put1, err := client.Index().
		Index(mappings.IndexNameFeeds).
		Type(mappings.DocType).
		Id(entry.ID).
		BodyJson(entry).
		Do(ctx)

	if err != nil {
		logrus.Warn("Could not create feed entry; Elasticsearch index create operation not acknowledged")
		return false, err
	}

	logrus.Debugf("Indexed feed entry %s to index %s, type %s", put1.Id, put1.Index, put1.Type)

	return true, nil
}

func (efs ElasticFeedStore) CreateFeedEntries(entries []*util.FeedEntry) (bool, error) {
	return false, nil
}

func (efs ElasticFeedStore) GetFeedConsumerNetwork(query *util.FeedConsumerNetworkQuery) ([]*util.FeedConsumerNetwork, error) {
	client, err := efs.Conn.ES2Client()

	if err != nil {
		logrus.Warn("Could not get feed consumer network(s); connection to persistent store could not be obtained")
		return nil, err
	}

	f := NewFinder()
	f.UserID = query.UserID
	resp, err := f.FindNetworks(client)

	if err != nil {
		logrus.WithError(err).Warn("Could not get feed consumer network(s)")
		return nil, err
	}
	return resp, nil
}

func (efs ElasticFeedStore) GetFeed(query *util.FeedQuery) ([]*util.FeedEntry, int64, error) {
	c, err := efs.getElasticClient()
	if err != nil {
		logrus.Warn("Could not get feed; connection to persistent store could not be obtained")
		return nil, 0, err
	}

	exists, err := efs.indexExists(mappings.IndexNameFeeds)
	if err != nil {
		logrus.Warnf("Could not get feed; error: %+v", err)
		return nil, 0, err
	}

	var entries []*util.FeedEntry

	if exists {
		filters, err := util.FormatFilters(query.Filters)
		if err != nil {
			return nil, 0, err
		}

		var mainQuery = newBoolQueryFromFilters(filters)

		rangeQuery, ok := newRangeQueryTime(query.Start, query.End, PublishedTimestampField)

		logrus.WithFields(logrus.Fields{
			"start":       query.Start,
			"end":         query.End,
			"range query": rangeQuery,
		}).Info("range query string")

		if ok {
			mainQuery = mainQuery.Must(rangeQuery)
		}

		searchService := c.Search().
			Query(mainQuery).
			Index(mappings.IndexNameFeeds).
			Size(query.Size).
			Sort(PublishedTimestampField, query.Ascending).
			Sort(FeedEntryID, query.Ascending)

		if query.CursorDate.Unix() != 0 && query.CursorID != "" {
			milliseconds := query.CursorDate.UnixNano() / int64(time.Millisecond)
			searchService = searchService.SearchAfter(milliseconds, query.CursorID) // the date has to be in milliseconds
		}

		searchResult, err := searchService.Do(context.Background())

		// Return an error if the search was not successful
		if err != nil {
			logrus.WithFields(logrus.Fields{"error": err}).Error("Error retrieving feed entries") // only log non-404
			return nil, 0, err
		}

		for _, hit := range searchResult.Hits.Hits {
			entry := new(util.FeedEntry)
			if err := json.Unmarshal(*hit.Source, entry); err != nil {
				logrus.WithFields(logrus.Fields{"error": err, "object": hit.Source}).Error("Error unmarshalling the feed entry object(s)")
				return nil, 0, err
			}
			entries = append(entries, entry)
		}
		return entries, searchResult.Hits.TotalHits, nil
	} else {
		return entries, 0, nil
	}
}

func (efs ElasticFeedStore) GetFeedSummary(q *util.FeedSummaryQuery) (map[string]int64, error) {
	exists, err := efs.indexExists(mappings.IndexNameFeeds)
	if err != nil {
		logrus.WithError(err).Warn("Could not get feed summary; error determining whether or not Elasticsearch index exists")
		return nil, err
	}

	if exists {
		if q.CountsCategory == "entity_type" {
			q.CountsCategory = "object_object_type"
		} else if q.CountsCategory == "task" {
			q.CountsCategory = "verb"
		} else {
			logrus.Warnf("Could not get feed summary; unsupported counts category '%s'", q.CountsCategory)
			return nil, feedErrors.NewBackendError("Could not get feed summary; unsupported counts category '%s'", q.CountsCategory)
		}

		counts, err := efs.getCounts(q)
		if err != nil {
			logrus.WithError(err).Warn("Could not get feed summary; Elasticsearch query error.")
			return nil, err
		}

		return counts, nil
	}

	logrus.Info("No feed index found... no feed entries have been created yet. Returning empty result set.")
	return make(map[string]int64), nil
}

func (efs ElasticFeedStore) getCounts(q *util.FeedSummaryQuery) (map[string]int64, error) {
	// E.g.,
	// scanjobs: 57
	// profile:  66
	counts := map[string]int64{}

	filters, err := util.FormatFilters(q.Filters)
	if err != nil {
		return nil, err
	}

	mainQuery := newBoolQueryFromFilters(filters)
	rangeQuery, ok := newRangeQueryTime(q.Start, q.End, PublishedTimestampField)

	logrus.WithFields(logrus.Fields{
		"start":       q.Start,
		"end":         q.End,
		"range query": rangeQuery,
	}).Info("range query string")

	if ok {
		mainQuery = mainQuery.Must(rangeQuery)
	}

	buckets, err := efs.getAggregationBucket(mainQuery, mappings.IndexNameFeeds, q.CountsCategory)

	// Return an error if the search was not successful
	if err != nil {
		return nil, err
	}

	for _, bucket := range buckets {
		counts[bucket.Key.(string)] = bucket.DocCount
	}

	return counts, nil
}

func newBoolQueryFromFilters(filters map[string][]string) *olivere.BoolQuery {
	boolQuery := olivere.NewBoolQuery()
	field := "tags" // all filter terms are in the "tags" field

	for _, values := range filters {
		if len(values) > 0 {
			if len(values) > 1 {
				// if the same key is associated with multiple values, construct an OR query (terms query)
				termsQuery := olivere.NewTermsQuery(field, util.StringArrayToInterfaceArray(values)...)
				boolQuery = boolQuery.Must(termsQuery)
			} else {
				// if there's only one value, construct an AND query (one MUST with multiple values for "tags"), e.g.:
				// "must":[
				//    {"term":{"tags":"org_1"},
				//	  {"term":{"tags":"org_2"}
				//  ]
				value := values[0]
				boolQuery.Must(olivere.NewTermQuery(field, value))
			}
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
func newRangeQuery(start string, end string, fieldTime string) (*olivere.RangeQuery, bool) {
	var ok = false

	rangeQuery := olivere.NewRangeQuery(fieldTime).
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

func (efs ElasticFeedStore) getAggregationBucket(boolQuery *olivere.BoolQuery, indexName string, searchTerm string) ([]*olivere.AggregationBucketKeyItem, error) {
	c, err := efs.getElasticClient()
	if err != nil {
		logrus.Warn("Could not get feed summary counts; connection to persistent store could not be obtained")
		return nil, err
	}

	var aggregationTerm = "counts"

	agg := olivere.NewFilterAggregation().
		Filter(boolQuery).
		SubAggregation(aggregationTerm,
			olivere.NewTermsAggregation().Field(searchTerm).
				Size(1000)) // Set the maximum number of results returned. Without this line only 10 items will be returned

	searchSource := olivere.NewSearchSource().
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
	searchResult, err := c.Search().
		SearchSource(searchSource).
		Index(indexName).
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		logrus.WithFields(logrus.Fields{"error": err}).Errorf("Error searching for aggregation")
		return nil, err
	}

	if searchResult.TotalHits() == 0 {
		return []*olivere.AggregationBucketKeyItem{}, nil
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
		return nil, feedErrors.NewBackendError("Aggregation term '%s' not found", searchTerm)
	}

	// Second aggregation `status_counts` (tag)
	statusCounts, found := statusResult.Aggregations.Terms(aggregationTerm)
	if !found {
		return nil, feedErrors.NewBackendError("Aggregation term '%s' not found", aggregationTerm)
	}

	return statusCounts.Buckets, nil
}

func (efs *ElasticFeedStore) getElasticClient() (*olivere.Client, error) {
	if efs.client == nil {
		c, err := efs.Conn.ES2Client()
		if err != nil {
			logrus.Warnf("Could not initialize Elasticsearch client; connection to persistent store could not be obtained: %v", err)
			return nil, err
		}
		efs.client = c
	}
	return efs.client, nil
}

func (efs *ElasticFeedStore) indexExists(name string) (bool, error) {
	c, err := efs.getElasticClient()
	if err != nil {
		logrus.Warnf("Error determining whether or not Elasticsearch index %s exists: %+v", name, err)
		return false, err
	}
	// if feed index doesn't exist yet, return false
	exists, err := c.IndexExists(name).Do(context.Background())
	if err != nil {
		logrus.Warnf("Error determining whether or not Elasticsearch index %s exists: %+v", name, err)
		return false, err
	}
	if !exists {
		logrus.Infof("No index %s found... no feed entries have been created yet.", name)
		return false, nil
	}
	return true, nil
}

// GetActionLine event_type filters
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
// interval - 24 must be divisible by this number
// For example 1, 2, 3, 4, 6, 8, 12, and 24 are valid. Where
// 5, 7, 9, 10, 11, and 13 are not valid values.
func (efs ElasticFeedStore) GetActionLine(filters []string, startDate string, endDate string, timezone string, interval int, action string) (*util.ActionLine, error) {
	c, err := efs.getElasticClient()
	if err != nil {
		logrus.Warn("Could not get feed timeline; connection to persistent store could not be obtained")
		return nil, err
	}

	exists, err := efs.indexExists(mappings.IndexNameFeeds)
	if err != nil {
		logrus.Warnf("Could not get feed; error: %+v", err)
		return &util.ActionLine{}, err
	}
	if !exists {
		logrus.Warnf("Could not get feed timeline; index %s does not exist", mappings.IndexNameFeeds)
		return &util.ActionLine{}, feedErrors.NewBackendError("Could not get feed timeline; index %s does not exist", mappings.IndexNameFeeds)
	}

	formattedFilters, err := util.FormatFilters(filters)
	if err != nil {
		return &util.ActionLine{}, err
	}

	var (
		bucketSize        = strconv.Itoa(interval) + "h"
		eventTypeItems    = "items"
		dateHistoTag      = "dateHisto"
		timeFormatRFC3339 = "yyyy-MM-dd'T'HH:mm:ssZ"
		mainQuery         = newBoolQueryFromFilters(formattedFilters)
	)

	loc, err := time.LoadLocation(timezone)
	if err != nil {
		return &util.ActionLine{}, err
	}

	startTime, err := efs.createStartTime(startDate, loc)
	if err != nil {
		return &util.ActionLine{}, err
	}

	endTime, err := efs.createEndTime(endDate, loc)
	if err != nil {
		return &util.ActionLine{}, err
	}

	mainQuery = mainQuery.Must(olivere.NewTermQuery("verb", action))

	rangeQuery, _ := newRangeQuery(
		startTime.Format(time.RFC3339),
		endTime.Format(time.RFC3339),
		PublishedTimestampField)
	mainQuery = mainQuery.Must(rangeQuery)

	bucketHist := olivere.NewDateHistogramAggregation().Field(PublishedTimestampField).
		Interval(bucketSize).
		MinDocCount(0). // needed to return empty buckets
		ExtendedBounds(
			startTime.Format(time.RFC3339), endTime.Format(time.RFC3339)). // needed to return empty buckets
		Format(timeFormatRFC3339).
		TimeZone(timezone). // needed start the buckets at the beginning of the day.
		SubAggregation(eventTypeItems,
			olivere.NewTermsAggregation().Field("object_object_type"))

	ss := olivere.NewSearchSource().Query(mainQuery).Aggregation(dateHistoTag, bucketHist)
	src, err := ss.Source()
	if err != nil {
		logrus.Warnf("Can't get feed timeline; error obtaining search source for action %s: %+v", action, err)
		return &util.ActionLine{}, err
	}

	data, _ := json.Marshal(src)
	logrus.Debugf("Feed timeline for action %s query string is: %s", action, string(data))

	// Use ss in search
	searchResult, err := c.Search().SearchSource(ss).Index(mappings.IndexNameFeeds).Do(context.Background())

	if err != nil {
		return &util.ActionLine{}, err
	}

	dateHistoRes, found := searchResult.Aggregations.DateHistogram(dateHistoTag)

	// When no feed entries are in Elasticsearch 'found' is false.
	// Here we are creating the empty buckets to return
	if !found {
		duration := endTime.Sub(startTime)
		numberOfBuckets := int(math.Ceil(duration.Hours())) / interval

		return &util.ActionLine{
			Action:    action,
			Timeslots: make([]util.Timeslot, numberOfBuckets),
		}, nil
	}

	timeslots := make([]util.Timeslot, len(dateHistoRes.Buckets))
	for index, bucket := range dateHistoRes.Buckets {
		item, found := bucket.Aggregations.Terms(eventTypeItems)
		if !found {
			return &util.ActionLine{}, feedErrors.NewBackendError("Item '%s' not found", eventTypeItems)
		}

		if len(item.Buckets) > 0 {
			timeslots[index].EntryCounts = make([]util.EntryCount, len(item.Buckets))
			for c, buc := range item.Buckets {
				timeslots[index].EntryCounts[c].Category = buc.Key.(string)
				timeslots[index].EntryCounts[c].Count = buc.DocCount
			}
		}
	}

	return &util.ActionLine{
		Action:    action,
		Timeslots: timeslots,
	}, nil
}

func (efs ElasticFeedStore) createStartTime(startDate string, loc *time.Location) (time.Time, error) {
	startTime, err := time.ParseInLocation("2006-01-02", startDate, loc)
	if err != nil {
		return startTime, err
	}

	// At the beginning of the day
	return time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location()), nil
}

func (efs ElasticFeedStore) createEndTime(endDate string, loc *time.Location) (time.Time, error) {
	endTime, err := time.ParseInLocation("2006-01-02", endDate, loc)
	if err != nil {
		return endTime, err
	}

	// At the end of the day
	return time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		23, 59, 59, 0, endTime.Location()), nil
}
