package persistence

import "fmt"

var AllMappings = []Mapping{
	Feeds,
}

const (
	feedsIndicesVersion = "2"
	DocType             = "_doc"
	IndexNameFeeds      = "eventfeed-" + feedsIndicesVersion + "-feeds"
)

// Mapping type is the representation of an ES mapping, it contains
// all the necessary fields you need to create a mapping and to insert
// documents to it
type Mapping struct {
	Index      string
	Alias      string
	Type       string
	Timeseries bool
	Mapping    string
	Properties string
}

var feedProps = `
	"properties":{
		"entity_uuid":{
			"type":"keyword"
		},
		"producer_id":{
				"type":"keyword"
		},
		"producer_name":{
			"type":"text"
		},
		"producer_object_type":{
			"type":"text"
		},
		"producer_tags":{
			"type":"keyword"
		},
		"feed_type":{
			"type":"text"
		},
		"event_type":{
			"fields": {
				"engram": {
					"analyzer": "autocomplete",
					"type": "text"
				},
				"lower": {
					"normalizer": "case_insensitive",
					"type": "keyword"
				}
			},
			"type":"keyword"
		},
		"tags":{
			"type":"keyword",
			"fields":{
				"keyword":{
						"type":"keyword",
						"ignore_above":256
				}
			}
		},
		"pub_timestamp":{
			"type":"date",
			"format":"strict_date_optional_time||epoch_millis"
		},
		"actor_id":{
			"type":"text"
		},
		"actor_name":{
			"fields": {
				"engram": {
					"analyzer": "autocomplete",
					"type": "text"
				},
				"lower": {
					"normalizer": "case_insensitive",
					"type": "keyword"
				}
			},
			"type":"keyword"
		},
		"actor_object_type":{
			"type":"text"
		},
		"verb":{
			"fields": {
				"engram": {
					"analyzer": "autocomplete",
					"type": "text"
				},
				"lower": {
					"normalizer": "case_insensitive",
					"type": "keyword"
				}
			},
			"type":"keyword"
		},
		"object_id":{
			"type":"text"
		},
		"object_name":{
			"type":"text"
		},
		"object_object_type":{
			"fields": {
				"engram": {
					"analyzer": "autocomplete",
					"type": "text"
				},
				"lower": {
					"normalizer": "case_insensitive",
					"type": "keyword"
				}
			},
			"type":"keyword"
		},
		"target_id":{
			"type":"text"
		},
		"target_name":{
			"type":"text"
		},
		"target_object_type":{
			"type":"text"
		},
		"created":{
			"type":"date",
			"format":"strict_date_optional_time||epoch_millis"
		}
	}
`

// Feeds mapping used to create the feeds-[version] index
var Feeds = Mapping{
	Index:      IndexNameFeeds,
	Type:       DocType,
	Timeseries: false,
	Properties: fmt.Sprintf(`{ %s }`, feedProps),
	Mapping: fmt.Sprintf(`
			{
				"template":"`+IndexNameFeeds+`",
				"settings":{
					"analysis": {
						"analyzer": {
							"autocomplete": {
								"filter": [
									"lowercase"
								],
								"tokenizer": "autocomplete_tokenizer"
							}
						},
						"tokenizer": {
							"autocomplete_tokenizer": {
								"max_gram": 20,
								"min_gram": 2,
								"token_chars": [
									"letter",
									"digit"
								],
								"type": "edge_ngram"
							}
						},
						"normalizer": {
							"case_insensitive": {
								"type": "custom",
								"char_filter": [],
								"filter": ["lowercase", "asciifolding"]
							}
						}
					},
					"index":{
							"refresh_interval":"1s"
					}
				},
				"mappings":{
					"`+DocType+`":{
						"dynamic": false,
						%s
					}
				}
			}
		`, feedProps),
}
