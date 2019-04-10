package mappings

// ComplianceFeeds mapping used to create the comp-[version]-feeds index
var ComplianceFeeds = Mapping{
	Index:      IndexNameFeeds,
	Type:       DocType,
	Timeseries: false,
	Mapping: `
		{
		  "template" : "` + IndexNameFeeds + `",
		  "settings": {
			"index": {
			  "refresh_interval": "1s"
			}
		  },
		  "mappings": {
			"` + DocType + `": {
                                "properties" : {
                                    "entity_uuid" : 					{ "type" : "keyword" },
									"producer_id" :						{ "type" : "keyword" },
									"producer_name" :					{ "type" : "text" },
									"producer_object_type" :			{ "type" : "text" },
									"producer_tags" : 					{ "type" : "keyword" },
									"feed_type" : 						{ "type" : "text" },
									"event_type" : 						{ "type" : "keyword" },
									"tags" : 							{
																			"type" : "keyword",
																			"fields" : {
																				"keyword" : {
																				"type" : "keyword",
																				"ignore_above" : 256
																				}
																			}
																		},
									"pub_timestamp" : 					{ "type" : "date", "format" : "strict_date_optional_time||epoch_millis" },
									"actor_id" : 						{ "type" : "text" },
									"actor_name" : 						{ "type" : "text" },
									"actor_object_type" : 				{ "type" : "text" },
									"verb" : 							{ "type" : "keyword" },
									"object_id" : 						{ "type" : "text" },
									"object_name" : 					{ "type" : "text" },
									"object_object_type" : 				{ "type" : "keyword" },
									"target_id" : 						{ "type" : "text" },
									"target_name" : 					{ "type" : "text" },
									"target_object_type" : 				{ "type" : "text" },
									"created" :							{ "type" : "date", "format" : "strict_date_optional_time||epoch_millis" }
                                }
                            }
			}
		}`,
}
