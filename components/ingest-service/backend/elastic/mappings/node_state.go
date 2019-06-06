package mappings

import "fmt"

var nodeProps = `
"properties": {
	"entity_uuid": {
		"type": "keyword"
	},
	"timestamp": {
		"type": "date",
		"format": "strict_date_optional_time||epoch_millis"
	},
	"expanded_run_list": {
		"type": "object",
		"properties": {
			"id": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			},
			"run_list": {
				"type": "object",
				"properties": {
					"name": {
						"type": "keyword",
						"index": false,
						"doc_values": false
					},
					"skipped": {
						"type": "boolean"
					},
					"type": {
						"type": "keyword",
						"index": false,
						"doc_values": false,
						"ignore_above": 256
					}
				}
			}
		}
	},
	"node_name": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"exists": {
		"type": "boolean",
		"null_value": false
	},
	"organization_name": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"run_list": {
		"type": "keyword",
		"index": false,
		"doc_values": false
	},
	"projects": {
		"type": "keyword"
	},
	"status": {
		"type": "keyword"
	},
	"deprecations": {
		"type": "object",
		"dynamic": true,
		"properties": {
			"message": {
				"type": "text",
				"fielddata": false,
				"norms": false
			},
			"url": {
				"type": "keyword",
				"index": false,
				"doc_values": false,
				"ignore_above": 256
			},
			"location": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			}
		}
	},
	"error": {
		"type": "object",
		"properties": {
			"class": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			},
			"message": {
				"type": "text",
				"fielddata": false,
				"norms": false
			},
			"backtrace": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			},
			"description": {
				"type": "object",
				"dynamic": true
			}
		}
	},
	"tags": {
		"type": "keyword",
		"index": false,
		"doc_values": false,
		"ignore_above": 256
	},
	"resource_names": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"error_message": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"recipes": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"chef_tags": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"cookbooks": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"attributes": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"platform": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"platform_family": {
		"type": "keyword",
		"index": false,
		"doc_values": false,
		"ignore_above": 256
	},
	"platform_version": {
		"type": "keyword",
		"index": false,
		"doc_values": false,
		"ignore_above": 256
	},
	"chef_version": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"uptime_seconds": {
		"type": "long"
	},
	"environment": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"roles": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"ec2": {
		"type": "object",
		"properties": {
			"instance_id": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			},
			"instance_type": {
				"type": "keyword",
				"index": false,
				"doc_values": false,
				"ignore_above": 256
			},
			"placement_availability_zone": {
				"type": "keyword",
				"index": false,
				"doc_values": false,
				"ignore_above": 256
			},
			"public_ipv4": {
				"type": "ip"
			}
		}
	},
	"cloud_id":{
		"type": "keyword"
	},
	"fqdn": {
		"type": "keyword",
		"index": false,
		"doc_values": false,
		"ignore_above": 256
	},
	"ipaddress": {
		"type": "ip"
	},
	"source_fqdn": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"policy_group": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"policy_name": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"policy_revision": {
		"type": "keyword",
		"normalizer": "case_insensitive",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			}
		}
	},
	"checkin": {
		"type": "date"
	},
	"last_ccr_received": {
		"type": "date"
	},
	"liveness_managed": {
		"type": "boolean",
		"null_value": false
	},
	"latest_run_id": {
		"type": "keyword"
	},
	"versioned_cookbooks": {
		"type": "nested",
		"properties":{
			"name": { "type": "keyword", "ignore_above": 256 },
			"version": { "type": "keyword", "ignore_above": 256 }
		}
	},
	"has_deprecations": {
		"type": "boolean",
		"null_value": false
	},
	"deprecations_count": {
		"type": "integer",
		"null_value": 0
	}
}`

// NodeState is the representation of our `node-state` Mapping
var NodeState = Mapping{
	Index:      "node-state-6",
	Alias:      "node-state",
	Type:       "node-state",
	Timeseries: false,
	Properties: fmt.Sprintf(`{ %s }`, nodeProps),
	Mapping: fmt.Sprintf(`
	{
		"template": "node-state",
		"index_patterns": ["node-state"],
		"settings": {
			"index": {
				"refresh_interval": "5s"
			},
			 "analysis": {
				"normalizer": {
					"case_insensitive": {
						"type": "custom",
						"char_filter": [],
						"filter": ["lowercase", "asciifolding"]
					}
				},
				"analyzer": {
					"autocomplete": {
						"tokenizer": "autocomplete_tokenizer",
						"filter": [
							"lowercase"
						]
					}
				},
				"tokenizer": {
					"autocomplete_tokenizer": {
						"type": "edge_ngram",
						"min_gram": 2,
						"max_gram": 20,
						"token_chars": [
							"letter",
							"digit",
							"punctuation"
						]
					}
				}
			}
		},
		"mappings": {
			"node-state": {
				"dynamic": false,
				%s
			}
		}
	}
	`, nodeProps),
}
