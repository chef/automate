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
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"exists": {
		"type": "boolean",
		"null_value": false
	},
	"organization_name": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
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
	"chef_run_status": {
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
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"error_message": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"error_type": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"recipes": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"chef_tags": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"cookbooks": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"attributes": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"platform": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
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
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"uptime_seconds": {
		"type": "long"
	},
	"environment": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"roles": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
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
			},
		  "account_id": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			},
			"region": {
				"type": "keyword",
				"index": false,
				"doc_values": false
			}
		}
	},
	"cloud_id":{
		"type": "keyword"
	},
	"cloud_region": {
		"type": "keyword"
	},
	"cloud_account_id":{
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
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"policy_group": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"policy_name": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"policy_revision": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
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
	},
	"created": {
		"type": "date",
		"format": "strict_date_optional_time||epoch_millis"
	},
	"cloud_provider": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"timezone": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"kernel_release": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"kernel_version": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"virtualization_system": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"virtualization_role": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"dmi_system_manufacturer": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"dmi_system_serial_number": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"domain": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"hostname": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"macaddress": {
		"type": "keyword",
		"fields": {
			"engram" : {
				"type": "text",
				"analyzer": "autocomplete"
			},
			"lower": {
				"normalizer": "case_insensitive",
				"type": "keyword"
			}
		}
	},
	"memory_total": {
		"type": "keyword"
	},
	"ip6address": {
		"type": "ip"
	}
}`

// NodeState is the representation of our `node-state` Mapping
var NodeState = Mapping{
	Index:      "node-state-7",
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
