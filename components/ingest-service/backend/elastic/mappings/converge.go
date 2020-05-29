package mappings

// ConvergeHistory is the representation of our `converge-history` Mapping
var ConvergeHistory = Mapping{
	Index:      "converge-history",
	Type:       "converge",
	Timeseries: true,
	Mapping: `
	{
		"template": "converge-history-*",
		"index_patterns": ["converge-history-*"],
		"settings": {
			"index": {
				"refresh_interval": "5s"
			}
		},
		"mappings": {
			"converge": {
				"dynamic": false,
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
								"type": "keyword"
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
						"type": "keyword"
					},
					"organization_name": {
						"type": "keyword"
					},
					"resources": {
						"type": "object",
						"dynamic": true,
						"properties": {
							"name": {
								"type": "keyword"
							},
							"id": {
								"type": "keyword"
							},
							"duration": {
								"type": "integer"
							},
							"delta": {
								"type": "keyword",
								"index": false,
								"doc_values": false,
								"ignore_above": 256
							},
							"ignore_failure": {
								"type": "boolean"
							},
							"result": {
								"type": "keyword"
							},
							"status": {
								"type": "keyword"
							},
							"cookbook_name": {
								"type": "keyword",
								"index": false,
								"doc_values": false
							},
							"cookbook_type": {
								"type": "keyword",
								"index": false,
								"doc_values": false,
								"ignore_above": 256
							},
							"recipe_name": {
								"type": "keyword",
								"index": false,
								"doc_values": false,
								"ignore_above": 256
							},
							"conditional": {
								"type": "keyword",
								"index": false,
								"doc_values": false
							}
						}
					},
					"run_id": {
						"type": "keyword"
					},
					"run_list": {
						"type": "keyword"
					},
					"start_time": {
						"type": "date",
						"format": "strict_date_optional_time||epoch_millis"
					},
					"end_time": {
						"type": "date",
						"format": "strict_date_optional_time||epoch_millis"
					},
					"source": {
						"type": "keyword"
					},
					"status": {
						"type": "keyword"
					},
					"total_resource_count": {
						"type": "integer"
					},
					"updated_resource_count": {
						"type": "integer"
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
						"type": "keyword"
					},
					"event_action": {
						"type": "keyword"
					},
					"resource_names": {
						"type": "keyword"
					},
					"recipes": {
						"type": "keyword"
					},
					"chef_tags": {
						"type": "keyword"
					},
					"cookbooks": {
						"type": "keyword"
					},
					"platform": {
						"type": "keyword"
					},
					"platform_family": {
						"type": "keyword"
					},
					"platform_version": {
						"type": "keyword"
					},
					"chef_version": {
						"type": "keyword"
					},
					"uptime_seconds": {
						"type": "long"
					},
					"environment": {
						"type": "keyword"
					},
					"roles": {
						"type": "keyword"
					},
					"ec2": {
						"type": "object",
						"properties": {
							"instance_id": {
								"type": "keyword",
								"index": false,
								"doc_values": false,
								"ignore_above": 256
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
								"type": "keyword"
							},
							"region": {
								"type": "keyword"
							}
						}
					},
					"policy_group": {
						"type": "keyword"
					},
					"policy_name": {
						"type": "keyword"
					},
					"policy_revision": {
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
						"index": false,
						"doc_values": false,
						"ignore_above": 256
					},
					"versioned_cookbooks": {
						"type": "nested",
						"properties":{
							"name": { "type": "keyword", "ignore_above": 256 },
							"version": { "type": "keyword", "ignore_above": 256 }
						}
					},
					"cloud_provider": {
						"type": "keyword"
					},
					"timezone": {
						"type": "keyword"
					},
					"kernel_release": {
						"type": "keyword"
					},
					"kernel_version": {
						"type": "keyword"
					},
					"virtualization_system": {
						"type": "keyword"
					},
					"virtualization_role": {
						"type": "keyword"
					},
					"dmi_system_manufacturer": {
						"type": "keyword"
					},
					"dmi_system_serial_number": {
						"type": "keyword"
					},
					"domain": {
						"type": "keyword"
					},
					"hostname": {
						"type": "keyword"
					},
					"macaddress": {
						"type": "keyword"
					},
					"memory_total": {
						"type": "keyword"
					},
					"ip6address": {
						"type": "ip"
					}
				}
			}
		}
	}
	`,
}
