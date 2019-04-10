package mappings

// Actions is the representation of our `actions` Mapping
var Actions = Mapping{
	Index:      "actions",
	Type:       "actions",
	Timeseries: true,
	Mapping: `
	{
		"template": "actions-*",
		"index_patterns": ["actions-*"],
		"settings": {
			"index": {
				"refresh_interval": "5s"
			}
		},
		"mappings": {
			"actions": {
				"dynamic": false,
				"properties": {
					"entity_name": {
						"type": "keyword",
						"ignore_above": 256
					},
					"entity_type": {
						"type": "keyword",
						"ignore_above": 256
					},
					"entity_uuid": {
						"type": "keyword",
						"ignore_above": 256
					},
					"id": {
						"type": "keyword",
						"ignore_above": 256
					},
					"organization_name": {
						"type": "keyword",
						"ignore_above": 256
					},
					"remote_hostname": {
						"type": "keyword",
						"ignore_above": 256
					},
					"requestor_name": {
						"type": "keyword",
						"ignore_above": 256
					},
					"run_id": {
						"type": "keyword",
						"ignore_above": 256
					},
					"service_hostname": {
						"type": "keyword",
						"ignore_above": 256
					},
					"source": {
						"type": "keyword",
						"ignore_above": 256
					},
					"task": {
						"type": "keyword",
						"ignore_above": 256
					},
					"user_agent": {
						"type": "keyword",
						"ignore_above": 256
					},
					"revision_id": {
						"type": "keyword",
						"ignore_above": 256
					},
					"data": {
						"type": "text",
						"index": "false"
					},
					"recorded_at": {
						"type": "date",
						"format": "strict_date_optional_time||epoch_millis"
					}
				}
			}
		}
	}
`,
}
