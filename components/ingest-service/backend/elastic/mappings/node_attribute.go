package mappings

import "fmt"

var attrProps = `
"properties": {
	"name": {
		"type": "text"
	},
	"entity_uuid": {
		"type": "keyword"
	},
	"chef_environment": {
		"type": "text"
	},
	"run_list": {
		"type": "keyword",
		"ignore_above": 256
	},
	"default": {
		"type": "text"
	},
	"normal": {
		"type": "text"
	},
	"override": {
		"type": "text"
	},
	"default_value_count": {
		"type": "long"
	},
	"normal_value_count": {
		"type": "long"
	},
	"override_value_count": {
		"type": "long"
	},
	"all_value_count": {
		"type": "long"
	},
	"last_update": {
		"type": "date"
	}
}`

// NodeAttribute is the representation of our `node-state` Mapping
var NodeAttribute = Mapping{
	Index:      "node-attribute",
	Type:       "node-attribute",
	Timeseries: false,
	Properties: fmt.Sprintf(`{ %s }`, attrProps),
	Mapping: fmt.Sprintf(`
	{
		"template": "node-attribute",
		"index_patterns": ["node-attribute"],
		"settings": {
			"index": {
				"refresh_interval": "5s"
			}
		},
		"mappings": {
			"node-attribute": {
				"dynamic": true,
				%s
			}
		}
	}
	`, attrProps),
}
