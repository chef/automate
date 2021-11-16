package mappings

import "fmt"

var runInfoProps = `
"properties": {
        "node_uuid": {
          "type": "keyword"
        },
        "first_run": {
          "type": "date"
        },
        "last_run": {
          "type": "date"
        }
      }
`

// NodeRunInfo mapping used to create the `config-mgmt-run-info index
var NodeRunInfo = Mapping{
	Index:      IndexNameNodeRunInfo,
	Type:       DocType,
	Timeseries: false,
	Properties: fmt.Sprintf(`{ %s }`, runInfoProps),
	Mapping: fmt.Sprintf(`
	{ 
		"template": "%s",
		"settings": {
			"index": {
				"refresh_interval": "1s" 
			} 
		}, 
		"mappings": {
			"%s": { 
				%s
			} 
		} 
	}`, IndexNameNodeRunInfo, DocType, runInfoProps),
}
