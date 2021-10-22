package mappings

// ConfigManagementRunInfo mapping used to create the `config-mgmt-run-info index
var ConfigManagementRunInfo = Mapping{
	Index:      IndexNameNodeRunInfo,
	Type:       DocType,
	Timeseries: false,
	Mapping: `
{
  "template": "` + IndexNameNodeRunInfo + `",
  "settings": {
    "index": {
      "refresh_interval": "1s"
    }
  },
  "mappings": {
    "` + DocType + `": {
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
    }
  }
}
	`,
}
