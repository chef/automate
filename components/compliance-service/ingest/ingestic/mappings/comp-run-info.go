package mappings

// ComplianceRunInfo mapping used to create the `compliance-run-info index
var ComplianceRunInfo = Mapping{
	Index:      IndexNameComplianceRunInfo,
	Type:       DocType,
	Timeseries: false,
	Mapping: `
{
  "template": "` + IndexNameComplianceRunInfo + `",
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
