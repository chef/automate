package mappings

// ComplianceRunInfo mapping used to create the `compliance-run-info index
var ComplianceRunInfo = Mapping{
	Index:      IndexNameComplianceRunInfo,
	Timeseries: false,
	Mapping: `
  {
    "index_patterns": ["` + IndexNameComplianceRunInfo + `"],
    "settings": {
      "index": {
        "refresh_interval": "1s",
        "number_of_shards": "5"
      }
    },
    "mappings": {
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
	`,
}
