package api

func init() {
	Swagger.Add("stats", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/compliance/reporting/stats/stats.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/compliance/reporting/stats/failures": {
      "post": {
        "summary": "Read Failures",
        "description": "Returns the top failures for the specified object. A types filter is required for this api.\nSupported values are ` + "`" + `platform` + "`" + `, ` + "`" + `environment` + "`" + `, ` + "`" + `control` + "`" + `, and ` + "`" + `profile` + "`" + `.\nBy default, the top ten failed objects for the specified type are returned.\nSupports filtering and respects ` + "`" + `size` + "`" + ` parameter.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"filters\":[\n{\"type\":\"start_time\",\"values\":[\"2019-10-26T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-11-05T23:59:59Z\"]},\n{\"type\":\"types\",\"values\":[\"platform\",\"environment\"]}\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportFailures:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReadFailures",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Failures"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Query"
            }
          }
        ],
        "tags": [
          "StatsService"
        ]
      }
    },
    "/compliance/reporting/stats/profiles": {
      "post": {
        "summary": "Read Profiles",
        "description": "Returns statistics and summary information for profiles executed as part of the compliance reports. \nIf called without specifying a profile ID (` + "`" + `id` + "`" + `), the API will return stats on all the profiles.\nIf the ` + "`" + `id` + "`" + ` field is provided (profile ID) as part of the query object, the ` + "`" + `type` + "`" + ` field must also be specified. Options are ` + "`" + `controls` + "`" + ` or ` + "`" + `summary` + "`" + `.\nSupports filtering.\n\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"type\":\"controls\",\n\"id\":\"09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988\",\n\"filters\":[\n{\"type\":\"environment\",\"values\":[\"dev*\"]},\n{\"type\":\"start_time\",\"values\":[\"2019-10-26T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-11-05T23:59:59Z\"]}\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportProfiles:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReadProfiles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Profile"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Query"
            }
          }
        ],
        "tags": [
          "StatsService"
        ]
      }
    },
    "/compliance/reporting/stats/summary": {
      "post": {
        "summary": "Read Summary",
        "description": "Returns summary statistics for compliance reports. \nGeneral report summary information is the default. \nAdding a ` + "`" + `type` + "`" + ` value of ` + "`" + `nodes` + "`" + ` or ` + "`" + `controls` + "`" + ` will return summary statistics for that object.\nSupports filtering.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"type\":\"nodes\",\n\"filters\":[\n{\"type\":\"environment\",\"values\":[\"dev*\"]},\n{\"type\":\"start_time\",\"values\":[\"2019-10-26T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-11-05T23:59:59Z\"]}\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportSummary:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReadSummary",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Summary"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Query"
            }
          }
        ],
        "tags": [
          "StatsService"
        ]
      }
    },
    "/compliance/reporting/stats/trend": {
      "post": {
        "summary": "Read Trend",
        "description": "Returns trendgraph statistics for compliance reports. \nThe ` + "`" + `type` + "`" + ` field is required for this api call. Options are ` + "`" + `nodes` + "`" + ` or ` + "`" + `controls` + "`" + `.\nRequires minimum ` + "`" + `interval` + "`" + ` field of 3600 and defined start time and end time filters.\nSupports filtering.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"type\":\"nodes\",\n\"interval\":86400,\n\"filters\":[\n{\"type\":\"environment\",\"values\":[\"dev*\"]},\n{\"type\":\"start_time\",\"values\":[\"2019-10-26T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-11-05T23:59:59Z\"]}\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportTrend:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReadTrend",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Trends"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Query"
            }
          }
        ],
        "tags": [
          "StatsService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.compliance.reporting.stats.v1.ControlStats": {
      "type": "object",
      "properties": {
        "control": {
          "type": "string",
          "description": "Control ID."
        },
        "title": {
          "type": "string",
          "description": "Control title."
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "description": "Count of passed nodes that executed the control."
        },
        "failed": {
          "type": "integer",
          "format": "int32",
          "description": "Count of failed nodes that executed the control."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "Count of skipped nodes that executed the control."
        },
        "impact": {
          "type": "number",
          "format": "float",
          "description": "Impact of the control."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "Count of waived nodes that executed the control."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.ControlsSummary": {
      "type": "object",
      "properties": {
        "failures": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed controls in the reports."
        },
        "majors": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed controls with an impact between 0.4 and 0.7."
        },
        "minors": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed controls with an impact of 0.3 or less."
        },
        "criticals": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed controls with an impact of 0.7 or higher."
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of passed controls in the reports."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of skipped controls in the reports."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of waived controls in the reports."
        }
      },
      "description": "Statistics for the controls executed in the compliance reports."
    },
    "chef.automate.api.compliance.reporting.stats.v1.FailureSummary": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the object failing."
        },
        "failures": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of failures."
        },
        "id": {
          "type": "string",
          "description": "ID of the object, included if applicable."
        },
        "profile": {
          "type": "string",
          "description": "Not used."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.Failures": {
      "type": "object",
      "properties": {
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.FailureSummary"
          },
          "description": "Top failed profiles across the infrastructure."
        },
        "platforms": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.FailureSummary"
          },
          "description": "Top failed platforms across the infrastructure."
        },
        "controls": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.FailureSummary"
          },
          "description": "Top failed controls across the infrastructure."
        },
        "environments": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.FailureSummary"
          },
          "description": "Top failed environments across the infrastructure."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.ListFilter": {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The list of values to filter on for the given type. We 'OR' between these fields."
        },
        "type": {
          "type": "string",
          "description": "The field to filter on."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.NodeSummary": {
      "type": "object",
      "properties": {
        "compliant": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes that passed their compliance scans."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes that skipped their compliance scans."
        },
        "noncompliant": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes that failed their compliance scans."
        },
        "high_risk": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes that failed their compliance scan with one or more control of critical impact."
        },
        "medium_risk": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes that failed their compliance scan with one or more control of major impact."
        },
        "low_risk": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes that failed their compliance scan with one or more control of minor impact."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes with a waived compliance scan."
        }
      },
      "description": "Statistics about the nodes scanned in the compliance reports."
    },
    "chef.automate.api.compliance.reporting.stats.v1.Profile": {
      "type": "object",
      "properties": {
        "profile_list": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ProfileList"
          },
          "description": "Set of statistics about the profiles executed in the reports."
        },
        "profile_summary": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ProfileSummary",
          "description": "Intentionally blank."
        },
        "control_stats": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ControlStats"
          },
          "description": "Summary information about a specific profile's control results across the reports."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.ProfileList": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The profile name."
        },
        "id": {
          "type": "string",
          "description": "The profile SHA ID."
        },
        "failures": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of nodes that failed this profile."
        },
        "majors": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of failed nodes with major control failures that executed the profile."
        },
        "minors": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of failed nodes with minor control failures that executed the profile."
        },
        "criticals": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of failed nodes with critical control failures that executed the profile."
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of passed nodes that executed the profile."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of skipped nodes that executed the profile."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of waived nodes that executed the profile."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.ProfileSummary": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the profile."
        },
        "title": {
          "type": "string",
          "description": "Title of the profile."
        },
        "version": {
          "type": "string",
          "description": "Version of the profile."
        },
        "license": {
          "type": "string",
          "description": "License info for the profile."
        },
        "maintainer": {
          "type": "string",
          "description": "Maintainer for the profile."
        },
        "copyright": {
          "type": "string",
          "description": "Copyright info for the profile."
        },
        "copyright_email": {
          "type": "string",
          "description": "Copyright email info for the profile."
        },
        "summary": {
          "type": "string",
          "description": "Summary description of the profile."
        },
        "supports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Support"
          },
          "description": "Supports information for the profile (which os it can run on)."
        },
        "stats": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ProfileSummaryStats",
          "description": "Intentionally blank."
        },
        "depends": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Dependency"
          },
          "description": "Dependency information about the profile (which profiles it inherits)."
        }
      },
      "description": "Summary information about a specific profile's execution across the reports."
    },
    "chef.automate.api.compliance.reporting.stats.v1.ProfileSummaryStats": {
      "type": "object",
      "properties": {
        "failed": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of failed nodes that executed the profile."
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of passed nodes that executed the profile."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of skipped nodes that executed the profile."
        },
        "failed_nodes": {
          "type": "integer",
          "format": "int32",
          "description": "Not used."
        },
        "total_nodes": {
          "type": "integer",
          "format": "int32",
          "description": "Not used."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of waived controls for the given profile across nodes."
        }
      },
      "description": "Statistics about the nodes that executed the profile."
    },
    "chef.automate.api.compliance.reporting.stats.v1.Query": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique identifier, such as a profile ID."
        },
        "type": {
          "type": "string",
          "description": "Type of data being requested, used for ReadTrend and ReadSummary."
        },
        "size": {
          "type": "integer",
          "format": "int32",
          "description": "The number of results to return (used when pagination is not supported)."
        },
        "interval": {
          "type": "integer",
          "format": "int32",
          "description": "The interval to use for ReadTrend results, in integer seconds. Default of one hour, 3600."
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ListFilter"
          },
          "description": "Filters applied to the results."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Query.OrderType"
        },
        "sort": {
          "type": "string",
          "description": "Sort the list of results by a field."
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "description": "The offset for paginating requests. An offset defines a place in the results in order to fetch the next page of the results."
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "description": "The number of results on each paginated request page."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC",
      "description": "Sort the results in ascending or descending order."
    },
    "chef.automate.api.compliance.reporting.stats.v1.ReportSummary": {
      "type": "object",
      "properties": {
        "stats": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Stats",
          "description": "Intentionally blank."
        },
        "status": {
          "type": "string",
          "description": "Overall aggregated status for all the reports."
        },
        "duration": {
          "type": "number",
          "format": "double",
          "description": "Not used."
        },
        "start_date": {
          "type": "string",
          "description": "Not used."
        }
      },
      "description": "Statistics on the overall compliance reports."
    },
    "chef.automate.api.compliance.reporting.stats.v1.Stats": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "string",
          "format": "int64",
          "title": "Deprecated. int64 types render into string types when serialized to satisfy all browsers\nReplaced by the ` + "`" + `nodes_cnt` + "`" + ` field"
        },
        "platforms": {
          "type": "integer",
          "format": "int32",
          "description": "The number of unique node platforms in the reports."
        },
        "environments": {
          "type": "integer",
          "format": "int32",
          "description": "The number of unique environments in the reports."
        },
        "profiles": {
          "type": "integer",
          "format": "int32",
          "description": "The number of unique profiles in the reports."
        },
        "nodes_cnt": {
          "type": "integer",
          "format": "int32",
          "description": "The number of unique nodes scanned in the reports."
        },
        "controls": {
          "type": "integer",
          "format": "int32",
          "description": "The number of unique controls scanned in the reports."
        }
      },
      "description": "General statistics about the reports."
    },
    "chef.automate.api.compliance.reporting.stats.v1.Summary": {
      "type": "object",
      "properties": {
        "controls_summary": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ControlsSummary",
          "description": "Intentionally blank."
        },
        "node_summary": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.NodeSummary",
          "description": "Intentionally blank."
        },
        "report_summary": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.ReportSummary",
          "description": "Intentionally blank."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.Support": {
      "type": "object",
      "properties": {
        "os_name": {
          "type": "string",
          "description": "OS Name compatible with the profile. This is legacy InSpec syntax."
        },
        "os_family": {
          "type": "string",
          "description": "OS Family compatible with the profile. This is legacy InSpec syntax."
        },
        "release": {
          "type": "string",
          "description": "OS Release compatible with the profile."
        },
        "inspec_version": {
          "type": "string",
          "description": "InSpec Version compatible with the profile."
        },
        "platform_name": {
          "type": "string",
          "description": "Platform Name compatible with the profile."
        },
        "platform_family": {
          "type": "string",
          "description": "Platform Family compatible with the profile."
        },
        "platform": {
          "type": "string",
          "description": "Platform compatible with the profile."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.Trend": {
      "type": "object",
      "properties": {
        "report_time": {
          "type": "string",
          "description": "Time in point for which the passed/failed/skipped data is valid."
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "description": "Total passed objects (nodes or controls) on the reports at the given report time."
        },
        "failed": {
          "type": "integer",
          "format": "int32",
          "description": "Total failed objects (nodes or controls) on the reports at the given report time."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "Total skipped objects (nodes or controls) on the reports at the given report time."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "Total waived objects (nodes or controls) on the reports at the given report time."
        }
      }
    },
    "chef.automate.api.compliance.reporting.stats.v1.Trends": {
      "type": "object",
      "properties": {
        "trends": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.stats.v1.Trend"
          },
          "description": "Set of statistics for passed/failed/skipped nodes or controls in a trendgraph friendly data format."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Dependency": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The name of the profile."
        },
        "url": {
          "type": "string",
          "description": "The URL of the profile accessible over HTTP or HTTPS."
        },
        "path": {
          "type": "string",
          "description": "The path to the profile on disk."
        },
        "git": {
          "type": "string",
          "description": "The git URL of the profile."
        },
        "branch": {
          "type": "string",
          "description": "The specific git branch of the dependency."
        },
        "tag": {
          "type": "string",
          "description": "The specific git tag of the dependency."
        },
        "commit": {
          "type": "string",
          "description": "The specific git commit of the dependency."
        },
        "version": {
          "type": "string",
          "description": "The specific git version of the dependency."
        },
        "supermarket": {
          "type": "string",
          "description": "The name of the dependency stored in Chef Supermarket."
        },
        "github": {
          "type": "string",
          "description": "The short name of the dependency stored on Github."
        },
        "compliance": {
          "type": "string",
          "description": "The short name of the dependency stored on the Chef Automate or Chef Compliance server."
        },
        "status": {
          "type": "string",
          "description": "The status of the dependency in the report."
        },
        "skip_message": {
          "type": "string",
          "description": "The reason this profile was skipped in the generated report, if any."
        }
      }
    }
  }
}
`)
}
