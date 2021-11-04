package api

func init() {
	Swagger.Add("reporting", `{
  "swagger": "2.0",
  "info": {
    "title": "external/compliance/reporting/reporting.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/compliance/reporting/controls": {
      "post": {
        "summary": "List Controls",
        "description": "Lists controls from the last run, with optional filtering.\nSupports filtering, but not pagination or sorting.\nLimited to 100 results by default.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:controlItems:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ListControlItems",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlItems"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlItemRequest"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/nodes/id/{id}": {
      "get": {
        "summary": "Show Node by ID",
        "description": "Show a specific node by ID.\nSupports filtering by profile or control.\nDoes not support pagination or sorting.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportNodes:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ReadNode",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Node"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "Unique identifier.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/nodes/search": {
      "post": {
        "summary": "List Nodes",
        "description": "List all nodes, with optional filtering, pagination, and sorting.\nMax return payload size is 4MB, use pagination to fetch remaining data.\n\n| Sort parameter | Sort value |\n| --- | --- |\n| environment | environment.lower |\n| latest_report.controls.failed.critical | controls_sums.failed.critical |\n| latest_report.controls.failed.total | controls_sums.failed.total |\n| latest_report.end_time (default) | end_time |\n| latest_report.status | status |\n| name | node_name.lower |\n| platform | platform.full |\n| status | status |\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"filters\":[\n{\"type\":\"environment\",\"values\":[\"dev*\"]},\n{\"type\":\"start_time\",\"values\":[\"2019-10-26T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-11-05T23:59:59Z\"]}\n],\n\"page\":1,\"per_page\":100,\n\"sort\":\"environment\",\"order\":\"ASC\"\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportNodes:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ListNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Nodes"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/profiles": {
      "post": {
        "summary": "List Profiles",
        "description": "List all profiles in use, with optional filtering.\nSupports pagination, filtering, and sorting.\nValid sort fields: name, title\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportProfiles:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ListProfiles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ProfileMins"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/report-ids": {
      "post": {
        "summary": "List Report IDs",
        "description": "List all IDs for the latest report for each node, with optional filtering.\nSupports filtering, but not pagination or sorting.\nIncluding more than one value for ` + "`" + `profile_id` + "`" + `, or ` + "`" + `profile_name` + "`" + ` is not allowed.\nIncluding values for both ` + "`" + `profile_id` + "`" + ` and ` + "`" + `profile_name` + "`" + ` in one request is not allowed.\nMax return payload size is 4MB.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportids:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ListReportIds",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ReportIds"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/reports": {
      "post": {
        "summary": "List Reports",
        "description": "Makes a list of reports. Adding a filter makes a list of all node reports that meet the filter criteria.\nSupports pagination, filtering, and sorting.\nMax return payload size is 4MB, use pagination to fetch remaining data.\n\nValid sort fields: latest_report.controls.failed.critical, latest_report.controls.failed.total, latest_report.end_time, latest_report.status, node_name\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\"filters\":\n[\n{\"type\":\"start_time\",\"values\":[\"2019-09-09T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-09-11T23:59:59Z\"]}\n],\n\"page\":1, \"per_page\": 3,\n\"sort\": \"latest_report.status\", \"order\": \"ASC\"\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reports:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ListReports",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ReportsSummaryLevelOne"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/reports/id/{id}": {
      "post": {
        "summary": "Show Report by ID",
        "description": "Show a specific report by ID. Supports filtering, but not pagination or sorting.\nIncluding more than one value for ` + "`" + `profile_id` + "`" + `, or ` + "`" + `profile_name` + "`" + ` is not allowed.\nIncluding values for both ` + "`" + `profile_id` + "`" + ` and ` + "`" + `profile_name` + "`" + ` in one request is not allowed.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reports:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ReadReport",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Report"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "Unique identifier.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/suggestions": {
      "post": {
        "summary": "List Reporting Suggestions",
        "description": "Get suggestions for compliance reporting resources based on matching text substrings.\nSupports filtering, but not pagination or sorting.\n` + "`" + `type` + "`" + ` parameter is required. It must be one of the parameters from the following table.\n\n| Suggestion type parameter | Suggestion type value |\n| --- | --- |\n| chef_server | source_fqdn |\n| chef_tags | chef_tags |\n| control | profiles.controls.title |\n| control_tag_key | profiles.controls.string_tags.key |\n| control_tag_value | profiles.controls.string_tags.values |\n| environment | environment |\n| inspec_version | version |\n| node | node_name |\n| organization | organization_name |\n| platform | platform.name |\n| platform_with_version | platform.full |\n| policy_group | policy_group |\n| policy_name | policy_name |\n| profile | profiles.title |\n| profile_with_version | profiles.full |\n| recipe | recipes |\n| role | roles |\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"type\":\"environment\",\n\"text\":\"aws*\",\n\"filters\":[\n{\"type\":\"start_time\",\"values\":[\"2019-10-26T00:00:00Z\"]},\n{\"type\":\"end_time\",\"values\":[\"2019-11-05T23:59:59Z\"]}\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncompliance:reportSuggestions:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ReportingService_ListSuggestions",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Suggestions"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.SuggestionRequest"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/api/v0/compliance/reporting/version": {
      "get": {
        "operationId": "ReportingService_GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.common.version.VersionInfo"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "hidden"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.common.ExportData": {
      "type": "object",
      "properties": {
        "content": {
          "type": "string",
          "format": "byte",
          "description": "Exported reports in JSON or CSV."
        }
      }
    },
    "chef.automate.api.common.version.VersionInfo": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "sha": {
          "type": "string"
        },
        "built": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Attribute": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The name of the attribute."
        },
        "options": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Option",
          "description": "The options defined for the attribute."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Control": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "The unique ID of this control."
        },
        "code": {
          "type": "string",
          "description": "The full ruby code of the control defined in the profile."
        },
        "desc": {
          "type": "string",
          "description": "The full description of the control."
        },
        "impact": {
          "type": "number",
          "format": "float",
          "description": "The severity of the control."
        },
        "title": {
          "type": "string",
          "description": "The compact description of the control."
        },
        "source_location": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.SourceLocation",
          "description": "Intentionally blank."
        },
        "results": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Result"
          },
          "description": "The results of running all tests defined in the control against the node."
        },
        "refs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Ref"
          },
          "description": "External supporting documents for the control."
        },
        "tags": {
          "type": "string",
          "description": "Metadata defined on the control in key-value format."
        },
        "waived_str": {
          "type": "string",
          "title": "Indicates if the control has been waived or not"
        },
        "waiver_data": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.OrigWaiverData",
          "title": "Additional details for waived controls"
        },
        "removed_results_counts": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.RemovedResultsCounts",
          "description": "When the control results are removed to reduce the size of the report, this summarize the status of the trimmed results."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ControlItem": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "The control's unique ID."
        },
        "title": {
          "type": "string",
          "description": "The control's compact description."
        },
        "profile": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ProfileMin",
          "description": "Intentionally blank."
        },
        "impact": {
          "type": "number",
          "format": "float",
          "description": "The severity of the control."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "The time the report using the control was submitted at."
        },
        "control_summary": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlSummary",
          "description": "Intentionally blank."
        },
        "waivers": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.WaiverData"
          },
          "description": "A list of waivers for the nodes affected by this control."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ControlItemRequest": {
      "type": "object",
      "properties": {
        "text": {
          "type": "string",
          "description": "The term to use to match resources on."
        },
        "size": {
          "type": "integer",
          "format": "int32",
          "description": "The maximum number of controls to return (Default 100)."
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ListFilter"
          },
          "description": "The criteria used to filter the controls returned."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ControlItems": {
      "type": "object",
      "properties": {
        "control_items": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlItem"
          },
          "description": "The paginated results of controls matching the filters."
        },
        "control_summary_totals": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlSummary",
          "title": "The summary totals for this list of control items"
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ControlSummary": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of controls in the report."
        },
        "passed": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Total",
          "description": "Intentionally blank."
        },
        "skipped": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Total",
          "description": "Intentionally blank."
        },
        "failed": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Failed",
          "description": "Intentionally blank."
        },
        "waived": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Total",
          "description": "Intentionally blank."
        }
      },
      "description": "A minimal representation of the statuses of the controls in the report."
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
    },
    "chef.automate.api.compliance.reporting.v1.Failed": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed controls."
        },
        "minor": {
          "type": "integer",
          "format": "int32",
          "description": "The number of failed controls with minor severity."
        },
        "major": {
          "type": "integer",
          "format": "int32",
          "description": "The number of failed controls with major severity."
        },
        "critical": {
          "type": "integer",
          "format": "int32",
          "description": "The number of failed controls with critical severity."
        }
      },
      "description": "Stats of failed controls."
    },
    "chef.automate.api.compliance.reporting.v1.Group": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "The name of the file the controls are defined in."
        },
        "title": {
          "type": "string",
          "description": "The title of control group."
        },
        "controls": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The ids of the controls defined in this file."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "description": "The key of the tag."
        },
        "value": {
          "type": "string",
          "description": "The value of the tag."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.LatestReportSummary": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "The latest report ID."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "The time the report was submitted at."
        },
        "status": {
          "type": "string",
          "description": "The status of the run the report was made from."
        },
        "controls": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlSummary",
          "description": "Intentionally blank."
        }
      },
      "description": "A summary of the latest report for this node."
    },
    "chef.automate.api.compliance.reporting.v1.ListFilter": {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Filters applied to the list."
        },
        "type": {
          "type": "string",
          "description": "The field to filter on."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Node": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "The node ID."
        },
        "name": {
          "type": "string",
          "description": "The name assigned to the node."
        },
        "platform": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Platform",
          "description": "Intentionally blank."
        },
        "environment": {
          "type": "string",
          "description": "The environment assigned to the node."
        },
        "latest_report": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.LatestReportSummary",
          "description": "A summary of the information contained in the latest report for this node."
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Kv"
          },
          "description": "The tags assigned to this node."
        },
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ProfileMeta"
          },
          "description": "A minimal representation of the compliance profiles run against the node."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Nodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Node"
          },
          "description": "The nodes matching the request filters."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes matching the filters."
        },
        "total_passed": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of passing nodes matching the filters."
        },
        "total_failed": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed nodes matching the filters."
        },
        "total_skipped": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of skipped nodes matching the filters."
        },
        "total_waived": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of waived nodes matching the filters."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Option": {
      "type": "object",
      "properties": {
        "description": {
          "type": "string",
          "description": "The description of the attribute."
        },
        "default": {
          "type": "string",
          "description": "The default value of the attribute."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.OrigWaiverData": {
      "type": "object",
      "properties": {
        "expiration_date": {
          "type": "string"
        },
        "justification": {
          "type": "string"
        },
        "run": {
          "type": "boolean",
          "format": "boolean"
        },
        "skipped_due_to_waiver": {
          "type": "boolean",
          "format": "boolean"
        },
        "message": {
          "type": "string"
        }
      },
      "title": "OrigWaiverData as it originally came from the InSpec report\nWill supplement this with waived_str to make consumption easier"
    },
    "chef.automate.api.compliance.reporting.v1.Platform": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The name of the node's operating system."
        },
        "release": {
          "type": "string",
          "description": "The version of the node's operating system."
        },
        "full": {
          "type": "string",
          "description": "The combined name and version of the node's operating system."
        }
      },
      "description": "The name and version of the node's operating system."
    },
    "chef.automate.api.compliance.reporting.v1.Profile": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The name of the profile. Must be unique."
        },
        "title": {
          "type": "string",
          "description": "The profile title."
        },
        "maintainer": {
          "type": "string",
          "description": "The maintainer listed in the profile metadata."
        },
        "copyright": {
          "type": "string",
          "description": "The name of the copyright holder."
        },
        "copyright_email": {
          "type": "string",
          "description": "The contact information for the copyright holder."
        },
        "license": {
          "type": "string",
          "description": "The license the profile is released under."
        },
        "summary": {
          "type": "string",
          "description": "A short description of the profile."
        },
        "version": {
          "type": "string",
          "description": "The version of the profile."
        },
        "owner": {
          "type": "string",
          "description": "The name of the account that uploaded the profile to Automate."
        },
        "full": {
          "type": "string",
          "description": "The combined name and version of the profile."
        },
        "supports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Support"
          },
          "description": "The supported platform targets."
        },
        "depends": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Dependency"
          },
          "description": "Other profiles that this profile depends on."
        },
        "sha256": {
          "type": "string",
          "description": "A unique value generated from the profile used to identify it."
        },
        "groups": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Group"
          },
          "description": "The groups of controls defined in the profile."
        },
        "controls": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Control"
          },
          "description": "The controls defined on the profile."
        },
        "attributes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Attribute"
          },
          "description": "The attributes defined on the profile."
        },
        "latest_version": {
          "type": "string",
          "description": "The highest version number of the profile stored in Automate."
        },
        "status": {
          "type": "string",
          "description": "The status of the profile in the generated report."
        },
        "skip_message": {
          "type": "string",
          "description": "The reason this profile was skipped in the generated report, if any."
        },
        "status_message": {
          "type": "string",
          "description": "A message to detail the reason why a profile is skipped or failed in the generated report."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ProfileCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of nodes matching the filters."
        },
        "failed": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of failed nodes matching the filters."
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of skipped nodes matching the filters."
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of passing nodes matching the filters."
        },
        "waived": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of waived nodes matching the filters."
        }
      },
      "description": "Stats on the statuses of nodes matching the filters."
    },
    "chef.automate.api.compliance.reporting.v1.ProfileMeta": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The name of the profile."
        },
        "version": {
          "type": "string",
          "description": "The profile version."
        },
        "id": {
          "type": "string",
          "description": "The profile unique ID."
        },
        "status": {
          "type": "string",
          "description": "The status of the profile run against the node."
        },
        "full": {
          "type": "string",
          "description": "The combined name and version of the profile."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ProfileMin": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "The name of the profile."
        },
        "title": {
          "type": "string",
          "description": "The profile title."
        },
        "id": {
          "type": "string",
          "description": "The profile ID."
        },
        "version": {
          "type": "string",
          "description": "The profile version."
        },
        "status": {
          "type": "string",
          "description": "The aggregated status of the profile across the nodes it has been run on."
        }
      },
      "description": "Minimal representation of a profile."
    },
    "chef.automate.api.compliance.reporting.v1.ProfileMins": {
      "type": "object",
      "properties": {
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ProfileMin"
          },
          "description": "Minimal representations of the profiles matching the filters."
        },
        "counts": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ProfileCounts",
          "description": "Intentionally blank."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Query": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique identifier."
        },
        "type": {
          "type": "string",
          "description": "File type, either JSON or CSV."
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ListFilter"
          },
          "description": "Filters applied to the report results."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Query.OrderType"
        },
        "sort": {
          "type": "string",
          "description": "Sort the list of results by a field."
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "description": "The offset for paginating requests. An offset defines a place in the results in order to show the next page of the results."
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "description": "The number of results on each paginated request page."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC",
      "description": "Sort the results in ascending or descending order."
    },
    "chef.automate.api.compliance.reporting.v1.Ref": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string",
          "description": "The external document URL."
        },
        "ref": {
          "type": "string",
          "description": "A description of the external document."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.RemovedResultsCounts": {
      "type": "object",
      "properties": {
        "failed": {
          "type": "integer",
          "format": "int32",
          "title": "The number of results with status of ` + "`" + `failed` + "`" + ` that have been trimmed (removed) from a control"
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "title": "The number of results with status of ` + "`" + `skipped` + "`" + ` that have been trimmed (removed) from a control"
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "title": "The number of results with status of ` + "`" + `passed` + "`" + ` that have been trimmed (removed) from a control"
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Report": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "A unique report identifier."
        },
        "node_id": {
          "type": "string",
          "description": "The reporting node's unique ID."
        },
        "node_name": {
          "type": "string",
          "description": "The reporting node name."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "The time that the report was completed."
        },
        "status": {
          "type": "string",
          "description": "The status of the run the report was made from."
        },
        "controls": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlSummary",
          "description": "Intentionally blank."
        },
        "environment": {
          "type": "string",
          "description": "The environment of the node making the report."
        },
        "version": {
          "type": "string",
          "description": "The version of the report."
        },
        "platform": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Platform",
          "description": "Intentionally blank."
        },
        "statistics": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Statistics",
          "description": "Intentionally blank."
        },
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Profile"
          },
          "description": "The profiles run as part of this report."
        },
        "job_id": {
          "type": "string",
          "description": "The compliance scan job ID associated with the report."
        },
        "ipaddress": {
          "type": "string",
          "description": "The reporting node IP address."
        },
        "fqdn": {
          "type": "string",
          "description": "The FQDN (fully qualified domain name) of the node making the report."
        },
        "chef_server": {
          "type": "string",
          "description": "The Chef Infra Server that manages the node making the report."
        },
        "chef_organization": {
          "type": "string",
          "description": "The Organization the node belongs to."
        },
        "roles": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The Roles associated with the node."
        },
        "chef_tags": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The Chef Tags associated with the node."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The projects the node is assigned to."
        },
        "status_message": {
          "type": "string",
          "description": "The status message of the report."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ReportData": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "The report run_uuid"
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "title": "The time in UTC that the scan was completed"
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ReportIds": {
      "type": "object",
      "properties": {
        "ids": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The list of unique report identifiers found matching the query."
        },
        "report_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ReportData"
          },
          "description": "The list of unique report identifiers with their respective end_time, found matching the query."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ReportSummaryLevelOne": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "A unique report identifier."
        },
        "node_id": {
          "type": "string",
          "description": "The reporting node's unique ID."
        },
        "node_name": {
          "type": "string",
          "description": "The reporting node name."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "The time that the report was completed."
        },
        "status": {
          "type": "string",
          "description": "The status of the run the report was made from."
        },
        "controls": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlSummary",
          "description": "Intentionally blank."
        },
        "ipaddress": {
          "type": "string",
          "description": "The reporting node IP address."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Reports": {
      "type": "object",
      "properties": {
        "reports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Report"
          },
          "description": "Paginated results of reports matching the filters."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of reports matching the filters."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.ReportsSummaryLevelOne": {
      "type": "object",
      "properties": {
        "reports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ReportSummaryLevelOne"
          },
          "description": "Paginated results of summary level reports matching the filters."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of reports matching the filters."
        }
      },
      "title": "ReportsSummaryLevelOne used for ListReports call"
    },
    "chef.automate.api.compliance.reporting.v1.Result": {
      "type": "object",
      "properties": {
        "status": {
          "type": "string",
          "description": "The status of the test."
        },
        "code_desc": {
          "type": "string",
          "description": "The description of the test."
        },
        "run_time": {
          "type": "number",
          "format": "float",
          "description": "The time taken to run the test."
        },
        "start_time": {
          "type": "string",
          "description": "The timestamp of when this individual test was run."
        },
        "message": {
          "type": "string",
          "description": "The reason the test failed, if any."
        },
        "skip_message": {
          "type": "string",
          "description": "The reason the test was skipped, if any."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.SourceLocation": {
      "type": "object",
      "properties": {
        "ref": {
          "type": "string",
          "description": "The source code file the control is defined in."
        },
        "line": {
          "type": "integer",
          "format": "int32",
          "description": "The line number the control is defined on."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Statistics": {
      "type": "object",
      "properties": {
        "duration": {
          "type": "number",
          "format": "float",
          "description": "The duration of the report's generation time."
        }
      },
      "description": "Statistics of the report's run."
    },
    "chef.automate.api.compliance.reporting.v1.Suggestion": {
      "type": "object",
      "properties": {
        "text": {
          "type": "string",
          "description": "The content that matched the search term."
        },
        "id": {
          "type": "string",
          "description": "The ID of the resource that was suggested."
        },
        "score": {
          "type": "number",
          "format": "float",
          "description": "The confidence in the match quality."
        },
        "version": {
          "type": "string",
          "description": "The version of the suggestion."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.SuggestionRequest": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string",
          "description": "The type of resource to get suggestions for."
        },
        "text": {
          "type": "string",
          "description": "The term to use to match resources on."
        },
        "size": {
          "type": "integer",
          "format": "int32",
          "description": "The maximum number of suggestions to return."
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ListFilter"
          },
          "description": "The criteria used to filter the suggestions returned."
        },
        "type_key": {
          "type": "string",
          "description": "The key (e.g. control_tag_key) to use for the type search."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Suggestions": {
      "type": "object",
      "properties": {
        "suggestions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.Suggestion"
          },
          "description": "The list of returned suggestions."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Support": {
      "type": "object",
      "properties": {
        "os_name": {
          "type": "string",
          "description": "The name of the supported operating system."
        },
        "os_family": {
          "type": "string",
          "description": "The wider category of supported platform (e.g., linux, windows)."
        },
        "release": {
          "type": "string",
          "description": "The specific operating system release number this profile supports."
        },
        "inspec_version": {
          "type": "string",
          "description": "The supported inspec version for this profile."
        },
        "platform": {
          "type": "string",
          "description": "The platform name and version combined."
        }
      }
    },
    "chef.automate.api.compliance.reporting.v1.Total": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "The total number of controls."
        }
      },
      "description": "A subtotal of controls."
    },
    "chef.automate.api.compliance.reporting.v1.WaiverData": {
      "type": "object",
      "properties": {
        "waived_str": {
          "type": "string",
          "description": "The waived state of the control item. Possible values: ` + "`" + `yes` + "`" + `, ` + "`" + `yes_run` + "`" + `, ` + "`" + `no` + "`" + `, ` + "`" + `no_expired` + "`" + `."
        },
        "expiration_date": {
          "type": "string",
          "description": "The expiration date for the waiver. After this date, the control is no longer waived."
        },
        "justification": {
          "type": "string",
          "description": "The reason for the waiver."
        },
        "waiver_summary": {
          "$ref": "#/definitions/chef.automate.api.compliance.reporting.v1.ControlSummary",
          "description": "Intentionally blank."
        }
      }
    },
    "google.protobuf.Any": {
      "type": "object",
      "properties": {
        "type_url": {
          "type": "string",
          "description": "A URL/resource name that uniquely identifies the type of the serialized\nprotocol buffer message. This string must contain at least\none \"/\" character. The last segment of the URL's path must represent\nthe fully qualified name of the type (as in\n` + "`" + `path/google.protobuf.Duration` + "`" + `). The name should be in a canonical form\n(e.g., leading \".\" is not accepted).\n\nIn practice, teams usually precompile into the binary all types that they\nexpect it to use in the context of Any. However, for URLs which use the\nscheme ` + "`" + `http` + "`" + `, ` + "`" + `https` + "`" + `, or no scheme, one can optionally set up a type\nserver that maps type URLs to message definitions as follows:\n\n* If no scheme is provided, ` + "`" + `https` + "`" + ` is assumed.\n* An HTTP GET on the URL must yield a [google.protobuf.Type][]\n  value in binary format, or produce an error.\n* Applications are allowed to cache lookup results based on the\n  URL, or have them precompiled into a binary to avoid any\n  lookup. Therefore, binary compatibility needs to be preserved\n  on changes to types. (Use versioned type names to manage\n  breaking changes.)\n\nNote: this functionality is not currently available in the official\nprotobuf release, and it is not used for type URLs beginning with\ntype.googleapis.com.\n\nSchemes other than ` + "`" + `http` + "`" + `, ` + "`" + `https` + "`" + ` (or the empty scheme) might be\nused with implementation specific semantics."
        },
        "value": {
          "type": "string",
          "format": "byte",
          "description": "Must be a valid serialized protocol buffer of the above specified type."
        }
      },
      "description": "` + "`" + `Any` + "`" + ` contains an arbitrary serialized protocol buffer message along with a\nURL that describes the type of the serialized message.\n\nProtobuf library provides support to pack/unpack Any values in the form\nof utility functions or additional generated methods of the Any type.\n\nExample 1: Pack and unpack a message in C++.\n\n    Foo foo = ...;\n    Any any;\n    any.PackFrom(foo);\n    ...\n    if (any.UnpackTo(\u0026foo)) {\n      ...\n    }\n\nExample 2: Pack and unpack a message in Java.\n\n    Foo foo = ...;\n    Any any = Any.pack(foo);\n    ...\n    if (any.is(Foo.class)) {\n      foo = any.unpack(Foo.class);\n    }\n\n Example 3: Pack and unpack a message in Python.\n\n    foo = Foo(...)\n    any = Any()\n    any.Pack(foo)\n    ...\n    if any.Is(Foo.DESCRIPTOR):\n      any.Unpack(foo)\n      ...\n\n Example 4: Pack and unpack a message in Go\n\n     foo := \u0026pb.Foo{...}\n     any, err := anypb.New(foo)\n     if err != nil {\n       ...\n     }\n     ...\n     foo := \u0026pb.Foo{}\n     if err := any.UnmarshalTo(foo); err != nil {\n       ...\n     }\n\nThe pack methods provided by protobuf library will by default use\n'type.googleapis.com/full.type.name' as the type URL and the unpack\nmethods only use the fully qualified type name after the last '/'\nin the type URL, for example \"foo.bar.com/x/y.z\" will yield type\nname \"y.z\".\n\n\nJSON\n====\nThe JSON representation of an ` + "`" + `Any` + "`" + ` value uses the regular\nrepresentation of the deserialized, embedded message, with an\nadditional field ` + "`" + `@type` + "`" + ` which contains the type URL. Example:\n\n    package google.profile;\n    message Person {\n      string first_name = 1;\n      string last_name = 2;\n    }\n\n    {\n      \"@type\": \"type.googleapis.com/google.profile.Person\",\n      \"firstName\": \u003cstring\u003e,\n      \"lastName\": \u003cstring\u003e\n    }\n\nIf the embedded message type is well-known and has a custom JSON\nrepresentation, that representation will be embedded adding a field\n` + "`" + `value` + "`" + ` which holds the custom JSON in addition to the ` + "`" + `@type` + "`" + `\nfield. Example (for message [google.protobuf.Duration][]):\n\n    {\n      \"@type\": \"type.googleapis.com/google.protobuf.Duration\",\n      \"value\": \"1.212s\"\n    }"
    },
    "grpc.gateway.runtime.Error": {
      "type": "object",
      "properties": {
        "error": {
          "type": "string"
        },
        "code": {
          "type": "integer",
          "format": "int32"
        },
        "message": {
          "type": "string"
        },
        "details": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/google.protobuf.Any"
          }
        }
      }
    },
    "grpc.gateway.runtime.StreamError": {
      "type": "object",
      "properties": {
        "grpc_code": {
          "type": "integer",
          "format": "int32"
        },
        "http_code": {
          "type": "integer",
          "format": "int32"
        },
        "message": {
          "type": "string"
        },
        "http_status": {
          "type": "string"
        },
        "details": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/google.protobuf.Any"
          }
        }
      }
    }
  }
}
`)
}
