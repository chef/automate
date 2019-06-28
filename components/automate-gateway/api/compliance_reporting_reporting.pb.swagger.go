package api

func init() {
	Swagger.Add("compliance_reporting_reporting", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/compliance/reporting/reporting.proto",
    "version": "version not set"
  },
  "schemes": [
    "http",
    "https"
  ],
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/compliance/reporting/nodes/id/{id}": {
      "get": {
        "operationId": "ReadNode",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1Node"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
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
    "/compliance/reporting/nodes/search": {
      "post": {
        "operationId": "ListNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1Nodes"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/profiles": {
      "post": {
        "summary": "should cover /search/profiles",
        "operationId": "ListProfiles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1ProfileMins"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/report-ids": {
      "post": {
        "summary": "should cover /reportids\nNB - ListReportIds is not limited to the 10k like some of the other reporting apis\nThis api is useful for getting a complete list of runids for latest runs.  It also honors all reporting filters",
        "operationId": "ListReportIds",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1ReportIds"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/reports": {
      "post": {
        "summary": "should cover /reports",
        "operationId": "ListReports",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1Reports"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/reports/id/{id}": {
      "post": {
        "summary": "should cover /reports/:reportid",
        "operationId": "ReadReport",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1Report"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Query"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/suggestions": {
      "post": {
        "summary": "should cover /suggestions",
        "operationId": "ListSuggestions",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1Suggestions"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1SuggestionRequest"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/version": {
      "get": {
        "operationId": "GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/versionVersionInfo"
            }
          }
        },
        "tags": [
          "ReportingService"
        ]
      }
    }
  },
  "definitions": {
    "QueryOrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
    "protobufAny": {
      "type": "object",
      "properties": {
        "type_url": {
          "type": "string"
        },
        "value": {
          "type": "string",
          "format": "byte"
        }
      }
    },
    "runtimeStreamError": {
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
            "$ref": "#/definitions/protobufAny"
          }
        }
      }
    },
    "v1Attribute": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "options": {
          "$ref": "#/definitions/v1Option"
        }
      }
    },
    "v1Control": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "code": {
          "type": "string"
        },
        "desc": {
          "type": "string"
        },
        "impact": {
          "type": "number",
          "format": "float"
        },
        "title": {
          "type": "string"
        },
        "source_location": {
          "$ref": "#/definitions/v1SourceLocation"
        },
        "results": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Result"
          }
        },
        "refs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Ref"
          }
        },
        "tags": {
          "type": "object",
          "additionalProperties": {
            "type": "string"
          }
        }
      }
    },
    "v1ControlSummary": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "passed": {
          "$ref": "#/definitions/v1Total"
        },
        "skipped": {
          "$ref": "#/definitions/v1Total"
        },
        "failed": {
          "$ref": "#/definitions/v1Failed"
        }
      }
    },
    "v1Dependency": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "path": {
          "type": "string"
        },
        "git": {
          "type": "string"
        },
        "branch": {
          "type": "string"
        },
        "tag": {
          "type": "string"
        },
        "commit": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "supermarket": {
          "type": "string"
        },
        "github": {
          "type": "string"
        },
        "compliance": {
          "type": "string"
        },
        "status": {
          "type": "string"
        },
        "skip_message": {
          "type": "string"
        }
      }
    },
    "v1ExportData": {
      "type": "object",
      "properties": {
        "content": {
          "type": "string",
          "format": "byte"
        }
      }
    },
    "v1Failed": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "minor": {
          "type": "integer",
          "format": "int32"
        },
        "major": {
          "type": "integer",
          "format": "int32"
        },
        "critical": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1Group": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "title": {
          "type": "string"
        },
        "controls": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v1Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string"
        },
        "value": {
          "type": "string"
        }
      }
    },
    "v1LatestReportSummary": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string"
        },
        "controls": {
          "$ref": "#/definitions/v1ControlSummary"
        }
      }
    },
    "v1ListFilter": {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "type": {
          "type": "string"
        }
      }
    },
    "v1Node": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "platform": {
          "$ref": "#/definitions/v1Platform"
        },
        "environment": {
          "type": "string"
        },
        "latest_report": {
          "$ref": "#/definitions/v1LatestReportSummary"
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Kv"
          }
        },
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ProfileMeta"
          }
        }
      }
    },
    "v1Nodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Node"
          }
        },
        "total": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1Option": {
      "type": "object",
      "properties": {
        "description": {
          "type": "string"
        },
        "default": {
          "type": "string"
        }
      }
    },
    "v1Platform": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "release": {
          "type": "string"
        }
      }
    },
    "v1Profile": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "title": {
          "type": "string"
        },
        "maintainer": {
          "type": "string"
        },
        "copyright": {
          "type": "string"
        },
        "copyright_email": {
          "type": "string"
        },
        "license": {
          "type": "string"
        },
        "summary": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "owner": {
          "type": "string"
        },
        "supports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Support"
          }
        },
        "depends": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Dependency"
          }
        },
        "sha256": {
          "type": "string"
        },
        "groups": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Group"
          }
        },
        "controls": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Control"
          }
        },
        "attributes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Attribute"
          }
        },
        "latest_version": {
          "type": "string"
        },
        "status": {
          "type": "string"
        },
        "skip_message": {
          "type": "string"
        }
      }
    },
    "v1ProfileCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "failed": {
          "type": "integer",
          "format": "int32"
        },
        "skipped": {
          "type": "integer",
          "format": "int32"
        },
        "passed": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1ProfileMeta": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "status": {
          "type": "string"
        }
      }
    },
    "v1ProfileMin": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "title": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "status": {
          "type": "string"
        }
      }
    },
    "v1ProfileMins": {
      "type": "object",
      "properties": {
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ProfileMin"
          }
        },
        "counts": {
          "$ref": "#/definitions/v1ProfileCounts"
        }
      }
    },
    "v1Query": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "type": {
          "type": "string"
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ListFilter"
          }
        },
        "order": {
          "$ref": "#/definitions/QueryOrderType"
        },
        "sort": {
          "type": "string"
        },
        "page": {
          "type": "integer",
          "format": "int32"
        },
        "per_page": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1Ref": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        },
        "ref": {
          "type": "string"
        }
      }
    },
    "v1Report": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "node_id": {
          "type": "string"
        },
        "node_name": {
          "type": "string"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string"
        },
        "controls": {
          "$ref": "#/definitions/v1ControlSummary"
        },
        "environment": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "platform": {
          "$ref": "#/definitions/v1Platform"
        },
        "statistics": {
          "$ref": "#/definitions/v1Statistics"
        },
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Profile"
          }
        },
        "job_id": {
          "type": "string"
        },
        "ipaddress": {
          "type": "string"
        },
        "fqdn": {
          "type": "string"
        }
      }
    },
    "v1ReportIds": {
      "type": "object",
      "properties": {
        "ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v1Reports": {
      "type": "object",
      "properties": {
        "reports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Report"
          }
        },
        "total": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1Result": {
      "type": "object",
      "properties": {
        "status": {
          "type": "string"
        },
        "code_desc": {
          "type": "string"
        },
        "run_time": {
          "type": "number",
          "format": "float"
        },
        "start_time": {
          "type": "string"
        },
        "message": {
          "type": "string"
        },
        "skip_message": {
          "type": "string"
        }
      }
    },
    "v1SourceLocation": {
      "type": "object",
      "properties": {
        "ref": {
          "type": "string"
        },
        "line": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1Statistics": {
      "type": "object",
      "properties": {
        "duration": {
          "type": "number",
          "format": "float"
        }
      }
    },
    "v1Suggestion": {
      "type": "object",
      "properties": {
        "text": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "score": {
          "type": "number",
          "format": "float"
        },
        "version": {
          "type": "string"
        }
      }
    },
    "v1SuggestionRequest": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string"
        },
        "text": {
          "type": "string"
        },
        "size": {
          "type": "integer",
          "format": "int32"
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ListFilter"
          }
        }
      }
    },
    "v1Suggestions": {
      "type": "object",
      "properties": {
        "suggestions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Suggestion"
          }
        }
      }
    },
    "v1Support": {
      "type": "object",
      "properties": {
        "os_name": {
          "type": "string"
        },
        "os_family": {
          "type": "string"
        },
        "release": {
          "type": "string"
        },
        "inspec_version": {
          "type": "string"
        },
        "platform": {
          "type": "string"
        }
      }
    },
    "v1Total": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "versionVersionInfo": {
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
    }
  },
  "x-stream-definitions": {
    "v1ExportData": {
      "type": "object",
      "properties": {
        "result": {
          "$ref": "#/definitions/v1ExportData"
        },
        "error": {
          "$ref": "#/definitions/runtimeStreamError"
        }
      },
      "title": "Stream result of v1ExportData"
    }
  }
}
`)
}
