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
    "/compliance/reporting/controls": {
      "post": {
        "summary": "should cover /controls\nThis api is useful for getting a limited list of control items for latest runs.  It also honors all reporting filters",
        "operationId": "ListControlItems",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1ControlItems"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1ControlItemRequest"
            }
          }
        ],
        "tags": [
          "ReportingService"
        ]
      }
    },
    "/compliance/reporting/nodes/id/{id}": {
      "get": {
        "summary": "Fetch a node",
        "description": "Fetch a specific node by id",
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
            "description": "The id of the node to fetch",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Compliance Reporting Nodes"
        ]
      }
    },
    "/compliance/reporting/nodes/search": {
      "post": {
        "summary": "List nodes",
        "description": "List all nodes optionally using filters",
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
          "Compliance Reporting Nodes"
        ]
      }
    },
    "/compliance/reporting/profiles": {
      "post": {
        "summary": "List profiles",
        "description": "List all profiles optionally using filters",
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
          "Compliance Reporting Profiles"
        ]
      }
    },
    "/compliance/reporting/report-ids": {
      "post": {
        "summary": "List report IDs",
        "description": "List all report IDs optionally using filters",
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
          "Compliance Reporting Report IDs"
        ]
      }
    },
    "/compliance/reporting/reports": {
      "post": {
        "summary": "List reports",
        "description": "List all reports optionally using filters",
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
          "Compliance Reporting Reports"
        ]
      }
    },
    "/compliance/reporting/reports/id/{id}": {
      "post": {
        "summary": "Fetch a report",
        "description": "Fetch a specific report by id",
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
            "description": "The id of a specific resource to fetch",
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
          "Compliance Reporting Reports"
        ]
      }
    },
    "/compliance/reporting/suggestions": {
      "post": {
        "summary": "List suggestions",
        "description": "Get suggestions for compliance reporting resources based on matching text substrings",
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
          "Compliance Reporting Suggestions"
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
          "hidden"
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
      "default": "ASC",
      "title": "The two allowed values for ordering results"
    },
    "protobufAny": {
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
      "description": "` + "`" + `Any` + "`" + ` contains an arbitrary serialized protocol buffer message along with a\nURL that describes the type of the serialized message.\n\nProtobuf library provides support to pack/unpack Any values in the form\nof utility functions or additional generated methods of the Any type.\n\nExample 1: Pack and unpack a message in C++.\n\n    Foo foo = ...;\n    Any any;\n    any.PackFrom(foo);\n    ...\n    if (any.UnpackTo(\u0026foo)) {\n      ...\n    }\n\nExample 2: Pack and unpack a message in Java.\n\n    Foo foo = ...;\n    Any any = Any.pack(foo);\n    ...\n    if (any.is(Foo.class)) {\n      foo = any.unpack(Foo.class);\n    }\n\n Example 3: Pack and unpack a message in Python.\n\n    foo = Foo(...)\n    any = Any()\n    any.Pack(foo)\n    ...\n    if any.Is(Foo.DESCRIPTOR):\n      any.Unpack(foo)\n      ...\n\n Example 4: Pack and unpack a message in Go\n\n     foo := \u0026pb.Foo{...}\n     any, err := ptypes.MarshalAny(foo)\n     ...\n     foo := \u0026pb.Foo{}\n     if err := ptypes.UnmarshalAny(any, foo); err != nil {\n       ...\n     }\n\nThe pack methods provided by protobuf library will by default use\n'type.googleapis.com/full.type.name' as the type URL and the unpack\nmethods only use the fully qualified type name after the last '/'\nin the type URL, for example \"foo.bar.com/x/y.z\" will yield type\nname \"y.z\".\n\n\nJSON\n====\nThe JSON representation of an ` + "`" + `Any` + "`" + ` value uses the regular\nrepresentation of the deserialized, embedded message, with an\nadditional field ` + "`" + `@type` + "`" + ` which contains the type URL. Example:\n\n    package google.profile;\n    message Person {\n      string first_name = 1;\n      string last_name = 2;\n    }\n\n    {\n      \"@type\": \"type.googleapis.com/google.profile.Person\",\n      \"firstName\": \u003cstring\u003e,\n      \"lastName\": \u003cstring\u003e\n    }\n\nIf the embedded message type is well-known and has a custom JSON\nrepresentation, that representation will be embedded adding a field\n` + "`" + `value` + "`" + ` which holds the custom JSON in addition to the ` + "`" + `@type` + "`" + `\nfield. Example (for message [google.protobuf.Duration][]):\n\n    {\n      \"@type\": \"type.googleapis.com/google.protobuf.Duration\",\n      \"value\": \"1.212s\"\n    }"
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
    "v1ControlItem": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "title": {
          "type": "string"
        },
        "profile": {
          "$ref": "#/definitions/v1ProfileMin"
        },
        "impact": {
          "type": "number",
          "format": "float"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        },
        "control_summary": {
          "$ref": "#/definitions/v1ControlSummary"
        }
      }
    },
    "v1ControlItemRequest": {
      "type": "object",
      "properties": {
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
    "v1ControlItems": {
      "type": "object",
      "properties": {
        "control_items": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ControlItem"
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
          "format": "byte",
          "title": "The exported reports in the requested format"
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
          },
          "title": "The values to filter for"
        },
        "type": {
          "type": "string",
          "title": "The field to filter on"
        }
      },
      "description": "The list of filters used in the request."
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
        },
        "total_passed": {
          "type": "integer",
          "format": "int32"
        },
        "total_failed": {
          "type": "integer",
          "format": "int32"
        },
        "total_skipped": {
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
        },
        "full": {
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
        "full": {
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
        },
        "full": {
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
          "type": "string",
          "title": "The id of a specific resource to fetch"
        },
        "type": {
          "type": "string",
          "title": "The type of request being made ???"
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ListFilter"
          },
          "title": "The list of filters used to narrow down the list"
        },
        "order": {
          "$ref": "#/definitions/QueryOrderType",
          "title": "Whether to sort in ascending or descending order"
        },
        "sort": {
          "type": "string",
          "title": "The field to sort by"
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "title": "The offset to use when paginating requests"
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "title": "The number of results to return with each paginated request"
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
          },
          "title": "The list of report ids found matching the query"
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
          "type": "string",
          "title": "The content that matched the search term"
        },
        "id": {
          "type": "string",
          "title": "The id of the resource that was suggested"
        },
        "score": {
          "type": "number",
          "format": "float",
          "title": "The confidence in the match quality"
        },
        "version": {
          "type": "string",
          "title": "???"
        }
      }
    },
    "v1SuggestionRequest": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string",
          "title": "The type of resource to get suggestions for"
        },
        "text": {
          "type": "string",
          "title": "The term to use to match resources on"
        },
        "size": {
          "type": "integer",
          "format": "int32",
          "title": "The maximum number of suggestions to return"
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ListFilter"
          },
          "description": "The criteria used to filter the suggestions returned."
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
          },
          "description": "The list of returned suggestions."
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
