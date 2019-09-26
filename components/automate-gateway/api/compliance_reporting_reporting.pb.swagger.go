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
            "description": "Used by the ReadReport endpoint to specify which report to return",
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
          "type": "string",
          "title": "The name of the attribute"
        },
        "options": {
          "$ref": "#/definitions/v1Option",
          "title": "The options defined for the attribute"
        }
      }
    },
    "v1Control": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "The unique id of this control"
        },
        "code": {
          "type": "string",
          "title": "The full ruby code of the control defined in the profile"
        },
        "desc": {
          "type": "string",
          "title": "The full description of the control"
        },
        "impact": {
          "type": "number",
          "format": "float",
          "title": "The severity of the control"
        },
        "title": {
          "type": "string",
          "title": "The compact description of the control"
        },
        "source_location": {
          "$ref": "#/definitions/v1SourceLocation",
          "title": "Intentionally blank"
        },
        "results": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Result"
          },
          "title": "The results of running all tests defined in the control against the node"
        },
        "refs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Ref"
          },
          "title": "External supporting documents for the control"
        },
        "tags": {
          "type": "object",
          "additionalProperties": {
            "type": "string"
          },
          "title": "Metadata defined on the control in key-value format"
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
          "format": "int32",
          "title": "The total number of controls in the report"
        },
        "passed": {
          "$ref": "#/definitions/v1Total",
          "title": "Intentionally blank"
        },
        "skipped": {
          "$ref": "#/definitions/v1Total",
          "title": "Intentionally blank"
        },
        "failed": {
          "$ref": "#/definitions/v1Failed",
          "title": "Intentionally blank"
        }
      },
      "title": "A minimal represenation of the statuses of the controls in the report"
    },
    "v1Dependency": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "title": "The name of the profile"
        },
        "url": {
          "type": "string",
          "title": "The URL of the profile accessible over HTTP or HTTPS"
        },
        "path": {
          "type": "string",
          "title": "The path to the profile on disk"
        },
        "git": {
          "type": "string",
          "title": "The git URL of the profile"
        },
        "branch": {
          "type": "string",
          "title": "The specific git branch of the dependency"
        },
        "tag": {
          "type": "string",
          "title": "The specific git tag of the dependency"
        },
        "commit": {
          "type": "string",
          "title": "The specific git commit of the dependency"
        },
        "version": {
          "type": "string",
          "title": "The specific git version of the dependency"
        },
        "supermarket": {
          "type": "string",
          "title": "The name of the dependency stored in Chef Supermarket"
        },
        "github": {
          "type": "string",
          "title": "The short name of the dependency stored on Github"
        },
        "compliance": {
          "type": "string",
          "title": "The short name of the dependency stored on the Chef Automate or Chef Compliance server"
        },
        "status": {
          "type": "string",
          "title": "The status of the dependency in the report"
        },
        "skip_message": {
          "type": "string",
          "title": "The reason this profile was skipped in the generated report, if any"
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
          "format": "int32",
          "title": "The total number of failed controls"
        },
        "minor": {
          "type": "integer",
          "format": "int32",
          "title": "The number of failed controls with minor severity"
        },
        "major": {
          "type": "integer",
          "format": "int32",
          "title": "The number of failed controls with major severity"
        },
        "critical": {
          "type": "integer",
          "format": "int32",
          "title": "The number of failed controls with critical severity"
        }
      },
      "title": "Stats of failed controls"
    },
    "v1Group": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "???"
        },
        "title": {
          "type": "string",
          "title": "???"
        },
        "controls": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "title": "???"
        }
      }
    },
    "v1Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "title": "The key of the tag"
        },
        "value": {
          "type": "string",
          "title": "The value of the tag"
        }
      }
    },
    "v1LatestReportSummary": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "The id of the latest report"
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "title": "The time the report was submitted at"
        },
        "status": {
          "type": "string",
          "title": "The status of the run the report was made from"
        },
        "controls": {
          "$ref": "#/definitions/v1ControlSummary",
          "title": "Intentionally blank"
        }
      },
      "title": "A summary of the information contained in the latest report for this node"
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
      }
    },
    "v1Node": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "The id of this node"
        },
        "name": {
          "type": "string",
          "title": "The name assigned to the node"
        },
        "platform": {
          "$ref": "#/definitions/v1Platform",
          "title": "Intentionally blank"
        },
        "environment": {
          "type": "string",
          "title": "The environment assigned to the node"
        },
        "latest_report": {
          "$ref": "#/definitions/v1LatestReportSummary",
          "title": "A summary of the information contained in the latest report for this node"
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Kv"
          },
          "title": "The tags assigned to this node"
        },
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1ProfileMeta"
          },
          "title": "A minimal represenation of the compliance profiles run against the node"
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
          },
          "title": "The nodes matching the request filters"
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of nodes matching the filters"
        },
        "total_passed": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of passing nodes matching the filters"
        },
        "total_failed": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of failed nodes matching the filters"
        },
        "total_skipped": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of skipped nodes matching the filters"
        }
      }
    },
    "v1Option": {
      "type": "object",
      "properties": {
        "description": {
          "type": "string",
          "title": "The description of the attribute"
        },
        "default": {
          "type": "string",
          "title": "The default value of the attribute"
        }
      }
    },
    "v1Platform": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "title": "The name of the node's operating system"
        },
        "release": {
          "type": "string",
          "title": "The version of the node's operating system"
        },
        "full": {
          "type": "string"
        }
      },
      "title": "The name and version of the node's operating system"
    },
    "v1Profile": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "title": "The name of the profile. Must be unique"
        },
        "title": {
          "type": "string",
          "title": "The human-readable name of the profile"
        },
        "maintainer": {
          "type": "string",
          "title": "The maintainer listed in the profile metadata"
        },
        "copyright": {
          "type": "string",
          "title": "The name of the copyright holder"
        },
        "copyright_email": {
          "type": "string",
          "title": "The contact information for the copyright holder"
        },
        "license": {
          "type": "string",
          "title": "The license the profile is released under"
        },
        "summary": {
          "type": "string",
          "title": "A short description of the profile"
        },
        "version": {
          "type": "string",
          "title": "The version of the profile"
        },
        "owner": {
          "type": "string",
          "title": "The name of the account that uploaded the profile to Automate"
        },
        "full": {
          "type": "string",
          "title": "???"
        },
        "supports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Support"
          },
          "title": "The supported platform targets"
        },
        "depends": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Dependency"
          },
          "title": "Other profiles that this profile depends on"
        },
        "sha256": {
          "type": "string",
          "title": "A unique value generated from the profile used to identify it"
        },
        "groups": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Group"
          },
          "title": "???"
        },
        "controls": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Control"
          },
          "title": "The controls defined on the profile"
        },
        "attributes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Attribute"
          },
          "title": "The attributes defined on the profile"
        },
        "latest_version": {
          "type": "string",
          "title": "The highest version number of the profile stored in Automate"
        },
        "status": {
          "type": "string",
          "title": "The status of the profile in the generated report"
        },
        "skip_message": {
          "type": "string",
          "title": "The reason this profile was skipped in the generated report, if any"
        }
      }
    },
    "v1ProfileCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of nodes matching the filters"
        },
        "failed": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of failed nodes matching the filters"
        },
        "skipped": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of skipped nodes matching the filters"
        },
        "passed": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of passing nodes matching the filters"
        }
      },
      "title": "Stats on the statuses of nodes matching the filters"
    },
    "v1ProfileMeta": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "title": "The name of the profile"
        },
        "version": {
          "type": "string",
          "title": "The version of the profile"
        },
        "id": {
          "type": "string",
          "title": "The unique id of the profile"
        },
        "status": {
          "type": "string",
          "title": "The status of the profile run against the node"
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
          "type": "string",
          "title": "The name of the profile"
        },
        "title": {
          "type": "string",
          "title": "The title of the profile"
        },
        "id": {
          "type": "string",
          "title": "The id of the profile"
        },
        "version": {
          "type": "string",
          "title": "The version of the profile"
        },
        "status": {
          "type": "string",
          "title": "The aggregated status of the profile across the nodes it has been run on"
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
          },
          "title": "Minimal represenations of the profiles matching the filters"
        },
        "counts": {
          "$ref": "#/definitions/v1ProfileCounts",
          "title": "Intentionally blank"
        }
      }
    },
    "v1Query": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "Used by the ReadReport endpoint to specify which report to return"
        },
        "type": {
          "type": "string",
          "title": "Used by the ListSuggestions endpoint to control the type of suggestions requested, used by the Export endpoint to control the file format of the returned documents"
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
          "title": "The field to sort the list of results by"
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
          "type": "string",
          "title": "The URL of the external document"
        },
        "ref": {
          "type": "string",
          "title": "The description of the external document"
        }
      }
    },
    "v1Report": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "The id of the report"
        },
        "node_id": {
          "type": "string",
          "title": "The id of the node generating the report"
        },
        "node_name": {
          "type": "string",
          "title": "The name of the node generating the report"
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "title": "The time the report was submitted at"
        },
        "status": {
          "type": "string",
          "title": "The status of the run the report was made from"
        },
        "controls": {
          "$ref": "#/definitions/v1ControlSummary",
          "title": "Intentionally blank"
        },
        "environment": {
          "type": "string",
          "title": "The environment of the node generating the report"
        },
        "version": {
          "type": "string",
          "title": "The version of the report (???)"
        },
        "platform": {
          "$ref": "#/definitions/v1Platform",
          "title": "Intentionally blank"
        },
        "statistics": {
          "$ref": "#/definitions/v1Statistics",
          "title": "Intentionally blank"
        },
        "profiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1Profile"
          },
          "title": "The profiles run as part of this report"
        },
        "job_id": {
          "type": "string",
          "title": "The id of the job associated with the report (???)"
        },
        "ipaddress": {
          "type": "string",
          "title": "The IP address of the node generating the report"
        },
        "fqdn": {
          "type": "string",
          "title": "The FQDN (fully qualified domain name) of the node generating the report"
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
          },
          "title": "The paginated results of reports matching the filters"
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "title": "The total number of reports matching the filters"
        }
      }
    },
    "v1Result": {
      "type": "object",
      "properties": {
        "status": {
          "type": "string",
          "title": "The status of the test"
        },
        "code_desc": {
          "type": "string",
          "title": "The description of the test"
        },
        "run_time": {
          "type": "number",
          "format": "float",
          "title": "The time taken to run the test"
        },
        "start_time": {
          "type": "string",
          "title": "The timestamp of when this individual test was run"
        },
        "message": {
          "type": "string",
          "title": "The reason the test failed, if any"
        },
        "skip_message": {
          "type": "string",
          "title": "The reason the test was skipped, if any"
        }
      }
    },
    "v1SourceLocation": {
      "type": "object",
      "properties": {
        "ref": {
          "type": "string",
          "title": "The source code file the control is defined in"
        },
        "line": {
          "type": "integer",
          "format": "int32",
          "title": "The line number the control is defined on"
        }
      }
    },
    "v1Statistics": {
      "type": "object",
      "properties": {
        "duration": {
          "type": "number",
          "format": "float",
          "title": "The duration of the report's generation time"
        }
      },
      "title": "Statistics of the report's run"
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
          "title": "The criteria used to filter the suggestions returned"
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
          "title": "The list of returned suggestions"
        }
      }
    },
    "v1Support": {
      "type": "object",
      "properties": {
        "os_name": {
          "type": "string",
          "title": "The name of the supported operating system"
        },
        "os_family": {
          "type": "string",
          "title": "The name of the broader category of the supported platform (eg, linux, windows)"
        },
        "release": {
          "type": "string",
          "title": "The specific release of the operating system this profile supports"
        },
        "inspec_version": {
          "type": "string",
          "title": "The supported inspec version this profile was made to run on"
        },
        "platform": {
          "type": "string",
          "title": "The platform name and version combined"
        }
      }
    },
    "v1Total": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32",
          "title": "The number of controls"
        }
      },
      "title": "A subtotal of controls"
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
