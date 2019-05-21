package api

func init() {
	Swagger.Add("cfgmgmt", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/cfgmgmt/cfgmgmt.proto",
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
    "/cfgmgmt/nodes": {
      "get": {
        "operationId": "GetNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/protobufListValue"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "pagination.page",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "in": "query",
            "required": false,
            "type": "string",
            "enum": [
              "ASC",
              "DESC"
            ],
            "default": "ASC"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/nodes/{node_id}/attribute": {
      "get": {
        "operationId": "GetAttributes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseNodeAttribute"
            }
          }
        },
        "parameters": [
          {
            "name": "node_id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/nodes/{node_id}/runs": {
      "get": {
        "operationId": "GetRuns",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/protobufListValue"
            }
          }
        },
        "parameters": [
          {
            "name": "node_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "pagination.page",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "start",
            "description": "TODO: (@afiune) Should we standardize these parameters as well?.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "end",
            "in": "query",
            "required": false,
            "type": "string"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/nodes/{node_id}/runs/{run_id}": {
      "get": {
        "operationId": "GetNodeRun",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseRun"
            }
          }
        },
        "parameters": [
          {
            "name": "node_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "run_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "end_time",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "date-time"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/organizations": {
      "get": {
        "operationId": "GetOrganizations",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/protobufListValue"
            }
          }
        },
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/policy_revision/{revision_id}": {
      "get": {
        "operationId": "GetPolicyCookbooks",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responsePolicyCookbooks"
            }
          }
        },
        "parameters": [
          {
            "name": "revision_id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/source_fqdns": {
      "get": {
        "operationId": "GetSourceFqdns",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/protobufListValue"
            }
          }
        },
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/stats/node_counts": {
      "get": {
        "operationId": "GetNodesCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/cfgmgmtresponseNodesCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/stats/run_counts": {
      "get": {
        "operationId": "GetRunsCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/cfgmgmtresponseRunsCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "start",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "end",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "node_id",
            "in": "query",
            "required": false,
            "type": "string"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/suggestions": {
      "get": {
        "operationId": "GetSuggestions",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/protobufListValue"
            }
          }
        },
        "parameters": [
          {
            "name": "type",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "text",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          }
        ],
        "tags": [
          "ConfigMgmt"
        ]
      }
    },
    "/cfgmgmt/version": {
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
          "ConfigMgmt"
        ]
      }
    }
  },
  "definitions": {
    "cfgmgmtresponseNodesCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "success": {
          "type": "integer",
          "format": "int32"
        },
        "failure": {
          "type": "integer",
          "format": "int32"
        },
        "missing": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "cfgmgmtresponseRunsCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "success": {
          "type": "integer",
          "format": "int32"
        },
        "failure": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "protobufListValue": {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/protobufValue"
          },
          "description": "Repeated field of dynamically typed values."
        }
      },
      "description": "` + "`" + `ListValue` + "`" + ` is a wrapper around a repeated field of values.\n\nThe JSON representation for ` + "`" + `ListValue` + "`" + ` is JSON array."
    },
    "protobufNullValue": {
      "type": "string",
      "enum": [
        "NULL_VALUE"
      ],
      "default": "NULL_VALUE",
      "description": "` + "`" + `NullValue` + "`" + ` is a singleton enumeration to represent the null value for the\n` + "`" + `Value` + "`" + ` type union.\n\n The JSON representation for ` + "`" + `NullValue` + "`" + ` is JSON ` + "`" + `null` + "`" + `.\n\n - NULL_VALUE: Null value."
    },
    "protobufStruct": {
      "type": "object",
      "properties": {
        "fields": {
          "type": "object",
          "additionalProperties": {
            "$ref": "#/definitions/protobufValue"
          },
          "description": "Unordered map of dynamically typed values."
        }
      },
      "description": "` + "`" + `Struct` + "`" + ` represents a structured data value, consisting of fields\nwhich map to dynamically typed values. In some languages, ` + "`" + `Struct` + "`" + `\nmight be supported by a native representation. For example, in\nscripting languages like JS a struct is represented as an\nobject. The details of that representation are described together\nwith the proto support for the language.\n\nThe JSON representation for ` + "`" + `Struct` + "`" + ` is JSON object."
    },
    "protobufValue": {
      "type": "object",
      "properties": {
        "null_value": {
          "$ref": "#/definitions/protobufNullValue",
          "description": "Represents a null value."
        },
        "number_value": {
          "type": "number",
          "format": "double",
          "description": "Represents a double value."
        },
        "string_value": {
          "type": "string",
          "description": "Represents a string value."
        },
        "bool_value": {
          "type": "boolean",
          "format": "boolean",
          "description": "Represents a boolean value."
        },
        "struct_value": {
          "$ref": "#/definitions/protobufStruct",
          "description": "Represents a structured value."
        },
        "list_value": {
          "$ref": "#/definitions/protobufListValue",
          "description": "Represents a repeated ` + "`" + `Value` + "`" + `."
        }
      },
      "description": "` + "`" + `Value` + "`" + ` represents a dynamically typed value which can be either\nnull, a number, a string, a boolean, a recursive struct value, or a\nlist of values. A producer of value is expected to set one of that\nvariants, absence of any variant indicates an error.\n\nThe JSON representation for ` + "`" + `Value` + "`" + ` is JSON value."
    },
    "queryPagination": {
      "type": "object",
      "properties": {
        "page": {
          "type": "integer",
          "format": "int32"
        },
        "size": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "querySortOrder": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
    "querySorting": {
      "type": "object",
      "properties": {
        "field": {
          "type": "string"
        },
        "order": {
          "$ref": "#/definitions/querySortOrder"
        }
      }
    },
    "responseChefError": {
      "type": "object",
      "properties": {
        "class": {
          "type": "string"
        },
        "message": {
          "type": "string"
        },
        "backtrace": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "description": {
          "$ref": "#/definitions/responseDescription"
        }
      }
    },
    "responseCookbookLock": {
      "type": "object",
      "properties": {
        "cookbook": {
          "type": "string"
        },
        "policy_identifier": {
          "type": "string"
        }
      }
    },
    "responseDeprecation": {
      "type": "object",
      "properties": {
        "message": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "location": {
          "type": "string"
        }
      }
    },
    "responseDescription": {
      "type": "object",
      "properties": {
        "title": {
          "type": "string"
        },
        "sections": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/protobufStruct"
          }
        }
      }
    },
    "responseExpandedRunList": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "run_list": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseRunList"
          }
        }
      }
    },
    "responseNodeAttribute": {
      "type": "object",
      "properties": {
        "node_id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "run_list": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "chef_environment": {
          "type": "string"
        },
        "normal": {
          "type": "string"
        },
        "default": {
          "type": "string"
        },
        "override": {
          "type": "string"
        },
        "automatic": {
          "type": "string"
        },
        "normal_value_count": {
          "type": "integer",
          "format": "int32"
        },
        "default_value_count": {
          "type": "integer",
          "format": "int32"
        },
        "override_value_count": {
          "type": "integer",
          "format": "int32"
        },
        "all_value_count": {
          "type": "integer",
          "format": "int32"
        },
        "automatic_value_count": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "responsePolicyCookbooks": {
      "type": "object",
      "properties": {
        "policy_name": {
          "type": "string"
        },
        "cookbook_locks": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseCookbookLock"
          }
        }
      }
    },
    "responseResource": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "duration": {
          "type": "string"
        },
        "delta": {
          "type": "string"
        },
        "cookbook_name": {
          "type": "string"
        },
        "cookbook_version": {
          "type": "string"
        },
        "status": {
          "type": "string"
        },
        "recipe_name": {
          "type": "string"
        },
        "result": {
          "type": "string"
        },
        "conditional": {
          "type": "string",
          "title": "Fields that might be empty"
        },
        "ignore_failure": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "responseRun": {
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
        "organization": {
          "type": "string"
        },
        "start_time": {
          "type": "string",
          "format": "date-time"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        },
        "source": {
          "type": "string"
        },
        "status": {
          "type": "string"
        },
        "total_resource_count": {
          "type": "integer",
          "format": "int32"
        },
        "updated_resource_count": {
          "type": "integer",
          "format": "int32"
        },
        "chef_version": {
          "type": "string"
        },
        "uptime_seconds": {
          "type": "integer",
          "format": "int32"
        },
        "environment": {
          "type": "string"
        },
        "fqdn": {
          "type": "string"
        },
        "source_fqdn": {
          "type": "string"
        },
        "ipaddress": {
          "type": "string"
        },
        "resources": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseResource"
          }
        },
        "run_list": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "deprecations": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseDeprecation"
          }
        },
        "error": {
          "$ref": "#/definitions/responseChefError"
        },
        "tags": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "resource_names": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "recipes": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "chef_tags": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "cookbooks": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "platform": {
          "type": "string"
        },
        "platform_family": {
          "type": "string"
        },
        "platform_version": {
          "type": "string"
        },
        "roles": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "policy_name": {
          "type": "string"
        },
        "policy_group": {
          "type": "string"
        },
        "policy_revision": {
          "type": "string"
        },
        "expanded_run_list": {
          "$ref": "#/definitions/responseExpandedRunList"
        }
      }
    },
    "responseRunList": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "skipped": {
          "type": "boolean",
          "format": "boolean"
        },
        "children": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseRunList"
          }
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
  }
}
`)
}
