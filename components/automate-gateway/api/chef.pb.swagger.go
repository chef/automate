package api

func init() {
	Swagger.Add("chef", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/ingest/chef.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/ingest/events/chef/action": {
      "post": {
        "operationId": "ProcessChefAction",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ProcessChefActionResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.request.Action"
            }
          }
        ],
        "tags": [
          "ChefIngester"
        ]
      }
    },
    "/ingest/events/chef/liveness": {
      "post": {
        "operationId": "ProcessLivenessPing",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ProcessLivenessResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.request.Liveness"
            }
          }
        ],
        "tags": [
          "ChefIngester"
        ]
      }
    },
    "/ingest/events/chef/node-multiple-deletes": {
      "post": {
        "operationId": "ProcessMultipleNodeDeletes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ProcessMultipleNodeDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.request.MultipleNodeDeleteRequest"
            }
          }
        ],
        "tags": [
          "ChefIngester"
        ]
      }
    },
    "/ingest/events/chef/nodedelete": {
      "post": {
        "operationId": "ProcessNodeDelete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ProcessNodeDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.request.Delete"
            }
          }
        ],
        "tags": [
          "ChefIngester"
        ]
      }
    },
    "/ingest/events/chef/run": {
      "post": {
        "operationId": "ProcessChefRun",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ProcessChefRunResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.request.Run"
            }
          }
        ],
        "tags": [
          "ChefIngester"
        ]
      }
    },
    "/ingest/version": {
      "get": {
        "operationId": "GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.common.version.VersionInfo"
            }
          }
        },
        "tags": [
          "ChefIngester"
        ]
      }
    }
  },
  "definitions": {
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
    "chef.automate.api.ingest.request.Action": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "ID of the action message itself"
        },
        "message_type": {
          "type": "string"
        },
        "message_version": {
          "type": "string"
        },
        "entity_name": {
          "type": "string"
        },
        "entity_type": {
          "type": "string"
        },
        "task": {
          "type": "string"
        },
        "organization_name": {
          "type": "string"
        },
        "remote_hostname": {
          "type": "string"
        },
        "run_id": {
          "type": "string"
        },
        "content": {
          "type": "string",
          "format": "byte",
          "description": "This new field called 'content' is being used to send the entire raw JSON\nmessage in bytes, this field is heavily used by the gateway for the DataCollector\nFunc Handler that will send the Action message to the (receiver) ingest-service\nthat will manually unmarshal the message from this field if it is provided.\nThe main purpose of this field it to improve the performance of ingestion when\nthe requests comes in REST/HTTP format."
        },
        "node_id": {
          "type": "string"
        },
        "recorded_at": {
          "type": "string"
        },
        "remote_request_id": {
          "type": "string"
        },
        "request_id": {
          "type": "string"
        },
        "requestor_name": {
          "type": "string"
        },
        "requestor_type": {
          "type": "string"
        },
        "service_hostname": {
          "type": "string"
        },
        "user_agent": {
          "type": "string"
        },
        "parent_type": {
          "type": "string"
        },
        "parent_name": {
          "type": "string"
        },
        "revision_id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.ingest.request.Delete": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "ID of the action message itself"
        },
        "node_name": {
          "type": "string"
        },
        "organization_name": {
          "type": "string"
        },
        "remote_hostname": {
          "type": "string"
        },
        "service_hostname": {
          "type": "string"
        },
        "node_id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.ingest.request.Deprecation": {
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
    "chef.automate.api.ingest.request.Description": {
      "type": "object",
      "properties": {
        "title": {
          "type": "string"
        },
        "sections": {
          "type": "array",
          "items": {
            "type": "object"
          }
        }
      }
    },
    "chef.automate.api.ingest.request.Error": {
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
          "$ref": "#/definitions/chef.automate.api.ingest.request.Description"
        }
      }
    },
    "chef.automate.api.ingest.request.ExpandedRunList": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "run_list": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.ingest.request.RunList"
          }
        }
      }
    },
    "chef.automate.api.ingest.request.Liveness": {
      "type": "object",
      "properties": {
        "event_type": {
          "type": "string"
        },
        "entity_uuid": {
          "type": "string"
        },
        "chef_server_fqdn": {
          "type": "string"
        },
        "source": {
          "type": "string"
        },
        "message_version": {
          "type": "string"
        },
        "organization_name": {
          "type": "string"
        },
        "node_name": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.ingest.request.MultipleNodeDeleteRequest": {
      "type": "object",
      "properties": {
        "node_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.ingest.request.Resource": {
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
        "after": {
          "type": "object"
        },
        "before": {
          "type": "object"
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
        "ignore_failure": {
          "type": "object"
        },
        "conditional": {
          "type": "string"
        },
        "result": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.ingest.request.Run": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "1 through 15 are for frequently occuring fields\nReserving for shared fields between run_start and run_converge mesages."
        },
        "run_id": {
          "type": "string"
        },
        "entity_uuid": {
          "type": "string"
        },
        "message_version": {
          "type": "string"
        },
        "message_type": {
          "type": "string"
        },
        "node_name": {
          "type": "string"
        },
        "organization_name": {
          "type": "string"
        },
        "start_time": {
          "type": "string"
        },
        "chef_server_fqdn": {
          "type": "string"
        },
        "content": {
          "type": "string",
          "format": "byte",
          "description": "This new field called 'content' is being used to send the entire raw JSON\nmessage in bytes, this field is heavily used by the gateway for the DataCollector\nFunc Handler that will send the Run message to the (receiver) ingest-service\nthat will manually unmarshal the message from this field if it is provided.\nThe main purpose of this field it to improve the performance of ingestion when\nthe requests comes in REST/HTTP format."
        },
        "end_time": {
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
        "source": {
          "type": "string"
        },
        "expanded_run_list": {
          "$ref": "#/definitions/chef.automate.api.ingest.request.ExpandedRunList"
        },
        "resources": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.ingest.request.Resource"
          }
        },
        "run_list": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "node": {
          "type": "object"
        },
        "error": {
          "$ref": "#/definitions/chef.automate.api.ingest.request.Error"
        },
        "policy_name": {
          "type": "string"
        },
        "policy_group": {
          "type": "string"
        },
        "deprecations": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.ingest.request.Deprecation"
          }
        },
        "tags": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.ingest.request.RunList": {
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
            "$ref": "#/definitions/chef.automate.api.ingest.request.RunList"
          }
        }
      }
    },
    "chef.automate.api.ingest.response.ProcessChefActionResponse": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.ProcessChefRunResponse": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.ProcessLivenessResponse": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.ProcessMultipleNodeDeleteResponse": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.ProcessNodeDeleteResponse": {
      "type": "object"
    },
    "google.protobuf.NullValue": {
      "type": "string",
      "enum": [
        "NULL_VALUE"
      ],
      "default": "NULL_VALUE",
      "description": "` + "`" + `NullValue` + "`" + ` is a singleton enumeration to represent the null value for the\n` + "`" + `Value` + "`" + ` type union.\n\n The JSON representation for ` + "`" + `NullValue` + "`" + ` is JSON ` + "`" + `null` + "`" + `.\n\n - NULL_VALUE: Null value."
    }
  }
}
`)
}
