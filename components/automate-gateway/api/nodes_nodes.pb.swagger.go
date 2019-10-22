package api

func init() {
	Swagger.Add("nodes_nodes", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/nodes/nodes.proto",
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
    "/nodes": {
      "post": {
        "operationId": "Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Id"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/bulk-create": {
      "post": {
        "operationId": "BulkCreate",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Ids"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Nodes"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/delete": {
      "post": {
        "operationId": "BulkDelete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.BulkDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Query"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/delete/ids": {
      "post": {
        "operationId": "BulkDeleteById",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.BulkDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Ids"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/id/{id}": {
      "get": {
        "operationId": "Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
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
          "NodesService"
        ]
      },
      "delete": {
        "operationId": "Delete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
          "NodesService"
        ]
      },
      "put": {
        "operationId": "Update",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/rerun/id/{id}": {
      "get": {
        "operationId": "Rerun",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.RerunResponse"
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
          "NodesService"
        ]
      }
    },
    "/nodes/search": {
      "post": {
        "operationId": "List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Nodes"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Query"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.nodes.v1.BulkDeleteResponse": {
      "type": "object",
      "properties": {
        "names": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.nodes.v1.Id": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.nodes.v1.Ids": {
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
    "chef.automate.api.nodes.v1.LastContactData": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData.Status"
        },
        "penultimate_status": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData.Status"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        }
      }
    },
    "chef.automate.api.nodes.v1.LastContactData.Status": {
      "type": "string",
      "enum": [
        "UNKNOWN",
        "PASSED",
        "FAILED",
        "SKIPPED"
      ],
      "default": "UNKNOWN"
    },
    "chef.automate.api.nodes.v1.Node": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "platform": {
          "type": "string"
        },
        "platform_version": {
          "type": "string"
        },
        "manager": {
          "type": "string"
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Kv"
          }
        },
        "last_contact": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string"
        },
        "last_job": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.ResultsRow"
        },
        "target_config": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.TargetConfig"
        },
        "manager_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "connection_error": {
          "type": "string"
        },
        "state": {
          "type": "string"
        },
        "name_prefix": {
          "type": "string"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "run_data": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData"
        },
        "scan_data": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData"
        }
      }
    },
    "chef.automate.api.nodes.v1.Nodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
          }
        },
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "total_unreachable": {
          "type": "integer",
          "format": "int32"
        },
        "total_reachable": {
          "type": "integer",
          "format": "int32"
        },
        "total_unknown": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.nodes.v1.Query": {
      "type": "object",
      "properties": {
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Filter"
          }
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.Query.OrderType"
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
    "chef.automate.api.nodes.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
    "chef.automate.api.nodes.v1.RerunResponse": {
      "type": "object"
    },
    "chef.automate.api.nodes.v1.ResultsRow": {
      "type": "object",
      "properties": {
        "node_id": {
          "type": "string"
        },
        "report_id": {
          "type": "string"
        },
        "status": {
          "type": "string"
        },
        "result": {
          "type": "string"
        },
        "job_id": {
          "type": "string"
        },
        "start_time": {
          "type": "string",
          "format": "date-time"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        }
      }
    },
    "chef.automate.api.nodes.v1.TargetConfig": {
      "type": "object",
      "properties": {
        "secrets": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "format": {
          "type": "string"
        },
        "backend": {
          "type": "string"
        },
        "host": {
          "type": "string"
        },
        "port": {
          "type": "integer",
          "format": "int32"
        },
        "path": {
          "type": "string"
        },
        "sudo": {
          "type": "boolean",
          "format": "boolean"
        },
        "ssl": {
          "type": "boolean",
          "format": "boolean"
        },
        "self_signed": {
          "type": "boolean",
          "format": "boolean"
        },
        "user": {
          "type": "string"
        },
        "key_files": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "sudo_options": {
          "type": "string"
        },
        "region": {
          "type": "string"
        },
        "subscription_id": {
          "type": "string"
        },
        "hosts": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.domain.compliance.api.common.Filter": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string"
        },
        "exclude": {
          "type": "boolean",
          "format": "boolean"
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.domain.compliance.api.common.Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string"
        },
        "value": {
          "type": "string"
        }
      }
    }
  }
}
`)
}
