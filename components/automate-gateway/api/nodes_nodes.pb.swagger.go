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
              "$ref": "#/definitions/v1Id"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Node"
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
              "$ref": "#/definitions/v1Ids"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Nodes"
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
              "$ref": "#/definitions/v1BulkDeleteResponse"
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
              "$ref": "#/definitions/v1BulkDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1Ids"
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
              "$ref": "#/definitions/v1Node"
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
              "$ref": "#/definitions/v1RerunResponse"
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
          "NodesService"
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
    "commonFilter": {
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
    "commonKv": {
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
    "v1BulkDeleteResponse": {
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
    "v1Id": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "v1Ids": {
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
            "$ref": "#/definitions/commonKv"
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
          "$ref": "#/definitions/v1ResultsRow"
        },
        "target_config": {
          "$ref": "#/definitions/v1TargetConfig"
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
    "v1Query": {
      "type": "object",
      "properties": {
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/commonFilter"
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
    "v1RerunResponse": {
      "type": "object"
    },
    "v1ResultsRow": {
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
    "v1TargetConfig": {
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
    }
  }
}
`)
}
