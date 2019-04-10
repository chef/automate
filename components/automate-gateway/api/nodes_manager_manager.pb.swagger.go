package api

func init() {
	Swagger.Add("nodes_manager_manager", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/nodes/manager/manager.proto",
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
    "/nodemanagers": {
      "post": {
        "operationId": "Create",
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
              "$ref": "#/definitions/v1NodeManager"
            }
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/id/{id}": {
      "get": {
        "operationId": "Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1NodeManager"
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
          "NodeManagerService"
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
          "NodeManagerService"
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
              "$ref": "#/definitions/v1NodeManager"
            }
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/id/{id}/with-node-state/stopped": {
      "delete": {
        "operationId": "DeleteWithNodeStateStopped",
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
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/id/{id}/with-node-state/terminated": {
      "delete": {
        "operationId": "DeleteWithNodeStateTerminated",
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
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/id/{id}/with-nodes": {
      "delete": {
        "operationId": "DeleteWithNodes",
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
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/id/{node_manager_id}/search-fields": {
      "post": {
        "operationId": "SearchNodeFields",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1Fields"
            }
          }
        },
        "parameters": [
          {
            "name": "node_manager_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1FieldQuery"
            }
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/id/{node_manager_id}/search-nodes": {
      "post": {
        "operationId": "SearchNodes",
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
            "name": "node_manager_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v1NodeQuery"
            }
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/rerun/id/{id}": {
      "post": {
        "operationId": "Connect",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1ConnectResponse"
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
              "$ref": "#/definitions/v1Id"
            }
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/nodemanagers/search": {
      "post": {
        "operationId": "List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v1NodeManagers"
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
          "NodeManagerService"
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
    "v1ConnectResponse": {
      "type": "object"
    },
    "v1CredentialsByTags": {
      "type": "object",
      "properties": {
        "tag_key": {
          "type": "string"
        },
        "tag_value": {
          "type": "string"
        },
        "credential_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v1FieldQuery": {
      "type": "object",
      "properties": {
        "query": {
          "$ref": "#/definitions/v1Query"
        },
        "field": {
          "type": "string"
        },
        "node_manager_id": {
          "type": "string"
        }
      }
    },
    "v1Fields": {
      "type": "object",
      "properties": {
        "fields": {
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
            "$ref": "#/definitions/v1Id"
          }
        }
      }
    },
    "v1NodeManager": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "type": {
          "type": "string"
        },
        "credential_id": {
          "type": "string"
        },
        "instance_credentials": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1CredentialsByTags"
          }
        },
        "status": {
          "type": "string"
        },
        "account_id": {
          "type": "string"
        },
        "date_added": {
          "type": "string",
          "format": "date-time"
        },
        "credential_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/commonKv"
          }
        }
      }
    },
    "v1NodeManagers": {
      "type": "object",
      "properties": {
        "managers": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v1NodeManager"
          }
        },
        "total": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1NodeQuery": {
      "type": "object",
      "properties": {
        "query": {
          "$ref": "#/definitions/v1Query"
        },
        "node_manager_id": {
          "type": "string"
        }
      }
    },
    "v1Nodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "total": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "v1Query": {
      "type": "object",
      "properties": {
        "filter_map": {
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
    }
  }
}
`)
}
