package api

func init() {
	Swagger.Add("manager", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/nodes/manager/manager.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/nodemanagers": {
      "post": {
        "summary": "Create a Node Manager",
        "description": "Creates a node manager given a name, credential id *or* credential data, and type.\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Ids"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManager"
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
        "summary": "View a Node Manager",
        "description": "List the details of a node manager.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManager"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "UUID for the node manager.",
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
        "summary": "Delete a Node Manager",
        "description": "Delete a single node manager. This deletes the node manager itself and\nreassigns its associated nodes to the Automate node manager.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
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
            "description": "UUID for the node manager.",
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
        "summary": "Update a Node Manager",
        "description": "Update a node manager's metadata, such as its name, associated credential id, or data.\nThis is a PUT operation and it overwrites ALL of the existing node manager metadata. Include all fields, because a PUT operation overwrites any missing fields to empty (\"\").\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:update\n` + "`" + `` + "`" + `` + "`" + `",
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
            "description": "UUID for the nodemanager.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManager"
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
        "summary": "Delete a Node Manager and Stop Nodes",
        "description": "Delete a node manager and update its associated nodes to ` + "`" + `stopped` + "`" + `.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
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
            "description": "UUID for the node manager.",
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
        "summary": "Delete a Node Manager and Terminate Nodes",
        "description": "Delete a node manager and update its associated nodes to ` + "`" + `terminated` + "`" + `.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
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
            "description": "UUID for the node manager.",
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
        "summary": "Delete a Node Manager and Delete Nodes",
        "description": "Delete a node manager and all of its associated nodes.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "DeleteWithNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Ids"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "UUID for the node manager.",
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
        "summary": "Search Node Fields",
        "description": "Searches the available values for a given field across all nodes associated with the nodemanager id.\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SearchNodeFields",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Fields"
            }
          }
        },
        "parameters": [
          {
            "name": "node_manager_id",
            "description": "Node manager ID.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.FieldQuery"
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
        "summary": "Search nodes",
        "description": "Searches the available nodes for a single node manager by id.\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SearchNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Nodes"
            }
          }
        },
        "parameters": [
          {
            "name": "node_manager_id",
            "description": "Node manager ID.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeQuery"
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
        "summary": "Connect",
        "description": "Attempts to reach the API for the given nodemanager id to validate the\ncredentials associated with the nodemanager.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:rerun\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Connect",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.ConnectResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "UUID for the node manager.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Id"
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
        "summary": "List all Node Managers",
        "description": "Returns a list of node managers.\nSupports filtering, sorting, and pagination.\n\nValid filtering fields: manager_type\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManagers"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Query"
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
    "chef.automate.api.common.query.Filter": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "description": "Field to filter on."
        },
        "exclude": {
          "type": "boolean",
          "format": "boolean",
          "description": "Include matches for this filter.(boolean)\n` + "`" + `true` + "`" + ` (default) *includes* all nodes that match this filter. \n` + "`" + `false` + "`" + ` *excludes* all nodes that match this filter."
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Field values to filter on."
        }
      }
    },
    "chef.automate.api.common.query.Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "description": "Tag key."
        },
        "value": {
          "type": "string",
          "description": "Tag value."
        }
      }
    },
    "chef.automate.api.nodes.manager.v1.ConnectResponse": {
      "type": "object"
    },
    "chef.automate.api.nodes.manager.v1.CredentialsByTags": {
      "type": "object",
      "properties": {
        "tag_key": {
          "type": "string",
          "description": "Tag key to match on."
        },
        "tag_value": {
          "type": "string",
          "description": "Tag value to match on."
        },
        "credential_ids": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of credential ids to associate with the key/value pair."
        }
      },
      "required": [
        "tag_key",
        "tag_value",
        "credential_ids"
      ]
    },
    "chef.automate.api.nodes.manager.v1.FieldQuery": {
      "type": "object",
      "properties": {
        "query": {
          "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Query",
          "description": "Query details (filters) to be applied to the results."
        },
        "field": {
          "type": "string",
          "description": "Possible search fields: regions, tags, name, subscription_id."
        },
        "node_manager_id": {
          "type": "string",
          "description": "Node manager ID."
        }
      },
      "required": [
        "node_manager_id"
      ]
    },
    "chef.automate.api.nodes.manager.v1.Fields": {
      "type": "object",
      "properties": {
        "fields": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "One or more fields: regions, tags, name, subscription_id."
        }
      }
    },
    "chef.automate.api.nodes.manager.v1.Id": {
      "type": "object",
      "example": {
        "uuid": "cd3ad3d9-2776-4ef1-a904-4c229d1642ee"
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "UUID for the node manager."
        }
      },
      "required": [
        "id"
      ]
    },
    "chef.automate.api.nodes.manager.v1.Ids": {
      "type": "object",
      "properties": {
        "ids": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Id"
          },
          "description": "List of node manager UUIDs."
        }
      }
    },
    "chef.automate.api.nodes.manager.v1.NodeManager": {
      "type": "object",
      "example": {
        "account_id": "12345EXAMPLE",
        "credential_data": [
          {
            "key": "AWS_ACCESS_KEY_ID",
            "value": "AKIAIOSFODNN7EXAMPLE"
          },
          {
            "key": "AWS_SECRET_ACCESS_KEY",
            "value": "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
          },
          {
            "key": "AWS_SESSION_TOKEN",
            "value": "AQoDYXdzEPT//////////wEXAMPLEt=="
          }
        ],
        "credential_ID": "my-credential-UUID",
        "id": "cd3ad3d9-2776-4ef1-a904-1EXAMPLEUUID",
        "instance_credentials": [],
        "name": "my aws api integration with session token",
        "type": "aws-ec2"
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "UUID for the nodemanager."
        },
        "name": {
          "type": "string",
          "description": "User defined name for the node manager."
        },
        "type": {
          "type": "string",
          "description": "Type of nodemanager (aws-ec2, azure-vm, aws-api, azure-api, gcp)."
        },
        "credential_id": {
          "type": "string",
          "description": "Use either 'credential_id' OR 'credential_data'.\n'credential_data' will overwrite values in 'credential_id'.\nThe 'credential_id' is the UUID of credential with the information\nyou need to connect to aws, azure, or gcp."
        },
        "instance_credentials": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.CredentialsByTags"
          },
          "description": "List of tag and credential UUID associations for making node managers.\nThese are ssh, winrm, and sudo creds used to access instances."
        },
        "status": {
          "type": "string",
          "description": "Status of the nodemanager (reachable, unreachable)."
        },
        "account_id": {
          "type": "string",
          "description": "Account id associated with the nodemanager."
        },
        "date_added": {
          "type": "string",
          "format": "date-time",
          "description": "Date the nodemanager was created."
        },
        "credential_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          },
          "description": "Use either 'credential_data' OR 'credential_id'.\n'credential_data' will overwrite values in 'credential_id'.\nUse 'credential_data' when you have not yet created node credentials\nand provide credential data (such as AWS_ACCESS_KEY) inline."
        }
      },
      "required": [
        "id",
        "credential_id",
        "credential_data",
        "type"
      ]
    },
    "chef.automate.api.nodes.manager.v1.NodeManagers": {
      "type": "object",
      "example": {
        "total": "1"
      },
      "properties": {
        "managers": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManager"
          },
          "description": "List of nodemanagers."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of nodemanagers."
        }
      }
    },
    "chef.automate.api.nodes.manager.v1.NodeQuery": {
      "type": "object",
      "properties": {
        "query": {
          "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Query",
          "description": "Valid search filters: manager_type."
        },
        "node_manager_id": {
          "type": "string",
          "description": "Node manager ID."
        }
      },
      "required": [
        "node_manager_id"
      ]
    },
    "chef.automate.api.nodes.manager.v1.Nodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of node names matching the request."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of node names matching the request."
        }
      }
    },
    "chef.automate.api.nodes.manager.v1.Query": {
      "type": "object",
      "example": {
        "filter_map": [
          {
            "key": "manager_type",
            "values": [
              "aws-ec2"
            ]
          }
        ],
        "sort": "date_added"
      },
      "properties": {
        "filter_map": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Filter"
          },
          "description": "Filters for the query: \"manager_type\"."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Query.OrderType"
        },
        "sort": {
          "type": "string",
          "description": "Field to use for sorting.\nValid fields are: name, type, status, status_message, date_added."
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "description": "Starting page for the list. For example, if your query returns 100 pages,\nand you know you're looking for a node manager somewhere in the middle,\nyou might want to start on page 50."
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "description": "Number of results on each page."
        }
      },
      "required": [
        "filter_map"
      ]
    },
    "chef.automate.api.nodes.manager.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC",
      "description": "Sort the results in ascending or descending order."
    }
  }
}
`)
}
