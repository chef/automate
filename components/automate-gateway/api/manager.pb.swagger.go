package api

func init() {
	Swagger.Add("manager", `{
  "swagger": "2.0",
  "info": {
    "title": "external/nodes/manager/manager.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/nodemanagers": {
      "post": {
        "summary": "Create a Node Manager",
        "description": "Creates a node manager given a name, credential id *or* credential data, and type.\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Ids"
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
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManager"
            }
          }
        ],
        "tags": [
          "NodeManagerService"
        ]
      }
    },
    "/api/v0/nodemanagers/id/{id}": {
      "get": {
        "summary": "View a Node Manager",
        "description": "List the details of a node manager.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManager"
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
        "operationId": "NodeManagerService_Delete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
        "operationId": "NodeManagerService_Update",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
    "/api/v0/nodemanagers/id/{id}/with-node-state/stopped": {
      "delete": {
        "summary": "Delete a Node Manager and Stop Nodes",
        "description": "Delete a node manager and update its associated nodes to ` + "`" + `stopped` + "`" + `.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_DeleteWithNodeStateStopped",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
    "/api/v0/nodemanagers/id/{id}/with-node-state/terminated": {
      "delete": {
        "summary": "Delete a Node Manager and Terminate Nodes",
        "description": "Delete a node manager and update its associated nodes to ` + "`" + `terminated` + "`" + `.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_DeleteWithNodeStateTerminated",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
    "/api/v0/nodemanagers/id/{id}/with-nodes": {
      "delete": {
        "summary": "Delete a Node Manager and Delete Nodes",
        "description": "Delete a node manager and all of its associated nodes.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_DeleteWithNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Ids"
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
    "/api/v0/nodemanagers/id/{node_manager_id}/search-fields": {
      "post": {
        "summary": "Search Node Fields",
        "description": "Searches the available values for a given field across all nodes associated with the nodemanager id.\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_SearchNodeFields",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Fields"
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
    "/api/v0/nodemanagers/id/{node_manager_id}/search-nodes": {
      "post": {
        "summary": "Search nodes",
        "description": "Searches the available nodes for a single node manager by id.\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_SearchNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.Nodes"
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
    "/api/v0/nodemanagers/rerun/id/{id}": {
      "post": {
        "summary": "Connect",
        "description": "Attempts to reach the API for the given nodemanager id to validate the\ncredentials associated with the nodemanager.\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:rerun\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_Connect",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.ConnectResponse"
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
    "/api/v0/nodemanagers/search": {
      "post": {
        "summary": "List all Node Managers",
        "description": "Returns a list of node managers.\nSupports filtering, sorting, and pagination.\n\nValid filtering fields: manager_type\n\n\nAuthorization Action:\n\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodeManagers:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodeManagerService_List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.manager.v1.NodeManagers"
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
    }
  }
}
`)
}
