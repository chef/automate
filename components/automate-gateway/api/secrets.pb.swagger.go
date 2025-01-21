package api

func init() {
	Swagger.Add("secrets", `{
  "swagger": "2.0",
  "info": {
    "title": "external/secrets/secrets.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/secrets": {
      "post": {
        "summary": "Create a secret",
        "description": "Creates a secret. Requires values for name, type, and data.\n\nSupported types: ssh, winrm, sudo, aws, azure, gcp, service_now\nSupported keys by type: \nssh: username, password, key\nwinrm: username, password\nsudo: username, password\nservice_now: username, password\naws: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN\nazure: AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID\nazure: AZURE_SUBSCRIPTION_ID is optional \ngcp: GOOGLE_CREDENTIALS_JSON\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"name\": \"my ssh secret\",\n\"type\": \"ssh\",\n\"data\": [\n{ \"key\": \"username\", \"value\": \"vagrant\" },\n{ \"key\": \"password\", \"value\": \"vagrant\"} \n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nsecrets:secrets:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SecretsService_Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.secrets.Id"
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
              "$ref": "#/definitions/chef.automate.api.secrets.Secret"
            }
          }
        ],
        "tags": [
          "SecretsService"
        ]
      }
    },
    "/api/v0/secrets/id/{id}": {
      "get": {
        "summary": "Read a secret",
        "description": "Reads a secret given the ID of the secret.\nNote that the secret information (password and key values) will not be returned by the API, as a safety measure.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nsecrets:secrets:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SecretsService_Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.secrets.Secret"
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
            "description": "Unique node ID (UUID).",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "SecretsService"
        ]
      },
      "delete": {
        "summary": "Delete a secret",
        "description": "Deletes a secret given the ID of the secret.\nNote that any nodes that were using the secret will no longer be associated with the deleted secret.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nsecrets:secrets:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SecretsService_Delete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.secrets.DeleteResponse"
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
            "description": "Unique node ID (UUID).",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "SecretsService"
        ]
      },
      "patch": {
        "summary": "Update a secret",
        "description": "Updates a secret. \nThis is a PATCH operation, meaning the details sent in will override/replace those stored in the DB.\nSecret information that is not in the body of the request will persist.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\ngiven a credential with a username and password, a user could update the password by passing in the following body, \nand the name of the secret as well as the username for the secret be unchanged:\n\n{\n\"id\": \"525c013a-2ab3-4e6f-9005-51bc620e9157\",\n\"data\": [\n{ \"key\": \"password\", \"value\": \"new-value\"} \n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nsecrets:secrets:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SecretsService_Update",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.secrets.UpdateResponse"
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
            "description": "Unique node ID (UUID).",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.secrets.Secret"
            }
          }
        ],
        "tags": [
          "SecretsService"
        ]
      }
    },
    "/api/v0/secrets/search": {
      "post": {
        "summary": "List and filter secrets",
        "description": "Makes a list of secrets.\nSupports filtering, pagination, and sorting.\nAdding a filter narrows the list of secrets to only those that match the filter or filters.\nSupported filters: type, name\nSupported sort types: name, type, last modified\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"sort\": \"type\",\n\"order\": \"ASC\",\n\"filters\": [\n{ \"key\": \"type\", \"values\": [\"ssh\",\"winrm\",\"sudo\"] }\n],\n\"page\":1,\n\"per_page\":100\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nsecrets:secrets:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "SecretsService_List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.secrets.Secrets"
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
              "$ref": "#/definitions/chef.automate.api.secrets.Query"
            }
          }
        ],
        "tags": [
          "SecretsService"
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
    "chef.automate.api.secrets.DeleteResponse": {
      "type": "object"
    },
    "chef.automate.api.secrets.Id": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique node ID (UUID)."
        }
      }
    },
    "chef.automate.api.secrets.Query": {
      "type": "object",
      "properties": {
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Filter"
          },
          "description": "Use filters to limit the set of secrets."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.secrets.Query.OrderType"
        },
        "sort": {
          "type": "string",
          "description": "Sort the results on a specific field."
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "description": "Starting page for the results."
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "description": "The number of results on each page."
        }
      }
    },
    "chef.automate.api.secrets.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC",
      "description": "Return the results in ascending or descending order."
    },
    "chef.automate.api.secrets.Secret": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique node ID (UUID)."
        },
        "name": {
          "type": "string",
          "description": "User-specified name for the secret."
        },
        "type": {
          "type": "string",
          "title": "Type of credential: ssh, winrm, sudo, aws, azure, gcp, service_now"
        },
        "last_modified": {
          "type": "string",
          "format": "date-time",
          "description": "Timestamp denoting when the secret was last modified."
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          },
          "description": "Tags to associate with the secret."
        },
        "data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          },
          "description": "Secret data, where the kv structs for the credential data live."
        }
      }
    },
    "chef.automate.api.secrets.Secrets": {
      "type": "object",
      "properties": {
        "secrets": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.secrets.Secret"
          },
          "description": "List of secrets."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "title": "Total count of secrets"
        }
      }
    },
    "chef.automate.api.secrets.UpdateResponse": {
      "type": "object"
    },
    "google.protobuf.Any": {
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
