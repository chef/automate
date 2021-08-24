package api

func init() {
	Swagger.Add("data_feed", `{
  "swagger": "2.0",
  "info": {
    "title": "external/data_feed/data_feed.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/datafeed/config": {
      "get": {
        "operationId": "DatafeedService_GlobalDataFeedConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.GlobalDataFeedConfigResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/api/v0/datafeed/destination": {
      "post": {
        "operationId": "DatafeedService_AddDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.AddDestinationResponse"
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
              "$ref": "#/definitions/chef.automate.api.datafeed.AddDestinationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/api/v0/datafeed/destination/enable/{id}": {
      "patch": {
        "operationId": "DatafeedService_EnableDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.GetDestinationResponse"
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.UpdateDestinationEnableRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/api/v0/datafeed/destination/{id}": {
      "get": {
        "operationId": "DatafeedService_GetDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.GetDestinationResponse"
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
            "in": "path",
            "required": true,
            "type": "string",
            "format": "int64"
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      },
      "delete": {
        "operationId": "DatafeedService_DeleteDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.DeleteDestinationResponse"
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
            "in": "path",
            "required": true,
            "type": "string",
            "format": "int64"
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      },
      "patch": {
        "operationId": "DatafeedService_UpdateDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.UpdateDestinationResponse"
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.UpdateDestinationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/api/v0/datafeed/destinations": {
      "post": {
        "operationId": "DatafeedService_ListDestinations",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.ListDestinationResponse"
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
              "$ref": "#/definitions/chef.automate.api.datafeed.ListDestinationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/api/v0/datafeed/destinations/test": {
      "post": {
        "operationId": "DatafeedService_TestDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.TestDestinationResponse"
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
              "$ref": "#/definitions/chef.automate.api.datafeed.URLValidationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    }
  },
  "definitions": {
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
    "chef.automate.api.datafeed.AWS": {
      "type": "object",
      "properties": {
        "access_key": {
          "type": "string"
        },
        "secret_access_key": {
          "type": "string"
        },
        "bucket": {
          "type": "string"
        },
        "region": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.AddDestinationRequest": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.AddDestinationResponse": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.DeleteDestinationResponse": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.GetDestinationResponse": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.GlobalDataFeedConfigResponse": {
      "type": "object",
      "properties": {
        "feed_interval": {
          "type": "string"
        },
        "node_batch_size": {
          "type": "string",
          "format": "int64"
        },
        "updated_nodes_only": {
          "type": "boolean",
          "format": "boolean"
        },
        "disable_cidr_filter": {
          "type": "boolean",
          "format": "boolean"
        },
        "cidr_filter": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "accepted_status_codes": {
          "type": "array",
          "items": {
            "type": "integer",
            "format": "int32"
          }
        }
      }
    },
    "chef.automate.api.datafeed.Header": {
      "type": "object",
      "properties": {
        "value": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.ListDestinationRequest": {
      "type": "object"
    },
    "chef.automate.api.datafeed.ListDestinationResponse": {
      "type": "object",
      "properties": {
        "destinations": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.datafeed.GetDestinationResponse"
          }
        }
      }
    },
    "chef.automate.api.datafeed.SecretId": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.SecretIdWithExtraPrams": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.TestDestinationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.datafeed.URLValidationRequest": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        },
        "username_password": {
          "$ref": "#/definitions/chef.automate.api.datafeed.UsernamePassword"
        },
        "secret_id": {
          "$ref": "#/definitions/chef.automate.api.datafeed.SecretId"
        },
        "secret_id_with_addon": {
          "$ref": "#/definitions/chef.automate.api.datafeed.SecretIdWithExtraPrams"
        },
        "header": {
          "$ref": "#/definitions/chef.automate.api.datafeed.Header"
        },
        "aws": {
          "$ref": "#/definitions/chef.automate.api.datafeed.AWS"
        }
      }
    },
    "chef.automate.api.datafeed.UpdateDestinationEnableRequest": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.datafeed.UpdateDestinationRequest": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.UpdateDestinationResponse": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        },
        "services": {
          "type": "string"
        },
        "integration_types": {
          "type": "string"
        },
        "enable": {
          "type": "boolean",
          "format": "boolean"
        },
        "meta_data": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          }
        }
      }
    },
    "chef.automate.api.datafeed.UsernamePassword": {
      "type": "object",
      "properties": {
        "username": {
          "type": "string"
        },
        "password": {
          "type": "string"
        }
      }
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
