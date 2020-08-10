package api

func init() {
	Swagger.Add("gateway_gateway", `{
  "swagger": "2.0",
  "info": {
    "title": "automate-gateway/api/gateway/gateway.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/gateway/health": {
      "get": {
        "operationId": "Gateway_GetHealth",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.Health"
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
          "Gateway"
        ]
      }
    },
    "/api/v0/gateway/version": {
      "get": {
        "operationId": "Gateway_GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.Version"
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
          "Gateway"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.Health": {
      "type": "object",
      "properties": {
        "status": {
          "type": "string"
        }
      },
      "description": "The automate-gateway service health is constructed with:\n* Status:\n           =\u003e ok:             Everything is alright\n           =\u003e initialization: The service is in its initialization process\n           =\u003e warning:        Something might be wrong?\n           =\u003e critical:       Something is wrong!\n\n@afiune: Here we can add more health information to the response",
      "title": "Health message"
    },
    "chef.automate.api.Version": {
      "type": "object",
      "properties": {
        "version": {
          "type": "string"
        },
        "built": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "sha": {
          "type": "string"
        }
      },
      "description": "The service version constructed with:\n* Service name\n* Built time\n* Semantic version\n* Git SHA",
      "title": "Version message"
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
