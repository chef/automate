package api

func init() {
	Swagger.Add("deployment_deployment", `{
  "swagger": "2.0",
  "info": {
    "title": "automate-gateway/api/deployment/deployment.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/deployment/service_versions": {
      "get": {
        "operationId": "Deployment_ServiceVersions",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.deployment.ServiceVersionsResponse"
            }
          },
          "default": {
            "description": "An unexpected error response.",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "Deployment"
        ]
      }
    },
    "/api/v0/version": {
      "get": {
        "operationId": "Deployment_GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.deployment.Version"
            }
          },
          "default": {
            "description": "An unexpected error response.",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "Deployment"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.deployment.ServiceVersion": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "origin": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "release": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.deployment.ServiceVersionsResponse": {
      "type": "object",
      "properties": {
        "services": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.deployment.ServiceVersion"
          }
        }
      }
    },
    "chef.automate.api.deployment.Version": {
      "type": "object",
      "properties": {
        "build_timestamp": {
          "type": "string"
        }
      },
      "description": "The manifest version constructed with:\n* build_timestamp",
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
