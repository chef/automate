package api

func init() {
	Swagger.Add("deployment_deployment", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/deployment/deployment.proto",
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
        "operationId": "ServiceVersions",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.deployment.ServiceVersionsResponse"
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
        "operationId": "GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.deployment.Version"
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
    }
  }
}
`)
}
