package api

func init() {
	Swagger.Add("gateway_gateway", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/gateway/gateway.proto",
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
    "/gateway/health": {
      "get": {
        "operationId": "GetHealth",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.Health"
            }
          }
        },
        "tags": [
          "Gateway"
        ]
      }
    },
    "/gateway/version": {
      "get": {
        "operationId": "GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.Version"
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
    }
  }
}
`)
}
