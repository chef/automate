package api

func init() {
	Swagger.Add("telemetry_telemetry", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/telemetry/telemetry.proto",
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
    "/telemetry/config": {
      "get": {
        "operationId": "GetTelemetryConfiguration",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.telemetry.TelemetryResponse"
            }
          }
        },
        "tags": [
          "Telemetry"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.telemetry.TelemetryResponse": {
      "type": "object",
      "properties": {
        "license_id": {
          "type": "string"
        },
        "customer_name": {
          "type": "string"
        },
        "customer_id": {
          "type": "string"
        },
        "license_type": {
          "type": "string"
        },
        "telemetry_enabled": {
          "type": "boolean",
          "format": "boolean"
        },
        "telemetry_url": {
          "type": "string"
        },
        "max_nodes": {
          "type": "string",
          "format": "int64"
        },
        "deployment_id": {
          "type": "string"
        }
      }
    }
  }
}
`)
}
