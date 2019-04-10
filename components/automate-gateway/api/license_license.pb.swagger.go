package api

func init() {
	Swagger.Add("license_license", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/license/license.proto",
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
    "/license/apply": {
      "post": {
        "operationId": "ApplyLicense",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/licenseApplyLicenseResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/licenseApplyLicenseReq"
            }
          }
        ],
        "tags": [
          "License"
        ]
      }
    },
    "/license/request": {
      "post": {
        "operationId": "RequestLicense",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/licenseRequestLicenseResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/licenseRequestLicenseReq"
            }
          }
        ],
        "tags": [
          "License"
        ]
      }
    },
    "/license/status": {
      "get": {
        "operationId": "GetStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/licenseGetStatusResp"
            }
          }
        },
        "tags": [
          "License"
        ]
      }
    }
  },
  "definitions": {
    "GetStatusRespDateRange": {
      "type": "object",
      "properties": {
        "start": {
          "type": "string",
          "format": "date-time"
        },
        "end": {
          "type": "string",
          "format": "date-time"
        }
      }
    },
    "licenseApplyLicenseReq": {
      "type": "object",
      "properties": {
        "license": {
          "type": "string"
        }
      }
    },
    "licenseApplyLicenseResp": {
      "type": "object",
      "properties": {
        "status": {
          "$ref": "#/definitions/licenseGetStatusResp"
        }
      }
    },
    "licenseGetStatusResp": {
      "type": "object",
      "properties": {
        "license_id": {
          "type": "string"
        },
        "configured_at": {
          "type": "string",
          "format": "date-time"
        },
        "licensed_period": {
          "$ref": "#/definitions/GetStatusRespDateRange"
        },
        "customer_name": {
          "type": "string"
        }
      }
    },
    "licenseRequestLicenseReq": {
      "type": "object",
      "properties": {
        "first_name": {
          "type": "string"
        },
        "last_name": {
          "type": "string"
        },
        "email": {
          "type": "string"
        },
        "gdpr_agree": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "licenseRequestLicenseResp": {
      "type": "object",
      "properties": {
        "license": {
          "type": "string"
        },
        "status": {
          "$ref": "#/definitions/licenseGetStatusResp"
        }
      }
    }
  }
}
`)
}
