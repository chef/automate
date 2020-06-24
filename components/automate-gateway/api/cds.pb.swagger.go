package api

func init() {
	Swagger.Add("cds", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/cds/cds.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/beta/content/install": {
      "post": {
        "summary": "InstallContentItem",
        "description": "Installs a content item from its ID\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncontent:items:install\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "InstallContentItem",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.cds.response.InstallContentItem"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.cds.request.InstallContentItem"
            }
          }
        ],
        "tags": [
          "Cds"
        ]
      }
    },
    "/api/beta/content/items": {
      "get": {
        "summary": "ListContentItems",
        "description": "Returns a list of metadata for each CDS content. Provides a description and current \nstate of each content item.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ncontent:items:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ListContentItems",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.cds.response.ContentItems"
            }
          }
        },
        "tags": [
          "Cds"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.cds.request.InstallContentItem": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.cds.response.ContentItem": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "description": {
          "type": "string"
        },
        "type": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "platforms": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "can_be_installed": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.cds.response.ContentItems": {
      "type": "object",
      "properties": {
        "items": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.cds.response.ContentItem"
          }
        }
      }
    },
    "chef.automate.api.cds.response.InstallContentItem": {
      "type": "object"
    }
  }
}
`)
}
