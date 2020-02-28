package api

func init() {
	Swagger.Add("iam_v2_tokens", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2/tokens.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/iam/v2/tokens": {
      "get": {
        "summary": "List all tokens",
        "description": "List all tokens, both admin and non-admin.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:tokens:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ListTokens",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListTokensResp"
            }
          }
        },
        "tags": [
          "tokens"
        ]
      },
      "post": {
        "summary": "Create a token",
        "description": "Creates a token.\nActive defaults to true when not specified.\nValue is auto-generated when not specified.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"name\": \"token 1\",\n\"id\": \"token-1\",\n\"active\": true,\n\"projects\": [\n\"east-region\",\n\"west-region\"\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nNote that this creates *non-admin* tokens that may then be assigned permissions via policies just like users or teams (unless you have already created policies that encompass all tokens using ` + "`" + `tokens:*` + "`" + `` + "`" + `).\n\nYou cannot create admin tokens via the REST API.\nAdmin tokens can only be created by specifying the ` + "`" + `--admin` + "`" + ` flag to this chef-automate sub-command:\n` + "`" + `` + "`" + `` + "`" + `\nchef-automate iam token create \u003cyour-token-name\u003e --admin` + "`" + `\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:tokens:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "CreateToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateTokenReq"
            }
          }
        ],
        "tags": [
          "tokens"
        ]
      }
    },
    "/iam/v2/tokens/{id}": {
      "get": {
        "summary": "Get a token",
        "description": "Returns the details for a token.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:tokens:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the token.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "tokens"
        ]
      },
      "delete": {
        "summary": "Delete a token",
        "description": "Delete a token and remove it from any policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:tokens:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "DeleteToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the token.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "tokens"
        ]
      },
      "put": {
        "summary": "Update a token",
        "description": "This operation overwrites all fields excepting ID, timestamps, and value,\nincluding those omitted from the request, so be sure to specify all properties.\nProperties that you do not include are reset to empty values.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:tokens:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "UpdateToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "Unique ID. Cannot be changed.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateTokenReq"
            }
          }
        ],
        "tags": [
          "tokens"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.CreateTokenReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the token."
        },
        "active": {
          "type": "boolean",
          "format": "boolean",
          "description": "Active state. Defaults to true.\nIf set to false, token will not be authenticated or authorized."
        },
        "value": {
          "type": "string",
          "description": "Unique value for the token; if omitted the system will generate this."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this token belongs to."
        }
      },
      "required": [
        "id",
        "name"
      ]
    },
    "chef.automate.api.iam.v2.CreateTokenResp": {
      "type": "object",
      "properties": {
        "token": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Token"
        }
      }
    },
    "chef.automate.api.iam.v2.DeleteTokenResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.GetTokenResp": {
      "type": "object",
      "properties": {
        "token": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Token"
        }
      }
    },
    "chef.automate.api.iam.v2.ListTokensResp": {
      "type": "object",
      "properties": {
        "tokens": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Token"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.ResetAllTokenProjectsResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.Token": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the token."
        },
        "value": {
          "type": "string",
          "description": "Unique, optionally user-specified value."
        },
        "active": {
          "type": "boolean",
          "format": "boolean",
          "description": "Active state. Defaults to true.\nIf set to false, token will not authenticate."
        },
        "created_at": {
          "type": "string",
          "description": "Created timestamp."
        },
        "updated_at": {
          "type": "string",
          "description": "Updated timestamp."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this token belongs to. May be empty."
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateTokenReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the token."
        },
        "active": {
          "type": "boolean",
          "format": "boolean",
          "description": "Active state. Defaults to true.\nIf set to false, token will not be authenticated or authorized."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this token belongs to."
        }
      },
      "required": [
        "name"
      ]
    },
    "chef.automate.api.iam.v2.UpdateTokenResp": {
      "type": "object",
      "properties": {
        "token": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Token"
        }
      }
    }
  }
}
`)
}
