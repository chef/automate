package api

func init() {
	Swagger.Add("auth_tokens_tokens", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/auth/tokens/tokens.proto",
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
    "/auth/tokens": {
      "get": {
        "operationId": "GetTokens",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.tokens.response.Tokens"
            }
          }
        },
        "tags": [
          "TokensMgmt"
        ]
      },
      "post": {
        "operationId": "CreateToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.tokens.response.Token"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.tokens.request.CreateToken"
            }
          }
        ],
        "tags": [
          "TokensMgmt"
        ]
      }
    },
    "/auth/tokens/{id}": {
      "get": {
        "operationId": "GetToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.tokens.response.Token"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "TokensMgmt"
        ]
      },
      "delete": {
        "operationId": "DeleteToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.tokens.response.DeleteTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "TokensMgmt"
        ]
      },
      "put": {
        "operationId": "UpdateToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.tokens.response.Token"
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
              "$ref": "#/definitions/chef.automate.api.tokens.request.UpdateToken"
            }
          }
        ],
        "tags": [
          "TokensMgmt"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.tokens.request.CreateToken": {
      "type": "object",
      "properties": {
        "description": {
          "type": "string"
        },
        "active": {
          "type": "boolean",
          "format": "boolean"
        },
        "value": {
          "type": "string"
        },
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.tokens.request.UpdateToken": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "active": {
          "type": "boolean",
          "format": "boolean"
        },
        "description": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.tokens.response.DeleteTokenResp": {
      "type": "object"
    },
    "chef.automate.api.tokens.response.Token": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "description": {
          "type": "string"
        },
        "value": {
          "type": "string"
        },
        "active": {
          "type": "boolean",
          "format": "boolean"
        },
        "created": {
          "type": "string"
        },
        "updated": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.tokens.response.Tokens": {
      "type": "object",
      "properties": {
        "tokens": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.tokens.response.Token"
          }
        }
      }
    }
  }
}
`)
}
