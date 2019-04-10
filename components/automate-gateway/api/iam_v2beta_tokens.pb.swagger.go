package api

func init() {
	Swagger.Add("iam_v2beta_tokens", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2beta/tokens.proto",
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
    "/iam/v2beta/tokens": {
      "get": {
        "operationId": "ListTokens",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListTokensResp"
            }
          }
        },
        "tags": [
          "Tokens"
        ]
      },
      "post": {
        "operationId": "CreateToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaCreateTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaCreateTokenReq"
            }
          }
        ],
        "tags": [
          "Tokens"
        ]
      }
    },
    "/iam/v2beta/tokens/{id}": {
      "get": {
        "operationId": "GetToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetTokenResp"
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
          "Tokens"
        ]
      },
      "delete": {
        "operationId": "DeleteToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaDeleteTokenResp"
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
          "Tokens"
        ]
      },
      "put": {
        "operationId": "UpdateToken",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaUpdateTokenResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID can't be changed; ID used to discover token",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaUpdateTokenReq"
            }
          }
        ],
        "tags": [
          "Tokens"
        ]
      }
    }
  },
  "definitions": {
    "v2betaCreateTokenReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "active": {
          "type": "boolean",
          "format": "boolean"
        },
        "value": {
          "type": "string"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaCreateTokenResp": {
      "type": "object",
      "properties": {
        "token": {
          "$ref": "#/definitions/v2betaToken"
        }
      }
    },
    "v2betaDeleteTokenResp": {
      "type": "object"
    },
    "v2betaGetTokenResp": {
      "type": "object",
      "properties": {
        "token": {
          "$ref": "#/definitions/v2betaToken"
        }
      }
    },
    "v2betaListTokensResp": {
      "type": "object",
      "properties": {
        "tokens": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaToken"
          }
        }
      }
    },
    "v2betaToken": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "value": {
          "type": "string"
        },
        "active": {
          "type": "boolean",
          "format": "boolean"
        },
        "created_at": {
          "type": "string"
        },
        "updated_at": {
          "type": "string"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaUpdateTokenReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "ID can't be changed; ID used to discover token"
        },
        "name": {
          "type": "string"
        },
        "active": {
          "type": "boolean",
          "format": "boolean"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaUpdateTokenResp": {
      "type": "object",
      "properties": {
        "token": {
          "$ref": "#/definitions/v2betaToken"
        }
      }
    }
  }
}
`)
}
