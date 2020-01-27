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
        "description": "Creates a token.\nName, ID are required.\nActive defaults to true when not specified.\nValue is auto-generated when not provided.\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"name\": \"token 1\",\n\"id\": \"token-1\",\n\"active\": true,\n\"projects\": [\n\"east-region\",\n\"west-region\"\n]\n}\n` + "`" + `` + "`" + `` + "`" + `",
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
        "description": "Returns the details for a token.",
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
        "description": "This PUT operation will overwrite all fields excepting ID, timestamps, and value,\nincluding those omitted from the request. Include ` + "`" + `name` + "`" + `, ` + "`" + `active` + "`" + ` and ` + "`" + `projects` + "`" + `\nin the request.",
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
            "description": "Unique, user-specified ID. Cannot be changed.",
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
    },
    "/iam/v2beta/tokens": {
      "get": {
        "summary": "List all tokens",
        "operationId": "ListTokens2",
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
        "description": "Creates a token.\nName, ID are required.\nActive defaults to true when not specified.\nValue is auto-generated when not provided.\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"name\": \"token 1\",\n\"id\": \"token-1\",\n\"active\": true,\n\"projects\": [\n\"east-region\",\n\"west-region\"\n]\n}\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "CreateToken2",
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
    "/iam/v2beta/tokens/{id}": {
      "get": {
        "summary": "Get a token",
        "description": "Returns the details for a token.",
        "operationId": "GetToken2",
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
        "operationId": "DeleteToken2",
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
        "description": "This PUT operation will overwrite all fields excepting ID, timestamps, and value,\nincluding those omitted from the request. Include ` + "`" + `name` + "`" + `, ` + "`" + `active` + "`" + ` and ` + "`" + `projects` + "`" + `\nin the request.",
        "operationId": "UpdateToken2",
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
            "description": "Unique, user-specified ID. Cannot be changed.",
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
          "description": "Unique, user-specified ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "User-specified name."
        },
        "active": {
          "type": "boolean",
          "format": "boolean",
          "description": "Active state. Defaults to true.\nIf set to false, token will not be authenticated or authorized."
        },
        "value": {
          "type": "string",
          "description": "Unique, optionally user-specified value."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Array of projects the token is in. Empty by default."
        }
      }
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
          "description": "Unique, optionally user-specified ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "User-specified name."
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
          "description": "Array of projects token is in. Empty by default."
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateTokenReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique, user-specified ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "User-specified name."
        },
        "active": {
          "type": "boolean",
          "format": "boolean",
          "description": "Active state. Defaults to true.\nIf set to false, token will not authenticate."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Array of projects token is in. Empty by default."
        }
      }
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
