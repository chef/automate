package api

func init() {
	Swagger.Add("iam_v2_users", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2/users.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/apis/iam/v2/self/{id}": {
      "put": {
        "summary": "Update self (as user)",
        "description": "Updates a local user's own name or password.\nIf changing the password, both \"password\" and \"previous_password\" are required.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:usersSelf:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "UpdateSelf",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateSelfResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the user. Cannot be changed. Used to sign in.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateSelfReq"
            }
          }
        ],
        "tags": [
          "users"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My New Name\", \"password\": \"aNewSafePassword\", \"previous_password\": \"aPreviousSafePassword\"}"
          }
        ]
      }
    },
    "/apis/iam/v2/users": {
      "get": {
        "summary": "List all users",
        "description": "Lists all local users.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:users:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ListUsers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListUsersResp"
            }
          }
        },
        "tags": [
          "users"
        ]
      },
      "post": {
        "summary": "Create a user",
        "description": "Creates a local user that can sign in to Automate and be a member of IAM teams\nand IAM policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:users:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "CreateUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateUserResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateUserReq"
            }
          }
        ],
        "tags": [
          "users"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"Display Name\", \"id\": \"username001rulez\", \"password\": \"aSafePassword\"}"
          }
        ]
      }
    },
    "/apis/iam/v2/users/{id}": {
      "get": {
        "summary": "Get a user",
        "description": "Returns the details for a local user.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:users:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetUserResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the user.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "users"
        ]
      },
      "delete": {
        "summary": "Delete a user",
        "description": "Deletes a local user.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:users:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "DeleteUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteUserResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the user.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "users"
        ]
      },
      "put": {
        "summary": "Update a user",
        "description": "Updates a local user's name or password.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:users:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "UpdateUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateUserResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the user. Cannot be changed. Used to sign in.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateUserReq"
            }
          }
        ],
        "tags": [
          "users"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"New Name\", \"password\": \"aSafePassword\"}"
          }
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.CreateUserReq": {
      "type": "object",
      "example": {
        "name": "Display Name",
        "id": "username001rulez",
        "password": "aSafePassword"
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed. Used to sign in."
        },
        "name": {
          "type": "string",
          "description": "Display name for local user."
        },
        "password": {
          "type": "string",
          "description": "Password for user. Used to sign in."
        }
      },
      "required": [
        "id",
        "name",
        "password"
      ]
    },
    "chef.automate.api.iam.v2.CreateUserResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.User"
        }
      }
    },
    "chef.automate.api.iam.v2.DeleteUserResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.GetUserResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.User"
        }
      }
    },
    "chef.automate.api.iam.v2.ListUsersResp": {
      "type": "object",
      "properties": {
        "users": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.User"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateSelfReq": {
      "type": "object",
      "example": {
        "name": "My New Name",
        "password": "aNewSafePassword",
        "previous_password": "aPreviousSafePassword"
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the user. Cannot be changed. Used to sign in."
        },
        "name": {
          "type": "string",
          "description": "Display name for local user."
        },
        "password": {
          "type": "string",
          "description": "New password for user. Used to sign in.\nOptional, but if included, previous_password is also required."
        },
        "previous_password": {
          "type": "string",
          "description": "Previous password for user.\nOptional, but if included, password is also required."
        }
      },
      "required": [
        "name"
      ]
    },
    "chef.automate.api.iam.v2.UpdateSelfResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.User"
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateUserReq": {
      "type": "object",
      "example": {
        "name": "New Name",
        "password": "aSafePassword"
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the user. Cannot be changed. Used to sign in."
        },
        "name": {
          "type": "string",
          "description": "Display name for local user."
        },
        "password": {
          "type": "string",
          "description": "Password used to log in. Will overwrite preexisting password."
        }
      },
      "required": [
        "name"
      ]
    },
    "chef.automate.api.iam.v2.UpdateUserResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.User"
        }
      }
    },
    "chef.automate.api.iam.v2.User": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Display name for local user."
        },
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed. Used to log in."
        },
        "membership_id": {
          "type": "string",
          "description": "Unique ID used to add local users to local teams. Cannot be changed."
        }
      }
    }
  }
}
`)
}
