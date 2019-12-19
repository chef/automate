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
    "/iam/v2/self/{id}": {
      "put": {
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
            "description": "The id cannot be changed, it's for determining which user this is supposed\nto update",
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
          "Users"
        ]
      }
    },
    "/iam/v2/users": {
      "get": {
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
          "Users"
        ]
      },
      "post": {
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
          "Users"
        ]
      }
    },
    "/iam/v2/users/{id}": {
      "get": {
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Users"
        ]
      },
      "delete": {
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Users"
        ]
      },
      "put": {
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
            "description": "The id cannot be changed, it's for determining which user this is supposed\nto update",
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
          "Users"
        ]
      }
    },
    "/iam/v2beta/self/{id}": {
      "put": {
        "operationId": "UpdateSelf2",
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
            "description": "The id cannot be changed, it's for determining which user this is supposed\nto update",
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
          "Users"
        ]
      }
    },
    "/iam/v2beta/users": {
      "get": {
        "operationId": "ListUsers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListUsersResp"
            }
          }
        },
        "tags": [
          "Users"
        ]
      },
      "post": {
        "operationId": "CreateUser2",
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
          "Users"
        ]
      }
    },
    "/iam/v2beta/users/{id}": {
      "get": {
        "operationId": "GetUser2",
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Users"
        ]
      },
      "delete": {
        "operationId": "DeleteUser2",
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Users"
        ]
      },
      "put": {
        "operationId": "UpdateUser2",
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
            "description": "The id cannot be changed, it's for determining which user this is supposed\nto update",
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
          "Users"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.CreateUserReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "password": {
          "type": "string"
        }
      }
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
      "properties": {
        "id": {
          "type": "string",
          "title": "The id cannot be changed, it's for determining which user this is supposed\nto update"
        },
        "name": {
          "type": "string"
        },
        "password": {
          "type": "string",
          "description": "Note: for UpdateSelfReq, the password and previous_password are optional,\nbut if passed, they must both be passed."
        },
        "previous_password": {
          "type": "string"
        }
      }
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
      "properties": {
        "id": {
          "type": "string",
          "title": "The id cannot be changed, it's for determining which user this is supposed\nto update"
        },
        "name": {
          "type": "string"
        },
        "password": {
          "type": "string"
        }
      }
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
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "membership_id": {
          "type": "string",
          "title": "TODO (tc): Will remove post-GA"
        }
      }
    }
  }
}
`)
}
