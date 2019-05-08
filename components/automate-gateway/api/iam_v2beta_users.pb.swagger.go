package api

func init() {
	Swagger.Add("iam_v2beta_users", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2beta/users.proto",
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
    "/iam/v2beta/self/{id}": {
      "put": {
        "operationId": "UpdateSelf",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaUpdateSelfResp"
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
              "$ref": "#/definitions/v2betaUpdateSelfReq"
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
        "operationId": "GetUsers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetUsersResp"
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
              "$ref": "#/definitions/v2betaCreateUserResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaCreateUserReq"
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
        "operationId": "GetUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetUserResp"
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
              "$ref": "#/definitions/v2betaDeleteUserResp"
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
              "$ref": "#/definitions/v2betaUpdateUserResp"
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
              "$ref": "#/definitions/v2betaUpdateUserReq"
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
    "v2betaCreateUserReq": {
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
    "v2betaCreateUserResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/v2betaUser"
        }
      }
    },
    "v2betaDeleteUserResp": {
      "type": "object"
    },
    "v2betaGetUserResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/v2betaUser"
        }
      }
    },
    "v2betaGetUsersResp": {
      "type": "object",
      "properties": {
        "users": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaUser"
          }
        }
      }
    },
    "v2betaUpdateSelfReq": {
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
    "v2betaUpdateSelfResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/v2betaUser"
        }
      }
    },
    "v2betaUpdateUserReq": {
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
    "v2betaUpdateUserResp": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/v2betaUser"
        }
      }
    },
    "v2betaUser": {
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
