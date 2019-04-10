package api

func init() {
	Swagger.Add("auth_users_users", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/auth/users/users.proto",
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
    "/auth/users": {
      "get": {
        "operationId": "GetUsers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseUsers"
            }
          }
        },
        "tags": [
          "UsersMgmt"
        ]
      },
      "post": {
        "operationId": "CreateUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseUser"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/requestCreateUser"
            }
          }
        ],
        "tags": [
          "UsersMgmt"
        ]
      }
    },
    "/auth/users/{username}": {
      "get": {
        "operationId": "GetUserByUsername",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseUser"
            }
          }
        },
        "parameters": [
          {
            "name": "username",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "UsersMgmt"
        ]
      },
      "delete": {
        "operationId": "DeleteUserByUsername",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseDeleteUserResp"
            }
          }
        },
        "parameters": [
          {
            "name": "username",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "UsersMgmt"
        ]
      },
      "put": {
        "operationId": "UpdateUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseUser"
            }
          }
        },
        "parameters": [
          {
            "name": "username",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/requestUpdateUser"
            }
          }
        ],
        "tags": [
          "UsersMgmt"
        ]
      }
    },
    "/users/{username}": {
      "put": {
        "operationId": "UpdateSelf",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseUser"
            }
          }
        },
        "parameters": [
          {
            "name": "username",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/requestUpdateSelf"
            }
          }
        ],
        "tags": [
          "UsersMgmt"
        ]
      }
    }
  },
  "definitions": {
    "requestCreateUser": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "email": {
          "type": "string"
        },
        "username": {
          "type": "string"
        },
        "password": {
          "type": "string"
        }
      }
    },
    "requestUpdateSelf": {
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
        },
        "username": {
          "type": "string"
        },
        "previous_password": {
          "type": "string"
        }
      }
    },
    "requestUpdateUser": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "email": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "password": {
          "type": "string"
        },
        "username": {
          "type": "string"
        }
      }
    },
    "responseDeleteUserResp": {
      "type": "object"
    },
    "responseUser": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "email": {
          "type": "string"
        },
        "username": {
          "type": "string"
        }
      }
    },
    "responseUsers": {
      "type": "object",
      "properties": {
        "users": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseUser"
          }
        }
      }
    }
  }
}
`)
}
