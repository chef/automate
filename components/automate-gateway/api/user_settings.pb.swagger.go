package api

func init() {
	Swagger.Add("user_settings", `{
  "swagger": "2.0",
  "info": {
    "title": "external/user_settings/user_settings.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/user-settings/{user.name}/{user.connector}": {
      "get": {
        "summary": "GetUserSettings returns all of the preferences for a given user",
        "operationId": "UserSettingsService_GetUserSettings",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.user_settings.GetUserSettingsResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "user.name",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "user.connector",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "UserSettingsService"
        ]
      },
      "delete": {
        "summary": "DeleteUserSettings deletes all settings for a given user",
        "operationId": "UserSettingsService_DeleteUserSettings",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "user.name",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "user.connector",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "UserSettingsService"
        ]
      },
      "put": {
        "summary": "PutUserSettings upserts all of the preferences for a given user",
        "operationId": "UserSettingsService_PutUserSettings",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.user_settings.PutUserSettingsResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "user.name",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "user.connector",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.user_settings.PutUserSettingsRequest"
            }
          }
        ],
        "tags": [
          "UserSettingsService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.user_settings.GetUserSettingsResponse": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.user_settings.User"
        },
        "settings": {
          "type": "object",
          "additionalProperties": {
            "$ref": "#/definitions/chef.automate.api.user_settings.UserSettingValue"
          }
        }
      }
    },
    "chef.automate.api.user_settings.PutUserSettingsRequest": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.user_settings.User",
          "description": "ID of the user. Cannot be changed. Used to sign in."
        },
        "settings": {
          "type": "object",
          "additionalProperties": {
            "$ref": "#/definitions/chef.automate.api.user_settings.UserSettingValue"
          },
          "description": "The user settings to persist."
        }
      }
    },
    "chef.automate.api.user_settings.PutUserSettingsResponse": {
      "type": "object",
      "properties": {
        "user": {
          "$ref": "#/definitions/chef.automate.api.user_settings.User"
        }
      }
    },
    "chef.automate.api.user_settings.User": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "connector": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.user_settings.UserSettingValue": {
      "type": "object",
      "properties": {
        "default_value": {
          "type": "string",
          "description": "Default value for this setting."
        },
        "value": {
          "type": "string",
          "description": "Value for this setting."
        },
        "enabled": {
          "type": "boolean",
          "format": "boolean",
          "title": "Enabled"
        },
        "valid_values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Valid values for this setting."
        }
      }
    },
    "google.protobuf.Any": {
      "type": "object",
      "properties": {
        "type_url": {
          "type": "string"
        },
        "value": {
          "type": "string",
          "format": "byte"
        }
      }
    },
    "grpc.gateway.runtime.Error": {
      "type": "object",
      "properties": {
        "error": {
          "type": "string"
        },
        "code": {
          "type": "integer",
          "format": "int32"
        },
        "message": {
          "type": "string"
        },
        "details": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/google.protobuf.Any"
          }
        }
      }
    }
  }
}
`)
}
