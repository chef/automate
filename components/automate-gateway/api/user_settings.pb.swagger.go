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
    "/api/v0/user/{id}/settings": {
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
            "name": "id",
            "description": "ID of the user.",
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
              "$ref": "#/definitions/chef.automate.api.user_settings.DeleteUserSettingsResponse"
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
            "name": "id",
            "description": "ID of the user.",
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
        "summary": "UpdateUserSettings upserts all of the preferences for a given user",
        "operationId": "UserSettingsService_UpdateUserSettings",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.user_settings.UpdateUserSettingsResponse"
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
              "$ref": "#/definitions/chef.automate.api.user_settings.UpdateUserSettingsRequest"
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
    "chef.automate.api.user_settings.DeleteUserSettingsResponse": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.user_settings.GetUserSettingsResponse": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "settings": {
          "type": "object",
          "additionalProperties": {
            "$ref": "#/definitions/chef.automate.api.user_settings.UserSettingValue"
          }
        }
      }
    },
    "chef.automate.api.user_settings.UpdateUserSettingsRequest": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
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
    "chef.automate.api.user_settings.UpdateUserSettingsResponse": {
      "type": "object",
      "properties": {
        "id": {
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
