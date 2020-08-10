package api

func init() {
	Swagger.Add("notifications_notifications", `{
  "swagger": "2.0",
  "info": {
    "title": "automate-gateway/api/notifications/notifications.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/notifications/rules": {
      "get": {
        "operationId": "Notifications_ListRules",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleListResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "Notifications"
        ]
      },
      "post": {
        "operationId": "Notifications_AddRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleAddResponse"
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
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleAddRequest"
            }
          }
        ],
        "tags": [
          "Notifications"
        ]
      }
    },
    "/api/v0/notifications/rules/{id}": {
      "get": {
        "operationId": "Notifications_GetRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleGetResponse"
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Notifications"
        ]
      },
      "delete": {
        "operationId": "Notifications_DeleteRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleDeleteResponse"
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Notifications"
        ]
      },
      "put": {
        "operationId": "Notifications_UpdateRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleUpdateResponse"
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.RuleUpdateRequest"
            }
          }
        ],
        "tags": [
          "Notifications"
        ]
      }
    },
    "/api/v0/notifications/version": {
      "get": {
        "operationId": "Notifications_Version",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.VersionResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "Notifications"
        ]
      }
    },
    "/api/v0/notifications/webhook": {
      "post": {
        "operationId": "Notifications_ValidateWebhook",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.URLValidationResponse"
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
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.notifications.URLValidationRequest"
            }
          }
        ],
        "tags": [
          "Notifications"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.notifications.Empty": {
      "type": "object"
    },
    "chef.automate.api.notifications.Rule": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "event": {
          "$ref": "#/definitions/chef.automate.api.notifications.Rule.Event"
        },
        "SlackAlert": {
          "$ref": "#/definitions/chef.automate.api.notifications.SlackAlert"
        },
        "WebhookAlert": {
          "$ref": "#/definitions/chef.automate.api.notifications.WebhookAlert"
        },
        "ServiceNowAlert": {
          "$ref": "#/definitions/chef.automate.api.notifications.ServiceNowAlert"
        }
      }
    },
    "chef.automate.api.notifications.Rule.Event": {
      "type": "string",
      "enum": [
        "CCRFailure",
        "CCRSuccess",
        "ComplianceFailure",
        "ComplianceSuccess"
      ],
      "default": "CCRFailure"
    },
    "chef.automate.api.notifications.RuleAddRequest": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.notifications.Rule"
        }
      }
    },
    "chef.automate.api.notifications.RuleAddResponse": {
      "type": "object",
      "properties": {
        "messages": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "rule": {
          "$ref": "#/definitions/chef.automate.api.notifications.Rule"
        }
      }
    },
    "chef.automate.api.notifications.RuleDeleteResponse": {
      "type": "object",
      "properties": {
        "messages": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.notifications.RuleGetResponse": {
      "type": "object",
      "properties": {
        "messages": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "rule": {
          "$ref": "#/definitions/chef.automate.api.notifications.Rule"
        }
      }
    },
    "chef.automate.api.notifications.RuleListResponse": {
      "type": "object",
      "properties": {
        "messages": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "rules": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.notifications.Rule"
          }
        }
      }
    },
    "chef.automate.api.notifications.RuleUpdateRequest": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.notifications.Rule"
        },
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.notifications.RuleUpdateResponse": {
      "type": "object",
      "properties": {
        "messages": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.notifications.SecretId": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.notifications.ServiceNowAlert": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        },
        "secret_id": {
          "type": "string"
        },
        "critical_controls_only": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.notifications.SlackAlert": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.notifications.URLValidationRequest": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        },
        "username_password": {
          "$ref": "#/definitions/chef.automate.api.notifications.UsernamePassword"
        },
        "secret_id": {
          "$ref": "#/definitions/chef.automate.api.notifications.SecretId"
        },
        "none": {
          "$ref": "#/definitions/chef.automate.api.notifications.Empty"
        }
      }
    },
    "chef.automate.api.notifications.URLValidationResponse": {
      "type": "object"
    },
    "chef.automate.api.notifications.UsernamePassword": {
      "type": "object",
      "properties": {
        "username": {
          "type": "string"
        },
        "password": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.notifications.VersionResponse": {
      "type": "object",
      "properties": {
        "version": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.notifications.WebhookAlert": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
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
