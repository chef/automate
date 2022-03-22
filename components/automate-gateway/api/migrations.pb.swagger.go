package api

func init() {
	Swagger.Add("migrations", `{
  "swagger": "2.0",
  "info": {
    "title": "external/infra_proxy/migrations/migrations.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/infra/servers/migrations/staged_data/{migration_id}": {
      "get": {
        "operationId": "InfraProxyMigrationService_GetStagedData",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.response.GetStagedDataResponse"
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
            "name": "migration_id",
            "description": "Migration ID",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxyMigrationService"
        ]
      }
    },
    "/api/v0/infra/servers/migrations/status/{migration_id}": {
      "get": {
        "operationId": "InfraProxyMigrationService_GetMigrationStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.response.GetMigrationStatusResponse"
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
            "name": "migration_id",
            "description": "Migration ID.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxyMigrationService"
        ]
      }
    },
    "/api/v0/infra/servers/{server_id}/migrations/cancel_migration/{migration_id}": {
      "get": {
        "operationId": "InfraProxyMigrationService_CancelMigration",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.response.CancelMigrationResponse"
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
            "name": "server_id",
            "description": "Chef Server ID",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "migration_id",
            "description": "Migration ID",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxyMigrationService"
        ]
      }
    },
    "/api/v0/infra/servers/{server_id}/migrations/confirm_preview/{migration_id}": {
      "post": {
        "operationId": "InfraProxyMigrationService_ConfirmPreview",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.response.ConfirmPreview"
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
            "name": "server_id",
            "description": "Server ID",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "migration_id",
            "description": "Migration ID.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.request.ConfirmPreview"
            }
          }
        ],
        "tags": [
          "InfraProxyMigrationService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.infra_proxy.migrations.request.ConfirmPreview": {
      "type": "object",
      "properties": {
        "server_id": {
          "type": "string",
          "title": "Server ID"
        },
        "migration_id": {
          "type": "string",
          "description": "Migration ID."
        },
        "staged_data": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.request.StagedData",
          "title": "Staged data"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.request.StagedData": {
      "type": "object",
      "properties": {
        "orgs_to_migrate": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to migrate"
        },
        "orgs_to_skip": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to skip"
        },
        "orgs_to_update": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to update"
        },
        "orgs_to_delete": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to delete"
        },
        "users": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.request.User"
          },
          "title": "Users"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.request.User": {
      "type": "object",
      "properties": {
        "username": {
          "type": "string",
          "title": "User's username"
        },
        "email": {
          "type": "string",
          "title": "User's email ID"
        },
        "display_name": {
          "type": "string",
          "title": "User's display name"
        },
        "first_name": {
          "type": "string",
          "title": "User's first name"
        },
        "last_name": {
          "type": "string",
          "title": "User's last name"
        },
        "middle_name": {
          "type": "string",
          "title": "User's middle name"
        },
        "automate_username": {
          "type": "string",
          "title": "User's username in automate"
        },
        "connector": {
          "type": "string",
          "title": "Local or ldap user"
        },
        "is_conflicting": {
          "type": "boolean",
          "format": "boolean",
          "title": "IsConflicting for user's existance in db"
        },
        "is_admin": {
          "type": "boolean",
          "format": "boolean",
          "title": "user is admin or not"
        },
        "hash_password": {
          "type": "string",
          "title": "Local User hash password"
        },
        "action_ops": {
          "type": "integer",
          "format": "int32",
          "title": "Local User actionops"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.response.CancelMigrationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean",
          "title": "migration ID"
        },
        "errors": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "title": "Error message"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.response.ConfirmPreview": {
      "type": "object",
      "properties": {
        "migration_id": {
          "type": "string",
          "title": "Migration ID"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.response.GetMigrationStatusResponse": {
      "type": "object",
      "properties": {
        "migration_id": {
          "type": "string",
          "description": "Migration ID."
        },
        "migration_type": {
          "type": "string",
          "title": "Migration type"
        },
        "migration_status": {
          "type": "string",
          "title": "Migration status"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.response.GetStagedDataResponse": {
      "type": "object",
      "properties": {
        "migration_id": {
          "type": "string",
          "description": "Migration ID."
        },
        "staged_data": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.response.StagedData",
          "title": "Staged data"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.response.StagedData": {
      "type": "object",
      "properties": {
        "orgs_to_migrate": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to migrate"
        },
        "orgs_to_skip": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to skip"
        },
        "orgs_to_update": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to update"
        },
        "orgs_to_delete": {
          "type": "integer",
          "format": "int32",
          "title": "Number of orgs to delete"
        },
        "users": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.migrations.response.User"
          },
          "title": "Users"
        }
      }
    },
    "chef.automate.api.infra_proxy.migrations.response.User": {
      "type": "object",
      "properties": {
        "username": {
          "type": "string",
          "title": "User's username"
        },
        "email": {
          "type": "string",
          "title": "User's email ID"
        },
        "display_name": {
          "type": "string",
          "title": "User's display name"
        },
        "first_name": {
          "type": "string",
          "title": "User's first name"
        },
        "last_name": {
          "type": "string",
          "title": "User's last name"
        },
        "middle_name": {
          "type": "string",
          "title": "User's middle name"
        },
        "automate_username": {
          "type": "string",
          "title": "User's username in automate"
        },
        "connector": {
          "type": "string",
          "title": "Local or ldap user"
        },
        "is_conflicting": {
          "type": "boolean",
          "format": "boolean",
          "title": "IsConflicting for user's existance in db"
        },
        "is_admin": {
          "type": "boolean",
          "format": "boolean",
          "title": "user is admin or not"
        },
        "hash_password": {
          "type": "string",
          "title": "Local user hashed password"
        },
        "action_ops": {
          "type": "integer",
          "format": "int32",
          "title": "Local User actionops"
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
