package api

func init() {
	Swagger.Add("data_lifecycle", `{
  "swagger": "2.0",
  "info": {
    "title": "external/data_lifecycle/data_lifecycle.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/data-lifecycle/compliance/config": {
      "put": {
        "summary": "SetComplianceConfig configures the compliance data lifecycle scheduler and jobs",
        "operationId": "DataLifecycle_SetComplianceConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetComplianceConfigResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetComplianceConfigRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/compliance/run": {
      "post": {
        "summary": "RunCompliance runs the compliance data lifecycle jobs",
        "operationId": "DataLifecycle_RunCompliance",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunComplianceResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunComplianceRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/compliance/status": {
      "get": {
        "summary": "GetComplianceStatus returns the compliance job scheduler status",
        "operationId": "DataLifecycle_GetComplianceStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetComplianceStatusResponse"
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
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/config": {
      "put": {
        "summary": "SetConfig provides a singular endpoint for configuring all data lifecycle jobs",
        "operationId": "DataLifecycle_SetConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetConfigResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetConfigRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/event-feed/config": {
      "put": {
        "summary": "SetEventFeedConfig configures the event feed data lifecycle scheduler and jobs",
        "operationId": "DataLifecycle_SetEventFeedConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetEventFeedConfigResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetEventFeedConfigRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/event-feed/run": {
      "post": {
        "summary": "RunEventFeed runs the event feed data lifecycle jobs",
        "operationId": "DataLifecycle_RunEventFeed",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunEventFeedResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunEventFeedRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/event-feed/status": {
      "get": {
        "summary": "GetEventFeedStatus returns the event feed job scheduler status",
        "operationId": "DataLifecycle_GetEventFeedStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetEventFeedStatusResponse"
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
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/infra/config": {
      "put": {
        "summary": "SetInfraConfig configures the infra data lifecycle scheduler and jobs",
        "operationId": "DataLifecycle_SetInfraConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetInfraConfigResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetInfraConfigRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/infra/run": {
      "post": {
        "summary": "RunInfra runs the infra data lifecycle jobs",
        "operationId": "DataLifecycle_RunInfra",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunInfraResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunInfraRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/infra/status": {
      "get": {
        "summary": "GetInfraStatus returns the infra job scheduler status",
        "operationId": "DataLifecycle_GetInfraStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetInfraStatusResponse"
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
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/run": {
      "post": {
        "summary": "Run runs all data lifecycle actions across all data lifecycle jobs",
        "operationId": "DataLifecycle_Run",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/services/config": {
      "put": {
        "operationId": "DataLifecycle_SetServicesConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetServicesConfigResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetServicesConfigRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/services/run": {
      "post": {
        "operationId": "DataLifecycle_RunServices",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunServicesResponse"
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
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunServicesRequest"
            }
          }
        ],
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/services/status": {
      "get": {
        "summary": "Services",
        "operationId": "DataLifecycle_GetServicesStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetServicesStatusResponse"
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
          "DataLifecycle"
        ]
      }
    },
    "/api/v0/data-lifecycle/status": {
      "get": {
        "summary": "GetStatus returns the aggregate status across all data lifecycle jobs",
        "operationId": "DataLifecycle_GetStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetStatusResponse"
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
          "DataLifecycle"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.data_lifecycle.EsPolicy": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "index": {
          "type": "string"
        },
        "older_than_days": {
          "type": "integer",
          "format": "int32"
        },
        "custom_purge_field": {
          "type": "string"
        },
        "disabled": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.data_lifecycle.EsPolicyUpdate": {
      "type": "object",
      "properties": {
        "policy_name": {
          "type": "string"
        },
        "disabled": {
          "type": "boolean",
          "format": "boolean"
        },
        "older_than_days": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.data_lifecycle.GetComplianceStatusResponse": {
      "type": "object",
      "properties": {
        "jobs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobStatus"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.GetEventFeedStatusResponse": {
      "type": "object",
      "properties": {
        "jobs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobStatus"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.GetInfraStatusResponse": {
      "type": "object",
      "properties": {
        "jobs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobStatus"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.GetServicesStatusResponse": {
      "type": "object",
      "properties": {
        "jobs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobStatus"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.GetStatusResponse": {
      "type": "object",
      "properties": {
        "infra": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetInfraStatusResponse"
        },
        "compliance": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetComplianceStatusResponse"
        },
        "event_feed": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetEventFeedStatusResponse"
        },
        "services": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetServicesStatusResponse"
        }
      }
    },
    "chef.automate.api.data_lifecycle.JobSettings": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "disabled": {
          "type": "boolean",
          "format": "boolean"
        },
        "recurrence": {
          "type": "string"
        },
        "threshold": {
          "type": "string"
        },
        "purge_policies": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.PurgePolicyUpdate"
        }
      },
      "title": "JobSettings are a job configuration setting update"
    },
    "chef.automate.api.data_lifecycle.JobStatus": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "disabled": {
          "type": "boolean",
          "format": "boolean"
        },
        "recurrence": {
          "type": "string"
        },
        "threshold": {
          "type": "string"
        },
        "purge_policies": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.PurgePolicies"
        },
        "last_elapsed": {
          "type": "string"
        },
        "next_due_at": {
          "type": "string",
          "format": "date-time"
        },
        "last_enqueued_at": {
          "type": "string",
          "format": "date-time"
        },
        "last_started_at": {
          "type": "string",
          "format": "date-time"
        },
        "last_ended_at": {
          "type": "string",
          "format": "date-time"
        }
      },
      "description": "JobStatus presents the current configuration of job, when it will be executed,\nand details about it's most recent execution."
    },
    "chef.automate.api.data_lifecycle.PgPolicy": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "disabled": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.data_lifecycle.PgPolicyUpdate": {
      "type": "object",
      "properties": {
        "policy_name": {
          "type": "string"
        },
        "disabled": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.data_lifecycle.PurgePolicies": {
      "type": "object",
      "properties": {
        "elasticsearch": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.EsPolicy"
          }
        },
        "postgres": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.PgPolicy"
          }
        }
      },
      "title": "PurgePolicies are data lifecycle purge policies"
    },
    "chef.automate.api.data_lifecycle.PurgePolicyUpdate": {
      "type": "object",
      "properties": {
        "elasticsearch": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.EsPolicyUpdate"
          }
        },
        "postgres": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.PgPolicyUpdate"
          }
        }
      },
      "title": "PurgePolicyUpdate is purge policy configuration update"
    },
    "chef.automate.api.data_lifecycle.RunComplianceRequest": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunComplianceResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunEventFeedRequest": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunEventFeedResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunInfraRequest": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunInfraResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunRequest": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunServicesRequest": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.RunServicesResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.SetComplianceConfigRequest": {
      "type": "object",
      "properties": {
        "job_settings": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobSettings"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.SetComplianceConfigResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.SetConfigRequest": {
      "type": "object",
      "properties": {
        "infra": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetInfraConfigRequest"
        },
        "compliance": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetComplianceConfigRequest"
        },
        "event_feed": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetEventFeedConfigRequest"
        },
        "services": {
          "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetServicesConfigRequest"
        }
      }
    },
    "chef.automate.api.data_lifecycle.SetConfigResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.SetEventFeedConfigRequest": {
      "type": "object",
      "properties": {
        "job_settings": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobSettings"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.SetEventFeedConfigResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.SetInfraConfigRequest": {
      "type": "object",
      "properties": {
        "job_settings": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobSettings"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.SetInfraConfigResponse": {
      "type": "object"
    },
    "chef.automate.api.data_lifecycle.SetServicesConfigRequest": {
      "type": "object",
      "properties": {
        "job_settings": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.data_lifecycle.JobSettings"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.SetServicesConfigResponse": {
      "type": "object"
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
