package api

func init() {
	Swagger.Add("data_lifecycle", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/data_lifecycle/data_lifecycle.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/data-lifecycle/compliance/config": {
      "put": {
        "operationId": "SetComplianceConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetComplianceConfigResponse"
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
    "/data-lifecycle/compliance/run": {
      "post": {
        "operationId": "RunCompliance",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunComplianceResponse"
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
    "/data-lifecycle/compliance/status": {
      "get": {
        "summary": "Compliance",
        "operationId": "GetComplianceStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetComplianceStatusResponse"
            }
          }
        },
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/data-lifecycle/config": {
      "put": {
        "summary": "SetConfig provides a singular endpoint for confiuging all data lifecycle handlers",
        "operationId": "SetConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetConfigResponse"
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
    "/data-lifecycle/event-feed/config": {
      "put": {
        "operationId": "SetEventFeedConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetEventFeedConfigResponse"
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
    "/data-lifecycle/event-feed/run": {
      "post": {
        "operationId": "RunEventFeed",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunEventFeedResponse"
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
    "/data-lifecycle/event-feed/status": {
      "get": {
        "summary": "Event Feed",
        "operationId": "GetEventFeedStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetEventFeedStatusResponse"
            }
          }
        },
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/data-lifecycle/infra/config": {
      "put": {
        "summary": "SetInfraConfig configures the infra data lifecycle handler",
        "operationId": "SetInfraConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetInfraConfigResponse"
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
    "/data-lifecycle/infra/run": {
      "post": {
        "summary": "RunInfra runs the infra data lifecycle operations",
        "operationId": "RunInfra",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunInfraResponse"
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
    "/data-lifecycle/infra/status": {
      "get": {
        "summary": "GetInfraStatus returns the infra handler status",
        "operationId": "GetInfraStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetInfraStatusResponse"
            }
          }
        },
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/data-lifecycle/run": {
      "post": {
        "summary": "Run runs all data lifecycle actions across all data lifecycle handlers",
        "operationId": "Run",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunResponse"
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
    "/data-lifecycle/services/config": {
      "put": {
        "operationId": "SetServicesConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.SetServicesConfigResponse"
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
    "/data-lifecycle/services/run": {
      "post": {
        "operationId": "RunServices",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.RunServicesResponse"
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
    "/data-lifecycle/services/status": {
      "get": {
        "summary": "Services",
        "operationId": "GetServicesStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetServicesStatusResponse"
            }
          }
        },
        "tags": [
          "DataLifecycle"
        ]
      }
    },
    "/data-lifecycle/status": {
      "get": {
        "summary": "GetStatus returns the aggregate status across all data lifecycle handlers.",
        "operationId": "GetStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.data_lifecycle.GetStatusResponse"
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
      }
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
      }
    },
    "chef.automate.api.data_lifecycle.PurgePolicies": {
      "type": "object",
      "properties": {
        "elasticsearch": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.infra.data_lifecycle.api.EsPolicy"
          }
        },
        "postgres": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.infra.data_lifecycle.api.PgPolicy"
          }
        }
      }
    },
    "chef.automate.api.data_lifecycle.PurgePolicyUpdate": {
      "type": "object",
      "properties": {
        "elasticsearch": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.infra.data_lifecycle.api.EsPolicyUpdate"
          }
        },
        "postgres": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.infra.data_lifecycle.api.PgPolicyUpdate"
          }
        }
      }
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
    "chef.automate.infra.data_lifecycle.api.EsPolicy": {
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
    "chef.automate.infra.data_lifecycle.api.EsPolicyUpdate": {
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
    "chef.automate.infra.data_lifecycle.api.PgPolicy": {
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
    "chef.automate.infra.data_lifecycle.api.PgPolicyUpdate": {
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
    }
  }
}
`)
}
