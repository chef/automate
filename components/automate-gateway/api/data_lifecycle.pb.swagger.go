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
        "summary": "SetComplianceConfig configures the compliance data lifecycle scheduler and jobs",
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
        "summary": "RunCompliance runs the compliance data lifecycle jobs",
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
        "summary": "GetComplianceStatus returns the compliance job scheduler status",
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
        "summary": "SetConfig provides a singular endpoint for configuring all data lifecycle jobs",
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
        "summary": "SetEventFeedConfig configures the event feed data lifecycle scheduler and jobs",
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
        "summary": "RunEventFeed runs the event feed data lifecycle jobs",
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
        "summary": "GetEventFeedStatus returns the event feed job scheduler status",
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
        "summary": "SetInfraConfig configures the infra data lifecycle scheduler and jobs",
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
        "summary": "RunInfra runs the infra data lifecycle jobs",
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
        "summary": "GetInfraStatus returns the infra job scheduler status",
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
        "summary": "Run runs all data lifecycle actions across all data lifecycle jobs",
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
        "summary": "GetStatus returns the aggregate status across all data lifecycle jobs",
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
      },
      "title": "PurgePolicies are data lifecycle purge policies"
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
