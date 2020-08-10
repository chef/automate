package api

func init() {
	Swagger.Add("job_scheduler", `{
  "swagger": "2.0",
  "info": {
    "title": "external/ingest/job_scheduler.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/retention/nodes/delete-nodes/config": {
      "post": {
        "operationId": "JobScheduler_ConfigureDeleteNodesScheduler",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ConfigureDeleteNodesScheduler"
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
              "$ref": "#/definitions/chef.automate.api.ingest.request.SchedulerConfig"
            }
          }
        ],
        "tags": [
          "JobScheduler"
        ]
      }
    },
    "/api/v0/retention/nodes/missing-nodes-deletion/config": {
      "post": {
        "operationId": "JobScheduler_ConfigureMissingNodesForDeletionScheduler",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ConfigureMissingNodesForDeletionScheduler"
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
              "$ref": "#/definitions/chef.automate.api.ingest.request.SchedulerConfig"
            }
          }
        ],
        "tags": [
          "JobScheduler"
        ]
      }
    },
    "/api/v0/retention/nodes/missing-nodes/config": {
      "post": {
        "operationId": "JobScheduler_ConfigureNodesMissingScheduler",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.ConfigureNodesMissingScheduler"
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
              "$ref": "#/definitions/chef.automate.api.ingest.request.SchedulerConfig"
            }
          }
        ],
        "tags": [
          "JobScheduler"
        ]
      }
    },
    "/api/v0/retention/nodes/status": {
      "get": {
        "operationId": "JobScheduler_GetStatusJobScheduler",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.ingest.response.JobSchedulerStatus"
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
          "JobScheduler"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.ingest.request.SchedulerConfig": {
      "type": "object",
      "properties": {
        "every": {
          "type": "string"
        },
        "threshold": {
          "type": "string"
        },
        "running": {
          "type": "boolean",
          "format": "boolean"
        }
      },
      "description": "SchedulerConfig\nThe job message to configure the Delete Node Job\nevery - It accepts '1h30m', '1m', '2h30m', ..."
    },
    "chef.automate.api.ingest.response.ConfigureDeleteNodesScheduler": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.ConfigureMissingNodesForDeletionScheduler": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.ConfigureNodesMissingScheduler": {
      "type": "object"
    },
    "chef.automate.api.ingest.response.Job": {
      "type": "object",
      "properties": {
        "running": {
          "type": "boolean",
          "format": "boolean"
        },
        "name": {
          "type": "string"
        },
        "every": {
          "type": "string"
        },
        "last_run": {
          "type": "string"
        },
        "next_run": {
          "type": "string"
        },
        "last_elapsed": {
          "type": "string"
        },
        "started_on": {
          "type": "string"
        },
        "threshold": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.ingest.response.JobSchedulerStatus": {
      "type": "object",
      "properties": {
        "running": {
          "type": "boolean",
          "format": "boolean"
        },
        "jobs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.ingest.response.Job"
          }
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
