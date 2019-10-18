package api

func init() {
	Swagger.Add("applications", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/applications/applications.proto",
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
    "/beta/applications/delete_disconnected_services": {
      "post": {
        "operationId": "DeleteDisconnectedServices",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServicesRes"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.DisconnectedServicesReq"
            }
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/disconnected_services": {
      "get": {
        "operationId": "GetDisconnectedServices",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServicesRes"
            }
          }
        },
        "parameters": [
          {
            "name": "threshold_seconds",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/service-groups": {
      "get": {
        "operationId": "GetServiceGroups",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServiceGroups"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "pagination.page",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "in": "query",
            "required": false,
            "type": "string",
            "enum": [
              "ASC",
              "DESC"
            ],
            "default": "ASC"
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/service-groups/{service_group_id}": {
      "get": {
        "operationId": "GetServicesBySG",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServicesBySGRes"
            }
          }
        },
        "parameters": [
          {
            "name": "service_group_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "pagination.page",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "in": "query",
            "required": false,
            "type": "string",
            "enum": [
              "ASC",
              "DESC"
            ],
            "default": "ASC"
          },
          {
            "name": "health",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/service_groups_health_counts": {
      "get": {
        "operationId": "GetServiceGroupsHealthCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.HealthCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/services": {
      "get": {
        "operationId": "GetServices",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServicesRes"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "pagination.page",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "in": "query",
            "required": false,
            "type": "string",
            "enum": [
              "ASC",
              "DESC"
            ],
            "default": "ASC"
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/services-distinct-values": {
      "get": {
        "operationId": "GetServicesDistinctValues",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServicesDistinctValuesRes"
            }
          }
        },
        "parameters": [
          {
            "name": "field_name",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "query_fragment",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/stats": {
      "get": {
        "operationId": "GetServicesStats",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.ServicesStatsRes"
            }
          }
        },
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/applications/version": {
      "get": {
        "operationId": "GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.common.version.VersionInfo"
            }
          }
        },
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/retention/service_groups/delete_disconnected_services/config": {
      "get": {
        "operationId": "GetDeleteDisconnectedServicesConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.PeriodicJobConfig"
            }
          }
        },
        "tags": [
          "ApplicationsService"
        ]
      },
      "post": {
        "operationId": "UpdateDeleteDisconnectedServicesConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.UpdateDeleteDisconnectedServicesConfigRes"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.PeriodicJobConfig"
            }
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    },
    "/beta/retention/service_groups/disconnected_services/config": {
      "get": {
        "operationId": "GetDisconnectedServicesConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.PeriodicMandatoryJobConfig"
            }
          }
        },
        "tags": [
          "ApplicationsService"
        ]
      },
      "post": {
        "operationId": "UpdateDisconnectedServicesConfig",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.UpdateDisconnectedServicesConfigRes"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.applications.PeriodicMandatoryJobConfig"
            }
          }
        ],
        "tags": [
          "ApplicationsService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.applications.DisconnectedServicesReq": {
      "type": "object",
      "properties": {
        "threshold_seconds": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.applications.HealthCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "integer",
          "format": "int32"
        },
        "ok": {
          "type": "integer",
          "format": "int32"
        },
        "warning": {
          "type": "integer",
          "format": "int32"
        },
        "critical": {
          "type": "integer",
          "format": "int32"
        },
        "unknown": {
          "type": "integer",
          "format": "int32"
        },
        "disconnected": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.applications.HealthStatus": {
      "type": "string",
      "enum": [
        "OK",
        "WARNING",
        "CRITICAL",
        "UNKNOWN",
        "NONE"
      ],
      "default": "OK",
      "description": "- NONE: The representation of NO health check status\nTODO @afiune how much effort would be to change\nthe OK enum to be NONE",
      "title": "The HealthStatus enum matches the habitat implementation for health-check status:\n=\u003e https://www.habitat.sh/docs/reference/#health-check"
    },
    "chef.automate.api.applications.PeriodicJobConfig": {
      "type": "object",
      "properties": {
        "running": {
          "type": "boolean",
          "format": "boolean"
        },
        "threshold": {
          "type": "string",
          "title": "To match the ingest API at /retention/nodes/missing-nodes/config, we use a\nstring format that is a subset of elasticsearch's date math. See the\nsimpledatemath package under lib/ for more details"
        }
      }
    },
    "chef.automate.api.applications.PeriodicMandatoryJobConfig": {
      "type": "object",
      "properties": {
        "threshold": {
          "type": "string",
          "title": "To match the ingest API at /retention/nodes/missing-nodes/config, we use a\nstring format that is a subset of elasticsearch's date math. See the\nsimpledatemath package under lib/ for more details"
        }
      },
      "title": "it's like a PeriodicJobConfig but the user isn't allowed to change whether\nor not the job runs"
    },
    "chef.automate.api.applications.Service": {
      "type": "object",
      "properties": {
        "supervisor_id": {
          "type": "string"
        },
        "release": {
          "type": "string"
        },
        "group": {
          "type": "string"
        },
        "health_check": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthStatus"
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.applications.ServiceStatus"
        },
        "application": {
          "type": "string"
        },
        "environment": {
          "type": "string"
        },
        "fqdn": {
          "type": "string"
        },
        "channel": {
          "type": "string"
        },
        "update_strategy": {
          "type": "string"
        },
        "site": {
          "type": "string"
        },
        "previous_health_check": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthStatus"
        },
        "current_health_since": {
          "type": "string"
        },
        "health_updated_at": {
          "type": "string",
          "format": "date-time"
        },
        "disconnected": {
          "type": "boolean",
          "format": "boolean"
        },
        "last_event_occurred_at": {
          "type": "string",
          "format": "date-time"
        },
        "last_event_since": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.applications.ServiceGroup": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "release": {
          "type": "string",
          "title": "Combination of the version and release in a single string like:\nExample: 0.1.0/8743278934278923"
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthStatus"
        },
        "health_percentage": {
          "type": "integer",
          "format": "int32",
          "title": "The health_percentage can be a number between 0-100"
        },
        "services_health_counts": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthCounts"
        },
        "id": {
          "type": "string"
        },
        "application": {
          "type": "string"
        },
        "environment": {
          "type": "string"
        },
        "package": {
          "type": "string",
          "title": "Combination of the origin and package name in a single string like:\nExample: core/redis"
        },
        "disconnected_count": {
          "type": "integer",
          "format": "int32"
        }
      },
      "title": "A service group message is the representation of one single service group that\nis internally generated by aggregating all the services"
    },
    "chef.automate.api.applications.ServiceGroups": {
      "type": "object",
      "properties": {
        "service_groups": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.applications.ServiceGroup"
          }
        }
      }
    },
    "chef.automate.api.applications.ServiceStatus": {
      "type": "string",
      "enum": [
        "RUNNING",
        "INITIALIZING",
        "DEPLOYING",
        "DOWN"
      ],
      "default": "RUNNING",
      "title": "The ServiceStatus enum describes the status of the service\n@afiune have we defined these states somewhere?"
    },
    "chef.automate.api.applications.ServicesBySGRes": {
      "type": "object",
      "properties": {
        "group": {
          "type": "string"
        },
        "services": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.applications.Service"
          }
        },
        "services_health_counts": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthCounts"
        }
      }
    },
    "chef.automate.api.applications.ServicesDistinctValuesRes": {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.applications.ServicesRes": {
      "type": "object",
      "properties": {
        "services": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.applications.Service"
          }
        }
      }
    },
    "chef.automate.api.applications.ServicesStatsRes": {
      "type": "object",
      "properties": {
        "total_service_groups": {
          "type": "integer",
          "format": "int32"
        },
        "total_services": {
          "type": "integer",
          "format": "int32"
        },
        "total_supervisors": {
          "type": "integer",
          "format": "int32"
        },
        "total_deployments": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.applications.UpdateDeleteDisconnectedServicesConfigRes": {
      "type": "object"
    },
    "chef.automate.api.applications.UpdateDisconnectedServicesConfigRes": {
      "type": "object"
    },
    "chef.automate.api.common.query.Pagination": {
      "type": "object",
      "properties": {
        "page": {
          "type": "integer",
          "format": "int32"
        },
        "size": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.common.query.SortOrder": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
    "chef.automate.api.common.query.Sorting": {
      "type": "object",
      "properties": {
        "field": {
          "type": "string"
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.common.query.SortOrder"
        }
      }
    },
    "chef.automate.api.common.version.VersionInfo": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "sha": {
          "type": "string"
        },
        "built": {
          "type": "string"
        }
      }
    }
  }
}
`)
}
