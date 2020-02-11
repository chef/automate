package api

func init() {
	Swagger.Add("applications", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/applications/applications.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/applications/delete_disconnected_services": {
      "post": {
        "summary": "Remove Disconnected Services",
        "description": "Removes services marked as disconnected based on the ` + "`" + `threshold_seconds` + "`" + ` setting.\nThis function is not used by the API or CLI and is here for testing purposes.\nThe functionality is currently covered by a periodically running job that can be configured using ` + "`" + `UpdateDeleteDisconnectedServicesConfig` + "`" + `.",
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
          "service_groups"
        ]
      }
    },
    "/applications/delete_services_by_id": {
      "post": {
        "summary": "Delete the services with the given IDs",
        "operationId": "DeleteServicesByID",
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
              "$ref": "#/definitions/chef.automate.api.applications.DeleteServicesByIDReq"
            }
          }
        ],
        "tags": [
          "service_groups"
        ]
      }
    },
    "/applications/disconnected_services": {
      "get": {
        "summary": "Mark Services as Disconnected",
        "description": "Marks services as disconnected based on the ` + "`" + `threshold_seconds` + "`" + ` setting.\nThis function is not used by the API or CLI and is here for testing purposes.\nThe functionality is currently covered by a periodically running job that can be configured\nby utilizing the ` + "`" + `UpdateDisconnectedServicesConfig` + "`" + ` endpoint.",
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
            "description": "Threshold for marking services disconnected in seconds.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          }
        ],
        "tags": [
          "service_groups"
        ]
      }
    },
    "/applications/service-groups": {
      "get": {
        "summary": "List Service Groups",
        "description": "Lists service groups with name, health information, and application, environment, package, release metadata.\nAccepts pagination, sorting, search and status filters.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\napplications/service-groups?sorting.field=percent_ok\u0026sorting.order=ASC\u0026pagination.page=1\u0026pagination.size=25\n` + "`" + `` + "`" + `` + "`" + `",
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
            "description": "Applies search and status filters, in the format of ` + "`" + `fieldname:value` + "`" + ` or ` + "`" + `status:value` + "`" + `.\n\nValid filter fieldnames are:\n* ` + "`" + `origin` + "`" + `: origin component of the service's package identifier\n* ` + "`" + `service` + "`" + `: the name component of the service's package identifier\n* ` + "`" + `version` + "`" + `: the version number component of the service's package identifier\n* ` + "`" + `buildstamp` + "`" + `: the build timestamp (also called \"release\") of the service's package identifier\n* ` + "`" + `channel` + "`" + `: the package channel to which the service subscribes for updates\n* ` + "`" + `application` + "`" + `: the application field of the service's event-stream metadata\n* ` + "`" + `environment` + "`" + `: the environment field of the service's event-stream metadata\n* ` + "`" + `site` + "`" + `: the site field of the service's event-stream metadata\n* ` + "`" + `group` + "`" + `: the suffix of the service group name\n\nService groups may also be filtered by ` + "`" + `status` + "`" + `, which refers to a service's\nconnected/disconnected state or it's most recent healthcheck result. Valid\nstatus filter parameters are:\n* ` + "`" + `status:disconnected` + "`" + `: only return service groups that contain at least\n  one service in the disconnected state\n* ` + "`" + `status:critical` + "`" + `: only return service groups that contain at least one\n  service that is returning a \"critical\" healthcheck result\n* ` + "`" + `status:critical` + "`" + `: only return service groups that contain at least one\n  service that is returning a \"critical\" healthcheck result\n* ` + "`" + `status:unknown` + "`" + `: only return service groups that contain at least one\n  service that is returning an \"unknown\" healthcheck result and no\n  services returning \"critical\" results\n* ` + "`" + `status:warning` + "`" + `: only return service groups that contain at least one\n  service that is returning a \"warning\" healthcheck result and have no\n  services returning \"critical\" or \"unknown\" results\n* ` + "`" + `status:ok` + "`" + `: only return service groups where all services are returning\n  \"ok\" health check results",
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
            "description": "Page number of the results to return.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "description": "Amount of results to include per page.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "description": "Field to sort the list results on.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "description": "Order the results should be returned in.",
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
          "service_groups"
        ]
      }
    },
    "/applications/service-groups/{service_group_id}": {
      "get": {
        "summary": "List Services for a Service Group",
        "description": "List the services for a service group with health status and service metadata.\nUses the service group ID generated by Chef Automate instead of the Chef Habitat- provided ID.\nSupports pagination and filtering.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\napplications/service-groups/1dfff679054c60a10c51d059b6dbf81a765c46f8d3e8ce0752b22ffe8d4d9716?pagination.page=1\u0026pagination.size=25\n` + "`" + `` + "`" + `` + "`" + `",
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
            "description": "Service group ID.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "pagination.page",
            "description": "Page number of the results to return.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "description": "Amount of results to include per page.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "description": "Field to sort the list results on.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "description": "Order the results should be returned in.",
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
            "name": "filter",
            "description": "Applies filters, in the format of ` + "`" + `fieldname:value` + "`" + `.\nSee documentation for ServicesReq for valid filter parameters.",
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
          "service_groups"
        ]
      }
    },
    "/applications/service_groups_health_counts": {
      "get": {
        "summary": "List Service Groups Health Counts",
        "description": "Lists the total service group health reports by critical, warning, ok and unknown responses. Supports search and status filtering.",
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
            "description": "Applies search filters, in the format of ` + "`" + `fieldname:value` + "`" + `.\nSee the documentation for ServiceGroupsReq for valid filter parameters.",
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
          "service_groups"
        ]
      }
    },
    "/applications/services": {
      "get": {
        "summary": "List Services",
        "description": "Lists service health status and service metadata for services.\nSupports pagination and search and status filtering. For a list of services for a specific service-group see \"List Services for a Service Group\" (GetServicesBySG endpoint).",
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
            "description": "Applies search filters, in the format of ` + "`" + `fieldname:value` + "`" + `.\n\nValid filter fieldnames are:\n* ` + "`" + `origin` + "`" + `: origin component of the service's package identifier\n* ` + "`" + `service` + "`" + `: the name component of the service's package identifier\n* ` + "`" + `version` + "`" + `: the version number component of the service's package identifier\n* ` + "`" + `buildstamp` + "`" + `: the build timestamp (also called \"release\") of the service's package identifier\n* ` + "`" + `channel` + "`" + `: the package channel to which the service subscribes for updates\n* ` + "`" + `application` + "`" + `: the application field of the service's event-stream metadata\n* ` + "`" + `environment` + "`" + `: the environment field of the service's event-stream metadata\n* ` + "`" + `site` + "`" + `: the site field of the service's event-stream metadata\n* ` + "`" + `group` + "`" + `: the suffix of the service group name\n\nServices may also be filtered by ` + "`" + `status` + "`" + `, which refers to a service's\nconnected/disconnected state or it's most recent healthcheck result. Valid\nstatus filter parameters are:\n* ` + "`" + `status:disconnected` + "`" + `: only return services in the disconnected state\n* ` + "`" + `status:critical` + "`" + `: only return services that are returning a \"critical\"\n  healthcheck result\n* ` + "`" + `status:unknown` + "`" + `: only return services that are returning an \"unknown\"\n  healthcheck result\n* ` + "`" + `status:warning` + "`" + `: only return services that are returning a \"warning\"\n  healthcheck result\n* ` + "`" + `status:ok` + "`" + `: only return services that are returning \"ok\" health check\n  results",
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
            "description": "Page number of the results to return.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "pagination.size",
            "description": "Amount of results to include per page.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "sorting.field",
            "description": "Field to sort the list results on.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "sorting.order",
            "description": "Order the results should be returned in.",
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
          "service_groups"
        ]
      }
    },
    "/applications/services-distinct-values": {
      "get": {
        "summary": "List Filter Values",
        "description": "Lists all of the possible filter values for a given valid field.\nLimit the returned values by providing at one or more characters in the ` + "`" + `query_fragment` + "`" + ` parameter.\nSupports wildcard (* and ?)",
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
            "description": "Field name of service values.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "query_fragment",
            "description": "Query value, supports wildcards (* and ?).",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "filter",
            "description": "Applies filters, in the format of ` + "`" + `fieldname:value` + "`" + `.\nSee documentation for ServicesReq for valid filter parameters.",
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
          "service_groups"
        ]
      }
    },
    "/applications/stats": {
      "get": {
        "summary": "Show Summary",
        "description": "Shows a summary of service-groups, services, deployments and supervisors.\nUsed for telemetry.\nDoes not support filtering.",
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
          "service_groups"
        ]
      }
    },
    "/applications/version": {
      "get": {
        "summary": "Show Version",
        "description": "Displays the current version of the applications-service",
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
          "hidden"
        ]
      }
    },
    "/retention/service_groups/delete_disconnected_services/config": {
      "get": {
        "summary": "Show 'Remove Disconnected Services' Configuration",
        "description": "Displays configuration for the task that deletes services marked as disconnected\nafter 'threshold'. Threshold is a string that follows Elasticsearch's date math expressions.\nThis job is disabled if running is set to false.",
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
          "retention"
        ]
      },
      "post": {
        "summary": "Change 'Remove Disconnected Services' Configuration",
        "description": "Updates configuration information for the task that deletes services marked as disconnected\nafter 'threshold'. Threshold is a string that follows Elasticsearch's date math expressions.\nThis job can be disabled by setting ` + "`" + `\"running\": false` + "`" + `.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\nservice_groups/delete_disconnected_services/config\" -d\n'{\n\"threshold\": \"1d\",\n\"running\":true\n}'\n` + "`" + `` + "`" + `` + "`" + `",
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
          "retention"
        ]
      }
    },
    "/retention/service_groups/disconnected_services/config": {
      "get": {
        "summary": "Show 'Disconnected Services' configuration",
        "description": "Returns the configuration for the task that marks services as disconnected. The ` + "`" + `threshold` + "`" + ` setting defines the period of time between the last report from a node and the moment when Chef Automate marks it as disconnected. ` + "`" + `Threshold` + "`" + ` is a string that follows Elasticsearch's date math expressions.\nThis task is always enabled, cannot be disabled. Because this task runs continuously, the response does not return information about its status.",
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
          "retention"
        ]
      },
      "post": {
        "summary": "Change 'Disconnected Services' Configuration",
        "description": "Changes the configuration for the task that marks services as disconnected after\n'threshold'. Threshold is a string that follows Elasticsearch's date math expressions.\nThis job cannot be disabled, and therefore no information about running is accepted.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n/retention/service_groups/disconnected_services/config\n'{\n\"threshold\": \"15m\"\n}'\n` + "`" + `` + "`" + `` + "`" + `",
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
          "retention"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.applications.DeleteServicesByIDReq": {
      "type": "object",
      "properties": {
        "ids": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of the database IDs of the services to be deleted."
        }
      }
    },
    "chef.automate.api.applications.DisconnectedServicesReq": {
      "type": "object",
      "properties": {
        "threshold_seconds": {
          "type": "integer",
          "format": "int32",
          "description": "Threshold for marking services disconnected in seconds."
        }
      },
      "description": "Request message for GetDisconnectedServices."
    },
    "chef.automate.api.applications.HealthCheckResult": {
      "type": "object",
      "properties": {
        "stdout": {
          "type": "string"
        },
        "stderr": {
          "type": "string"
        },
        "exit_status": {
          "type": "integer",
          "format": "int32"
        }
      },
      "title": "HealthCheckResult aggregates the stdout output, stderr output and process\nexit status of a habitat health check"
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
      },
      "description": "Combined count values from the health status and disconnected status reports."
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
      "description": "The HealthStatus enumerable matches the Chef Habitat implementation for health-check status:\n=\u003e https://www.habitat.sh/docs/reference/#health-check\nFor a health status within a service group.\n*critical* means that one or more services are in critical condition.\n*warning* means that one or more services have a warning, but none are in critical condition.\n*unknown* means that one or more services have not responded, but all of the remaining nodes responded to the health check as \"OK\".\n*OK* means that all of the services are OK and all have responded to the health check.\n*none* means that there is no health check information."
    },
    "chef.automate.api.applications.PeriodicJobConfig": {
      "type": "object",
      "properties": {
        "running": {
          "type": "boolean",
          "format": "boolean",
          "description": "The job status? ` + "`" + `false` + "`" + ` is disabled, ` + "`" + `true` + "`" + ` is enabled."
        },
        "threshold": {
          "type": "string",
          "description": "The ` + "`" + `threshold` + "`" + ` setting used by periodic jobs for evaluating services.\nThreshold is a string that follows Elasticsearch's date math expressions. For more information, see the simpledatemath package under ` + "`" + `lib/` + "`" + `."
        }
      },
      "description": "Periodic job configuration."
    },
    "chef.automate.api.applications.PeriodicMandatoryJobConfig": {
      "type": "object",
      "properties": {
        "threshold": {
          "type": "string",
          "description": "The ` + "`" + `threshold` + "`" + ` setting used by periodic jobs for evaluating services.\nThreshold is a string that follows Elasticsearch's date math expressions. For more information, see the simpledatemath package under ` + "`" + `lib/` + "`" + `."
        }
      },
      "description": "Configuration for the mandatory periodic job.\nRequired and cannot be disabled."
    },
    "chef.automate.api.applications.Service": {
      "type": "object",
      "properties": {
        "supervisor_id": {
          "type": "string",
          "description": "The Chef Habitat Supervisor ID."
        },
        "release": {
          "type": "string",
          "description": "Combination of the service version and release in a single string.\nExample: 0.1.0/8743278934278923."
        },
        "group": {
          "type": "string",
          "description": "Service group name."
        },
        "health_check": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthStatus",
          "description": "Intentionally blank."
        },
        "application": {
          "type": "string",
          "description": "Application name."
        },
        "environment": {
          "type": "string",
          "description": "Environment name."
        },
        "fqdn": {
          "type": "string",
          "description": "FQDN reported by a Chef Habitat Supervisor."
        },
        "channel": {
          "type": "string",
          "description": "Chef Habitat channel that the service is subscribed to."
        },
        "update_strategy": {
          "type": "string",
          "description": "Update strategy that the service employs."
        },
        "site": {
          "type": "string",
          "description": "Site reported by Chef Habitat service, a user defined flag."
        },
        "previous_health_check": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthStatus",
          "description": "Intentionally blank."
        },
        "current_health_since": {
          "type": "string",
          "description": "Time interval of current health status from last status change until now."
        },
        "health_updated_at": {
          "type": "string",
          "format": "date-time",
          "description": "Timestamp since health status change."
        },
        "disconnected": {
          "type": "boolean",
          "format": "boolean",
          "description": "Service connection information.\nBased on time since last healthcheck received and disconnected service configuration."
        },
        "last_event_occurred_at": {
          "type": "string",
          "format": "date-time",
          "description": "Timestamp of last received health check message."
        },
        "last_event_since": {
          "type": "string",
          "description": "Interval since last event received until now."
        },
        "health_check_result": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthCheckResult",
          "description": "Intentionally blank."
        },
        "id": {
          "type": "string",
          "title": "Internal ID"
        }
      }
    },
    "chef.automate.api.applications.ServiceGroup": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of service group."
        },
        "release": {
          "type": "string",
          "description": "Combination of the version and release in a single string.\nExample: 0.1.0/8743278934278923."
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthStatus",
          "description": "Intentionally blank."
        },
        "health_percentage": {
          "type": "integer",
          "format": "int32",
          "description": "Percentage of services reporting OK status.\nThe health_percentage can be a number between 0-100."
        },
        "services_health_counts": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthCounts",
          "description": "Intentionally blank."
        },
        "id": {
          "type": "string",
          "description": "Service group ID. This is a value constructed by Chef Automate and is not reported by Chef Habitat."
        },
        "application": {
          "type": "string",
          "description": "Application name for the service group."
        },
        "environment": {
          "type": "string",
          "description": "Environment name for the service group."
        },
        "package": {
          "type": "string",
          "description": "Combination of the origin and package name in a single string.\nExample: core/redis."
        },
        "disconnected_count": {
          "type": "integer",
          "format": "int32",
          "description": "Count of disconnected services within this service group."
        }
      },
      "description": "A service group message is the representation of an individual service group that\nis internally generated by aggregating all of its services."
    },
    "chef.automate.api.applications.ServiceGroups": {
      "type": "object",
      "properties": {
        "service_groups": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.applications.ServiceGroup"
          },
          "description": "List of service groups."
        }
      },
      "description": "List of service groups."
    },
    "chef.automate.api.applications.ServicesBySGRes": {
      "type": "object",
      "properties": {
        "group": {
          "type": "string",
          "description": "Service group name."
        },
        "services": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.applications.Service"
          },
          "description": "List of services."
        },
        "services_health_counts": {
          "$ref": "#/definitions/chef.automate.api.applications.HealthCounts",
          "description": "Intentionally blank."
        }
      },
      "description": "Response message for GetServicesBySG."
    },
    "chef.automate.api.applications.ServicesDistinctValuesRes": {
      "type": "object",
      "properties": {
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of distinct values fitting query_fragment and filters."
        }
      },
      "description": "Response message for GetServicesDistinctValues."
    },
    "chef.automate.api.applications.ServicesRes": {
      "type": "object",
      "properties": {
        "services": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.applications.Service"
          },
          "description": "List of services."
        }
      },
      "description": "Response message for GetServices."
    },
    "chef.automate.api.applications.ServicesStatsRes": {
      "type": "object",
      "properties": {
        "total_service_groups": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of service groups reporting to Chef Automate."
        },
        "total_services": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of services reporting to Chef Automate, counts both connected and disconnected services."
        },
        "total_supervisors": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of supervisors reporting to Chef Automate."
        },
        "total_deployments": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of deployments reporting to Chef Automate."
        }
      },
      "description": "Response message for ServicesStats."
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
          "format": "int32",
          "description": "Page number of the results to return."
        },
        "size": {
          "type": "integer",
          "format": "int32",
          "description": "Amount of results to include per page."
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
          "type": "string",
          "description": "Field to sort the list results on."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.common.query.SortOrder",
          "description": "Order the results should be returned in."
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
