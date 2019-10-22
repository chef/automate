package api

func init() {
	Swagger.Add("compliance_scanner_jobs_jobs", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/compliance/scanner/jobs/jobs.proto",
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
    "/compliance/scanner/jobs": {
      "post": {
        "operationId": "Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Id"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Job"
            }
          }
        ],
        "tags": [
          "JobsService"
        ]
      }
    },
    "/compliance/scanner/jobs/id/{id}": {
      "get": {
        "operationId": "Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Job"
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
          "JobsService"
        ]
      },
      "delete": {
        "operationId": "Delete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
          "JobsService"
        ]
      },
      "put": {
        "operationId": "Update",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Job"
            }
          }
        ],
        "tags": [
          "JobsService"
        ]
      }
    },
    "/compliance/scanner/jobs/rerun/id/{id}": {
      "get": {
        "operationId": "Rerun",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.RerunResponse"
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
          "JobsService"
        ]
      }
    },
    "/compliance/scanner/jobs/search": {
      "post": {
        "operationId": "List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Jobs"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Query"
            }
          }
        ],
        "tags": [
          "JobsService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.compliance.scanner.jobs.v1.Id": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.compliance.scanner.jobs.v1.Job": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "type": {
          "type": "string"
        },
        "timeout": {
          "type": "integer",
          "format": "int32"
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Kv"
          }
        },
        "start_time": {
          "type": "string",
          "format": "date-time"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string"
        },
        "retries": {
          "type": "integer",
          "format": "int32"
        },
        "retries_left": {
          "type": "integer",
          "format": "int32"
        },
        "results": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.ResultsRow"
          }
        },
        "nodes": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "profiles": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "node_count": {
          "type": "integer",
          "format": "int32"
        },
        "profile_count": {
          "type": "integer",
          "format": "int32"
        },
        "node_selectors": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.ManagerFilter"
          }
        },
        "scheduled_time": {
          "type": "string",
          "format": "date-time"
        },
        "recurrence": {
          "type": "string"
        },
        "parent_id": {
          "type": "string"
        },
        "job_count": {
          "type": "integer",
          "format": "int32"
        },
        "deleted": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.compliance.scanner.jobs.v1.Jobs": {
      "type": "object",
      "properties": {
        "jobs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Job"
          }
        },
        "total": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.compliance.scanner.jobs.v1.ManagerFilter": {
      "type": "object",
      "properties": {
        "manager_id": {
          "type": "string"
        },
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Filter"
          }
        }
      }
    },
    "chef.automate.api.compliance.scanner.jobs.v1.Query": {
      "type": "object",
      "properties": {
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Filter"
          }
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.compliance.scanner.jobs.v1.Query.OrderType"
        },
        "sort": {
          "type": "string"
        },
        "page": {
          "type": "integer",
          "format": "int32"
        },
        "per_page": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.compliance.scanner.jobs.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
    "chef.automate.api.compliance.scanner.jobs.v1.RerunResponse": {
      "type": "object"
    },
    "chef.automate.api.compliance.scanner.jobs.v1.ResultsRow": {
      "type": "object",
      "properties": {
        "node_id": {
          "type": "string"
        },
        "report_id": {
          "type": "string"
        },
        "status": {
          "type": "string"
        },
        "result": {
          "type": "string"
        },
        "job_id": {
          "type": "string"
        },
        "start_time": {
          "type": "string",
          "format": "date-time"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        }
      }
    },
    "chef.automate.domain.compliance.api.common.Filter": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string"
        },
        "exclude": {
          "type": "boolean",
          "format": "boolean"
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.domain.compliance.api.common.Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string"
        },
        "value": {
          "type": "string"
        }
      }
    }
  }
}
`)
}
