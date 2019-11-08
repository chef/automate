package api

func init() {
	Swagger.Add("nodes_nodes", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/nodes/nodes.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/nodes": {
      "post": {
        "summary": "Create a node",
        "description": "Creates a node given a FQDN or IP address, user-specified name, and an ssh or winrm credential reference.\nNodes created via this endpoint will be added to the \"Automate\" node manager.\nThese nodes are usually created for the purposes of running scan jobs.",
        "operationId": "Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Id"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/bulk-create": {
      "post": {
        "summary": "Bulk create nodes",
        "description": "Creates multiple nodes given node data.",
        "operationId": "BulkCreate",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Ids"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Nodes"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/delete": {
      "post": {
        "summary": "Bulk delete",
        "description": "Deletes a set of nodes given a filter query to match. \nQuery is the same one that is accepted by the list endpoint.",
        "operationId": "BulkDelete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.BulkDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Query"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/delete/ids": {
      "post": {
        "summary": "Bulk delete by id",
        "description": "Deletes a set of nodes given a list of ids.",
        "operationId": "BulkDeleteById",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.BulkDeleteResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Ids"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/id/{id}": {
      "get": {
        "summary": "Read a node",
        "description": "Returns the details for a node given the node id.",
        "operationId": "Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "UUID for the node.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "NodesService"
        ]
      },
      "delete": {
        "summary": "Delete a node",
        "description": "Deletes a node given the node id.",
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
            "description": "UUID for the node.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "NodesService"
        ]
      },
      "put": {
        "summary": "Update a node",
        "description": "PUT operation to update the details for a node, such as the name, fqdn, tags, or associated credentials.\nPlease note that this is a PUT operation, so all node details included in the create function\nshould be included in the PUT message to update.",
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
            "description": "UUID for the node.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/rerun/id/{id}": {
      "get": {
        "summary": "Rerun a node",
        "description": "Runs an ` + "`" + `inspec detect` + "`" + ` job on the node and updates the status to reflect\nthe status of the node accordingly (reachable or unreachable).",
        "operationId": "Rerun",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.RerunResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "UUID for the node.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/nodes/search": {
      "post": {
        "summary": "List nodes",
        "description": "Makes a list of nodes. \nSupports filtering, pagination, and sorting.\nAdding a filter makes a list of nodes that meet the filter criteria.\nSupported filters are: \naccount_id, last_contact, manager_id, manager_type, name, platform_name,\nplatform_release, region, source_id, state, statechange_timerange, status,\nlast_run_timerange, last_scan_timerange, last_run_status, last_scan_status,\nlast_run_penultimate_status, last_scan_penultimate_status",
        "operationId": "List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Nodes"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Query"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.nodes.v1.BulkDeleteResponse": {
      "type": "object",
      "properties": {
        "names": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of names of nodes created."
        }
      }
    },
    "chef.automate.api.nodes.v1.Id": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "UUID for the node."
        }
      }
    },
    "chef.automate.api.nodes.v1.Ids": {
      "type": "object",
      "properties": {
        "ids": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of node UUIDs."
        }
      }
    },
    "chef.automate.api.nodes.v1.LastContactData": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Infra run report id or InSpec scan report id."
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData.Status",
          "description": "Status on the last report for the node."
        },
        "penultimate_status": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData.Status",
          "description": "Status on the next-to-last report for the node."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "Endtime on the most recent report for the node."
        }
      },
      "description": "Most recent run or scan data for the node, taken from the most recent Infra run or InSpec scan executed on the node."
    },
    "chef.automate.api.nodes.v1.LastContactData.Status": {
      "type": "string",
      "enum": [
        "UNKNOWN",
        "PASSED",
        "FAILED",
        "SKIPPED"
      ],
      "default": "UNKNOWN"
    },
    "chef.automate.api.nodes.v1.Node": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "UUID for the node."
        },
        "name": {
          "type": "string",
          "description": "User-specified name for the node."
        },
        "platform": {
          "type": "string",
          "description": "Platform for the node."
        },
        "platform_version": {
          "type": "string",
          "description": "Platform version for the node."
        },
        "manager": {
          "type": "string",
          "description": "Manager associated with the node (automate, aws-ec2, aws-api, azure-vm, azure-api, gcp)."
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Kv"
          },
          "description": "Tags to be applied to the node."
        },
        "last_contact": {
          "type": "string",
          "format": "date-time",
          "description": "End time from the most recent ` + "`" + `detect` + "`" + ` or ` + "`" + `exec` + "`" + ` job."
        },
        "status": {
          "type": "string",
          "description": "Status of the node (unreachable, reachable, unknown)."
        },
        "last_job": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.ResultsRow",
          "description": "Details from the most recent scan job that executed for the node."
        },
        "target_config": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.TargetConfig",
          "description": "Details for ssh/winrm access of the node."
        },
        "manager_ids": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of manager ids associated with the node."
        },
        "connection_error": {
          "type": "string",
          "description": "Most recent connection error received from attempting to contact the node."
        },
        "state": {
          "type": "string",
          "description": "Last known state of the node (running, stopped, terminated)."
        },
        "name_prefix": {
          "type": "string",
          "description": "Name prefix to attach to the node. The full node name is constructed based off the prefix and the host."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects associated with the node. Projects are a concept introduced in IAMv2."
        },
        "run_data": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData",
          "description": "Most recent run data for the node, taken from the most recent infra run for the node."
        },
        "scan_data": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData",
          "description": "Most recent scan data for the node, taken from the most recent InSpec scan executed on the node."
        }
      },
      "description": "Node information."
    },
    "chef.automate.api.nodes.v1.Nodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
          },
          "description": "List of node objects."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of nodes in the system."
        },
        "total_unreachable": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of unreachable nodes in the system."
        },
        "total_reachable": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of reachable nodes in the system."
        },
        "total_unknown": {
          "type": "integer",
          "format": "int32",
          "description": "Total count of unknown nodes in the system."
        }
      }
    },
    "chef.automate.api.nodes.v1.Query": {
      "type": "object",
      "properties": {
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.domain.compliance.api.common.Filter"
          },
          "description": "Filters to be applied to the query."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.Query.OrderType",
          "description": "Order in which the results should be returned."
        },
        "sort": {
          "type": "string",
          "description": "Field on which to sort."
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "description": "Page number of results to return."
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "description": "Count of results that should be returned for each page."
        }
      }
    },
    "chef.automate.api.nodes.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
    "chef.automate.api.nodes.v1.RerunResponse": {
      "type": "object"
    },
    "chef.automate.api.nodes.v1.ResultsRow": {
      "type": "object",
      "properties": {
        "node_id": {
          "type": "string",
          "description": "Node id for the result."
        },
        "report_id": {
          "type": "string",
          "description": "Report id for the result."
        },
        "status": {
          "type": "string",
          "description": "Status of the report."
        },
        "result": {
          "type": "string",
          "description": "Error message on failed attempts to reach a node."
        },
        "job_id": {
          "type": "string",
          "description": "Job id from the report."
        },
        "start_time": {
          "type": "string",
          "format": "date-time",
          "description": "Start time on the report."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "End time on the report."
        }
      },
      "description": "Details from the most recent scan job that executed for the node."
    },
    "chef.automate.api.nodes.v1.TargetConfig": {
      "type": "object",
      "properties": {
        "secrets": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of credential ids to associate with the node."
        },
        "backend": {
          "type": "string",
          "description": "Details for the node backend (ssh, winrm, aws, ssm, azure, gcp)."
        },
        "host": {
          "type": "string",
          "description": "FQDN or IP address for the node."
        },
        "port": {
          "type": "integer",
          "format": "int32",
          "description": "Port used to connect to the node via ssh/winrm."
        },
        "sudo": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean to denote whether or not sudo should be used when accessing the node."
        },
        "ssl": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean to denote whether or not ssl should be checked when accessing the node."
        },
        "self_signed": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean to denote whether or not self signed certificate should be allowed when accessing the node."
        },
        "user": {
          "type": "string",
          "description": "Username used to access the node (taken from credential id associated with node)."
        },
        "sudo_options": {
          "type": "string",
          "description": "Sudo options to use when accessing the node."
        },
        "hosts": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of hostnames (fqdn or ip address) for nodes to be created with bulk create."
        }
      },
      "description": "Details for ssh/winrm access of the node."
    },
    "chef.automate.domain.compliance.api.common.Filter": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "description": "Field to filter on."
        },
        "exclude": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean to denote whether we should find nodes that match the filter or do not match the filter."
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Values to filter on for the key."
        }
      }
    },
    "chef.automate.domain.compliance.api.common.Kv": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "description": "Tag key."
        },
        "value": {
          "type": "string",
          "description": "Tag value."
        }
      }
    }
  }
}
`)
}
