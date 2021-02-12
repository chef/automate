package api

func init() {
	Swagger.Add("nodes", `{
  "swagger": "2.0",
  "info": {
    "title": "external/nodes/nodes.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/nodes": {
      "post": {
        "summary": "Create a Node",
        "description": "Creates a node and adds it to the Chef Automate node manager.\nRequires a FQDN or IP address, a user-specified name, and a ssh or winrm credential reference.\nUseful for creating nodes for the purpose of running compliance scan jobs.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"name\": \"my-vagrant-node\",\n\"manager\":\"automate\",\n\"target_config\": {\n\"backend\":\"ssh\",\n\"host\":\"localhost\",\n\"secrets\":[\"b75195e5-a173-4502-9f59-d949adfe2c38\"],\n\"port\": 22\n},\n\"tags\": [\n{ \"key\":\"test-node\", \"value\":\"is amazing\" }\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_Create",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Id"
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
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/api/v0/nodes/bulk-create": {
      "post": {
        "summary": "Bulk Create Nodes",
        "description": "Creates multiple nodes from a list of node data.\n` + "`" + `hosts` + "`" + ` field is required. Multiple hosts may be defined in this field.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"name_prefix\": \"000-my-ssh-node\",\n\"manager\":\"automate\",\n\"target_config\": {\n\"backend\":\"ssh\",\n\"hosts\":[\"localhost\",\"127.0.0.1\"],\n\"secrets\":[\"b75195e5-a173-4502-9f59-d949adfe2c38\"],\n\"port\": 22\n},\n\"tags\": [\n{ \"key\":\"test-node\", \"value\":\"is-amazing\" },\n]\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_BulkCreate",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Ids"
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
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Nodes"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/api/v0/nodes/delete": {
      "post": {
        "summary": "Bulk Delete Nodes by Filter",
        "description": "Deletes a set of nodes that match a filter.\nAvailable filters: account_id, last_contact, manager_id, manager_type, name, platform_name,\nplatform_release, region, source_id, state, statechange_timerange, status,\nlast_run_timerange, last_scan_timerange, last_run_status, last_scan_status,\nlast_run_penultimate_status, last_scan_penultimate_status\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\"filters\": [{\"key\": \"name\", \"values\": [\"vj*\"]}]}'\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_BulkDelete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.BulkDeleteResponse"
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
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Query"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/api/v0/nodes/delete/ids": {
      "post": {
        "summary": "Bulk Delete Nodes by ID",
        "description": "Deletes a set of nodes given a list of IDs.\nInvalid IDs will be ignored.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_BulkDeleteById",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.BulkDeleteResponse"
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
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Ids"
            }
          }
        ],
        "tags": [
          "NodesService"
        ]
      }
    },
    "/api/v0/nodes/id/{id}": {
      "get": {
        "summary": "Show Node Details",
        "description": "Returns the details for a node given the node ID.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_Read",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Node"
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
            "description": "Unique node ID (UUID)",
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
        "summary": "Delete a Node",
        "description": "Deletes the node with the node ID.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_Delete",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
            "description": "Unique node ID (UUID)",
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
        "summary": "Update Node",
        "description": "This PUT operation overwrites ALL node details and requires the complete set of node details,\nconsisting of a FQDN or IP address, a user-specified name, and the ID for an ssh or winrm credential.\nSubstitute the desired values for the existing node details in the PUT message.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_Update",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "properties": {}
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
            "description": "Unique node ID (UUID).",
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
    "/api/v0/nodes/rerun/id/{id}": {
      "get": {
        "summary": "List Node Status",
        "description": "Use this to run an ` + "`" + `inspec detect` + "`" + ` job on the node, which updates the status to reflect that the node is reachable or unreachable.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:rerun\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_Rerun",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.RerunResponse"
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
            "description": "Unique node ID (UUID)",
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
    "/api/v0/nodes/search": {
      "post": {
        "summary": "List and Filter Nodes",
        "description": "Makes a list of nodes.\nSupports filtering, pagination, and sorting.\nAdding a filter narrows the list of nodes to only those that match the filter or filters.\nSupported filters:\naccount_id, last_contact, manager_id, manager_type, name, platform_name,\nplatform_release, region, source_id, state, statechange_timerange, status,\nlast_run_timerange, last_scan_timerange, last_run_status, last_scan_status,\nlast_run_penultimate_status, last_scan_penultimate_status\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\n{\n\"filters\":[\n{\"key\": \"last_scan_status\", \"values\": [\"FAILED\"]},\n{\"key\": \"last_scan_penultimate_status\", \"values\": [\"PASSED\"]},\n{\"key\": \"name\", \"values\": [\"MyNode*\"]}\n],\n\"page\":1, \"per_page\":100,\n\"sort\":\"status\", \"order\":\"ASC\"\n}\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\ninfra:nodes:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "NodesService_List",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.nodes.v1.Nodes"
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
    "chef.automate.api.common.query.Filter": {
      "type": "object",
      "properties": {
        "key": {
          "type": "string",
          "description": "Field to filter on."
        },
        "exclude": {
          "type": "boolean",
          "format": "boolean",
          "description": "Include matches for this filter.(boolean)\n` + "`" + `true` + "`" + ` (default) *includes* all nodes that match this filter. \n` + "`" + `false` + "`" + ` *excludes* all nodes that match this filter."
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Field values to filter on."
        }
      }
    },
    "chef.automate.api.common.query.Kv": {
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
    },
    "chef.automate.api.nodes.v1.BulkDeleteResponse": {
      "type": "object",
      "properties": {
        "names": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of deleted nodes, by name."
        }
      }
    },
    "chef.automate.api.nodes.v1.Id": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "title": "Unique node ID (UUID)"
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
          "description": "Chef Infra run report ID or InSpec scan report ID."
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData.Status",
          "description": "Last node report status."
        },
        "penultimate_status": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData.Status",
          "description": "Next-to-last node status report."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "description": "Last node report endtime."
        }
      },
      "description": "Most recent node data from the latest Chef Infra run and InSpec scan."
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
          "description": "Unique node ID (UUID)."
        },
        "name": {
          "type": "string",
          "description": "User-specified node name."
        },
        "platform": {
          "type": "string",
          "description": "Node platform."
        },
        "platform_version": {
          "type": "string",
          "description": "Node platform version."
        },
        "manager": {
          "type": "string",
          "description": "Node manager (automate, aws-ec2, aws-api, azure-vm, azure-api, gcp)."
        },
        "tags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Kv"
          },
          "description": "Node tags."
        },
        "last_contact": {
          "type": "string",
          "format": "date-time",
          "description": "Timestamp of the last ` + "`" + `detect` + "`" + ` or ` + "`" + `exec` + "`" + ` job."
        },
        "status": {
          "type": "string",
          "description": "Node status (unreachable, reachable, unknown)."
        },
        "last_job": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.ResultsRow",
          "description": "Results of the last compliance scan job for this node."
        },
        "target_config": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.TargetConfig",
          "description": "Node configuration for ssh or winrm."
        },
        "manager_ids": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of manager IDs for the node."
        },
        "connection_error": {
          "type": "string",
          "description": "Last connection error received when trying to contact the node."
        },
        "state": {
          "type": "string",
          "description": "Last known node state (running, stopped, terminated)."
        },
        "name_prefix": {
          "type": "string",
          "description": "Prefix for node name. The full node name is the prefix + the host."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects associated with the node."
        },
        "run_data": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData",
          "description": "Most recent node data from the last Chef Infra run results."
        },
        "scan_data": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.LastContactData",
          "description": "Most recent compliance scan data for the node from the last InSpec scan."
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
          "description": "List of nodes."
        },
        "total": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of nodes in the system."
        },
        "total_unreachable": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of unreachable nodes in the system."
        },
        "total_reachable": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of reachable nodes in the system."
        },
        "total_unknown": {
          "type": "integer",
          "format": "int32",
          "description": "Total number of unknown nodes in the system."
        }
      }
    },
    "chef.automate.api.nodes.v1.Query": {
      "type": "object",
      "properties": {
        "filters": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.common.query.Filter"
          },
          "description": "Use filters to limit the set of nodes to delete."
        },
        "order": {
          "$ref": "#/definitions/chef.automate.api.nodes.v1.Query.OrderType"
        },
        "sort": {
          "type": "string",
          "description": "Sort the results on a specific field."
        },
        "page": {
          "type": "integer",
          "format": "int32",
          "description": "Starting page for the results."
        },
        "per_page": {
          "type": "integer",
          "format": "int32",
          "description": "The number of results on each page."
        }
      }
    },
    "chef.automate.api.nodes.v1.Query.OrderType": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC",
      "description": "Return the results in ascending or descending order."
    },
    "chef.automate.api.nodes.v1.RerunResponse": {
      "type": "object"
    },
    "chef.automate.api.nodes.v1.ResultsRow": {
      "type": "object",
      "properties": {
        "node_id": {
          "type": "string",
          "description": "Unique node ID."
        },
        "report_id": {
          "type": "string",
          "description": "Unique ID of the report generated by the InSpec scan."
        },
        "status": {
          "type": "string",
          "description": "Status of the report (failed, success, skipped)."
        },
        "result": {
          "type": "string",
          "description": "Error message returned after several failed attempts to contact a node."
        },
        "job_id": {
          "type": "string",
          "description": "Unique ID of the scan job that generated the report."
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
      "description": "Summary of the last Chef InSpec scan job run on the node."
    },
    "chef.automate.api.nodes.v1.TargetConfig": {
      "type": "object",
      "properties": {
        "secrets": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of credential IDs for a node."
        },
        "backend": {
          "type": "string",
          "description": "Node backend type (ssh, winrm, aws, ssm, azure, gcp)."
        },
        "host": {
          "type": "string",
          "description": "Node FQDN or IP address."
        },
        "port": {
          "type": "integer",
          "format": "int32",
          "title": "ssh or winrm connection port"
        },
        "sudo": {
          "type": "boolean",
          "format": "boolean",
          "description": "Uses ` + "`" + `sudo` + "`" + ` (boolean)."
        },
        "ssl": {
          "type": "boolean",
          "format": "boolean",
          "description": "Check ssl (boolean)."
        },
        "self_signed": {
          "type": "boolean",
          "format": "boolean",
          "description": "Allow self-signed certificate (boolean)."
        },
        "user": {
          "type": "string",
          "description": "Username from the credential ID for this node."
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
          "description": "List of hostnames (FQDN or IP address) for bulk creating nodes."
        }
      },
      "description": "Details for ssh/winrm access of the node."
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
