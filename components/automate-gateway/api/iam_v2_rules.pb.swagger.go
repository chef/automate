package api

func init() {
	Swagger.Add("iam_v2_rules", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2/rules.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/iam/v2/apply-rules": {
      "get": {
        "summary": "Get the status of a project update",
        "description": "Returns details about a project update operation.\n\nYou can poll this endpoint during a project update to monitor progress. \nQuerying this endpoint when there is no update in progress will return details \nabout the completion status of the most recent update.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:rules:status\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ApplyRulesStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ApplyRulesStatusResp"
            }
          }
        },
        "tags": [
          "rules"
        ]
      },
      "delete": {
        "summary": "Cancel project update",
        "description": "Cancels an ongoing project update.\n\nWarning! This action leaves the system in an unknown state that only another\nsuccessful project update can rectify.\n\nThis command exists really just for one scenario: you started a project update\nbut shortly thereafter discovered that you had one more change to include in the\nbatch of updates to be done.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:rules:cancel\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ApplyRulesCancel",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ApplyRulesCancelResp"
            }
          }
        },
        "tags": [
          "rules"
        ]
      },
      "post": {
        "summary": "Start project update",
        "description": "Any changes to a project's rules are staged first. They do not take effect until \nall projects are updated.\n\nUpdating all projects begins an operation that applies all pending rule edits \nand then moves ingested resources into the correct projects according to those latest changes.\n\nWith a large amount of historical compliance data, rule application can take a considerable amount of time.\nIt’s best to batch up rule changes and apply them all at once.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:rules:apply\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ApplyRulesStart",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ApplyRulesStartResp"
            }
          }
        },
        "tags": [
          "rules"
        ]
      }
    },
    "/iam/v2/projects/{id}/rules": {
      "get": {
        "summary": "List a project's rules",
        "description": "Lists all of the project rules of a specific project.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "ListRulesForProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListRulesForProjectResp"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "description": "ID of the project.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "rules"
        ]
      }
    },
    "/iam/v2/projects/{project_id}/rules": {
      "post": {
        "summary": "Create a project rule",
        "description": "Creates a new project rule to move ingested resources into projects.\n\nA project rule contains conditions that determine if an ingested resource should be moved into the rule’s project. \n\nEach condition specifies one or more values to match for a particular attribute on an ingested resource.\n\nThe choice of attributes depends on the rule type.\nFor NODE type, specify any of the available attributes.\nFor EVENT type, specify either CHEF_ORGANIZATION or CHEF_SERVER.\n\nThe choice of operator depends on how many values you provide.\nIf you wish to match one among a group of values, set the operator to MEMBER_OF. \nFor a single value, use EQUALS.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "CreateRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRuleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "project_id",
            "description": "Unique ID of the project this rule belongs to. Cannot be changed.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRuleReq"
            }
          }
        ],
        "tags": [
          "rules"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"id\": \"example-rule\", \"project-id\": \"example-project\", \"name\": \"My Example Rule\", \"type\": \"NODE\", \"conditions\": [{\"attribute\": \"CHEF_SERVER\", \"operator\": \"MEMBER_OF\", \"values\": [\"example.co\", \"example.io\"]}]}"
          }
        ]
      }
    },
    "/iam/v2/projects/{project_id}/rules/{id}": {
      "get": {
        "summary": "Get a project rule",
        "description": "Returns the details for a project rule.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetRuleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "project_id",
            "description": "ID of the project the rule belongs to.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "description": "ID of the project rule.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "rules"
        ]
      },
      "delete": {
        "summary": "Delete a project rule",
        "description": "The resulting change to the project's resources does not take effect immediately. \nUpdates to project rules must be applied to ingested resources by a project update.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "DeleteRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteRuleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "project_id",
            "description": "ID of the project the rule belongs to.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "description": "ID of the project rule.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "rules"
        ]
      },
      "put": {
        "summary": "Update a project rule",
        "description": "Updates the name and conditions of an existing project rule.\nNew conditions can be added. Existing conditions can be updated or removed.\n\nThis operation overwrites all fields excluding ID and Type,\nincluding those omitted from the request, so be sure to specify all properties.\nProperties that you do not include are reset to empty values.\n\nThe resulting change to the project's resources does not take effect immediately. \nUpdates to project rules must be applied to ingested resources by a project update.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "UpdateRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRuleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "project_id",
            "description": "Unique ID of the project this rule belongs to. Cannot be changed.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "description": "Unique ID. Cannot be changed.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRuleReq"
            }
          }
        ],
        "tags": [
          "rules"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"id\": \"example-rule\", \"project-id\": \"example-project\", \"name\": \"My Updated Rule\", \"type\": \"NODE\", \"conditions\": [{\"attribute\": \"CHEF_SERVER\", \"operator\": \"EQUALS\", \"values\": [\"example.co\"]}]}"
          }
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.ApplyRulesCancelResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.ApplyRulesStartResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.ApplyRulesStatusResp": {
      "type": "object",
      "example": {
        "state": "running",
        "estimated_time_complete": "2020-03-20T19:24:55Z",
        "percentage_complete": 0.5,
        "failed": false,
        "failure_message": "",
        "cancelled": false
      },
      "properties": {
        "state": {
          "type": "string",
          "description": "One of two states: ` + "`" + `not_running` + "`" + ` and ` + "`" + `running` + "`" + `."
        },
        "estimated_time_complete": {
          "type": "string",
          "format": "date-time",
          "description": "Estimated time when the project update will complete."
        },
        "percentage_complete": {
          "type": "number",
          "format": "float",
          "description": "The percentage complete in decimal format from 0 to 1."
        },
        "failed": {
          "type": "boolean",
          "format": "boolean",
          "description": "Whether or not the project update has failed."
        },
        "failure_message": {
          "type": "string",
          "description": "The error message from the failure."
        },
        "cancelled": {
          "type": "boolean",
          "format": "boolean",
          "description": "Whether or not the project update was canceled."
        }
      }
    },
    "chef.automate.api.iam.v2.Condition": {
      "type": "object",
      "properties": {
        "attribute": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.ConditionAttribute",
          "description": "Represents a property of an ingested resource. Depends on the rule type."
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The value(s) of the attribute that an ingested resource must match."
        },
        "operator": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.ConditionOperator",
          "description": "Whether the attribute matches a single value (` + "`" + `EQUALS` + "`" + `) or \nmatches at least one of a set of values (` + "`" + `MEMBER_OF` + "`" + `)."
        }
      }
    },
    "chef.automate.api.iam.v2.ConditionAttribute": {
      "type": "string",
      "enum": [
        "CONDITION_ATTRIBUTE_UNSET",
        "CHEF_SERVER",
        "CHEF_ORGANIZATION",
        "ENVIRONMENT",
        "CHEF_ROLE",
        "CHEF_TAG",
        "CHEF_POLICY_GROUP",
        "CHEF_POLICY_NAME"
      ],
      "default": "CONDITION_ATTRIBUTE_UNSET"
    },
    "chef.automate.api.iam.v2.ConditionOperator": {
      "type": "string",
      "enum": [
        "CONDITION_OPERATOR_UNSET",
        "MEMBER_OF",
        "EQUALS"
      ],
      "default": "CONDITION_OPERATOR_UNSET"
    },
    "chef.automate.api.iam.v2.CreateRuleReq": {
      "type": "object",
      "example": {
        "id": "example-rule",
        "project-id": "example-project",
        "name": "My Example Rule",
        "type": "NODE",
        "conditions": [
          {
            "attribute": "CHEF_SERVER",
            "operator": "MEMBER_OF",
            "values": [
              "example.co",
              "example.io"
            ]
          }
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "project_id": {
          "type": "string",
          "description": "Unique ID of the project this rule belongs to. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the project rule."
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleType",
          "description": "Whether the rule is ` + "`" + `STAGED` + "`" + ` (not in effect) or ` + "`" + `APPLIED` + "`" + ` (in effect)."
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Condition"
          },
          "description": "Conditions that ingested resources must match to belong to the project. \nWill contain one or more."
        }
      },
      "required": [
        "id",
        "project_id",
        "name",
        "type",
        "conditions"
      ]
    },
    "chef.automate.api.iam.v2.CreateRuleResp": {
      "type": "object",
      "example": {
        "id": "example-rule",
        "project-id": "example-project",
        "name": "My Example Rule",
        "type": "NODE",
        "conditions": [
          {
            "attribute": "CHEF_SERVER",
            "operator": "MEMBER_OF",
            "values": [
              "example.co",
              "example.io"
            ]
          }
        ],
        "status": "STAGED"
      },
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Rule"
        }
      }
    },
    "chef.automate.api.iam.v2.DeleteRuleResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.GetRuleResp": {
      "type": "object",
      "example": {
        "id": "example-rule",
        "project-id": "example-project",
        "name": "My Applied Rule",
        "type": "NODE",
        "conditions": [
          {
            "attribute": "CHEF_SERVER",
            "operator": "EQUALS",
            "values": [
              "example.co"
            ]
          }
        ],
        "status": "APPLIED"
      },
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Rule"
        }
      }
    },
    "chef.automate.api.iam.v2.ListRulesForProjectResp": {
      "type": "object",
      "example": {
        "rules": [
          {
            "id": "example-rule",
            "project-id": "example-project",
            "name": "My Applied Rule",
            "type": "NODE",
            "conditions": [
              {
                "attribute": "CHEF_SERVER",
                "operator": "EQUALS",
                "values": [
                  "example.co"
                ]
              }
            ],
            "status": "APPLIED"
          },
          {
            "id": "example-rule-2",
            "project-id": "example-project",
            "name": "My 2nd Example Rule",
            "type": "EVENT",
            "conditions": [
              {
                "attribute": "CHEF_ORGANIZATION",
                "operator": "MEMBER_OF",
                "values": [
                  "east",
                  "west"
                ]
              }
            ],
            "status": "APPLIED"
          }
        ],
        "status": "APPLIED"
      },
      "properties": {
        "rules": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Rule"
          }
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.ProjectRulesStatus"
        }
      }
    },
    "chef.automate.api.iam.v2.ProjectRulesStatus": {
      "type": "string",
      "enum": [
        "PROJECT_RULES_STATUS_UNSET",
        "RULES_APPLIED",
        "EDITS_PENDING",
        "NO_RULES"
      ],
      "default": "PROJECT_RULES_STATUS_UNSET"
    },
    "chef.automate.api.iam.v2.Rule": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "project_id": {
          "type": "string",
          "description": "Unique ID of the project this rule belongs to. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the project rule."
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleType",
          "description": "Whether the rule applies to ingested ` + "`" + `NODE` + "`" + ` or ` + "`" + `EVENT resources.\nCannot be changed."
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Condition"
          },
          "description": "Conditions that ingested resources must match to belong to the project. \nWill contain one or more."
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleStatus",
          "description": "Whether the rule is ` + "`" + `STAGED` + "`" + ` (not in effect) or ` + "`" + `APPLIED` + "`" + ` (in effect)."
        }
      }
    },
    "chef.automate.api.iam.v2.RuleStatus": {
      "type": "string",
      "enum": [
        "RULE_STATUS_UNSET",
        "STAGED",
        "APPLIED"
      ],
      "default": "RULE_STATUS_UNSET"
    },
    "chef.automate.api.iam.v2.RuleType": {
      "type": "string",
      "enum": [
        "RULE_TYPE_UNSET",
        "NODE",
        "EVENT"
      ],
      "default": "RULE_TYPE_UNSET"
    },
    "chef.automate.api.iam.v2.UpdateRuleReq": {
      "type": "object",
      "example": {
        "id": "example-rule",
        "project-id": "example-project",
        "name": "My Updated Rule",
        "type": "NODE",
        "conditions": [
          {
            "attribute": "CHEF_SERVER",
            "operator": "EQUALS",
            "values": [
              "example.co"
            ]
          }
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "project_id": {
          "type": "string",
          "description": "Unique ID of the project this rule belongs to. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the project rule."
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleType",
          "description": "Whether the rule applies to ingested ` + "`" + `NODE` + "`" + ` or ` + "`" + `EVENT resources.\nCannot be changed."
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Condition"
          },
          "description": "Conditions that ingested resources must match to belong to the project. \nWill contain one or more."
        }
      },
      "required": [
        "id",
        "project_id",
        "name",
        "type",
        "conditions"
      ]
    },
    "chef.automate.api.iam.v2.UpdateRuleResp": {
      "type": "object",
      "example": {
        "id": "example-rule",
        "project-id": "example-project",
        "name": "My Updated Rule",
        "type": "NODE",
        "conditions": [
          {
            "attribute": "CHEF_SERVER",
            "operator": "EQUALS",
            "values": [
              "example.co"
            ]
          }
        ],
        "status": "STAGED"
      },
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Rule"
        }
      }
    }
  }
}
`)
}
