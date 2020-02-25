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
          "Rules"
        ]
      },
      "delete": {
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
          "Rules"
        ]
      },
      "post": {
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
          "Rules"
        ]
      }
    },
    "/iam/v2/projects/{id}/rules": {
      "get": {
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Rules"
        ]
      }
    },
    "/iam/v2/projects/{project_id}/rules": {
      "post": {
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
          "Rules"
        ]
      }
    },
    "/iam/v2/projects/{project_id}/rules/{id}": {
      "get": {
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Rules"
        ]
      },
      "delete": {
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Rules"
        ]
      },
      "put": {
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
            "in": "path",
            "required": true,
            "type": "string"
          },
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRuleReq"
            }
          }
        ],
        "tags": [
          "Rules"
        ]
      }
    },
    "/iam/v2beta/apply-rules": {
      "get": {
        "operationId": "ApplyRulesStatus2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ApplyRulesStatusResp"
            }
          }
        },
        "tags": [
          "Rules"
        ]
      },
      "delete": {
        "operationId": "ApplyRulesCancel2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ApplyRulesCancelResp"
            }
          }
        },
        "tags": [
          "Rules"
        ]
      },
      "post": {
        "operationId": "ApplyRulesStart2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ApplyRulesStartResp"
            }
          }
        },
        "tags": [
          "Rules"
        ]
      }
    },
    "/iam/v2beta/projects/{id}/rules": {
      "get": {
        "operationId": "ListRulesForProject2",
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
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Rules"
        ]
      }
    },
    "/iam/v2beta/projects/{project_id}/rules": {
      "post": {
        "operationId": "CreateRule2",
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
          "Rules"
        ]
      }
    },
    "/iam/v2beta/projects/{project_id}/rules/{id}": {
      "get": {
        "operationId": "GetRule2",
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Rules"
        ]
      },
      "delete": {
        "operationId": "DeleteRule2",
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
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "Rules"
        ]
      },
      "put": {
        "operationId": "UpdateRule2",
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
            "in": "path",
            "required": true,
            "type": "string"
          },
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRuleReq"
            }
          }
        ],
        "tags": [
          "Rules"
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
      "properties": {
        "state": {
          "type": "string"
        },
        "estimated_time_complete": {
          "type": "string",
          "format": "date-time"
        },
        "percentage_complete": {
          "type": "number",
          "format": "float"
        },
        "failed": {
          "type": "boolean",
          "format": "boolean"
        },
        "failure_message": {
          "type": "string"
        },
        "cancelled": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.iam.v2.Condition": {
      "type": "object",
      "properties": {
        "attribute": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.ConditionAttribute"
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "operator": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.ConditionOperator"
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
      "properties": {
        "id": {
          "type": "string"
        },
        "project_id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Condition"
          }
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
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Rule"
        }
      }
    },
    "chef.automate.api.iam.v2.ListRulesForProjectResp": {
      "type": "object",
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
          "type": "string"
        },
        "project_id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Condition"
          }
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleStatus"
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
      "properties": {
        "id": {
          "type": "string"
        },
        "project_id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.RuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Condition"
          }
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
