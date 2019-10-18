package api

func init() {
	Swagger.Add("iam_v2beta_rules", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2beta/rules.proto",
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
    "/iam/v2beta/apply-rules": {
      "get": {
        "operationId": "ApplyRulesStatus",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.ApplyRulesStatusResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.ApplyRulesCancelResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.ApplyRulesStartResp"
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
        "operationId": "ListRulesForProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.ListRulesForProjectResp"
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
        "operationId": "CreateRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.CreateRuleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.CreateRuleReq"
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
        "operationId": "GetRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.GetRuleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.DeleteRuleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.UpdateRuleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.UpdateRuleReq"
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
    "chef.automate.api.iam.v2beta.ApplyRulesCancelResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2beta.ApplyRulesStartResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2beta.ApplyRulesStatusResp": {
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
    "chef.automate.api.iam.v2beta.Condition": {
      "type": "object",
      "properties": {
        "attribute": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.ConditionAttribute"
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "operator": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.ConditionOperator"
        }
      }
    },
    "chef.automate.api.iam.v2beta.ConditionAttribute": {
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
    "chef.automate.api.iam.v2beta.ConditionOperator": {
      "type": "string",
      "enum": [
        "CONDITION_OPERATOR_UNSET",
        "MEMBER_OF",
        "EQUALS"
      ],
      "default": "CONDITION_OPERATOR_UNSET"
    },
    "chef.automate.api.iam.v2beta.CreateRuleReq": {
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
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.RuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2beta.Condition"
          }
        }
      }
    },
    "chef.automate.api.iam.v2beta.CreateRuleResp": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Rule"
        }
      }
    },
    "chef.automate.api.iam.v2beta.DeleteRuleResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2beta.GetRuleResp": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Rule"
        }
      }
    },
    "chef.automate.api.iam.v2beta.ListRulesForProjectResp": {
      "type": "object",
      "properties": {
        "rules": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2beta.Rule"
          }
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.ProjectRulesStatus"
        }
      }
    },
    "chef.automate.api.iam.v2beta.ProjectRulesStatus": {
      "type": "string",
      "enum": [
        "PROJECT_RULES_STATUS_UNSET",
        "RULES_APPLIED",
        "EDITS_PENDING",
        "NO_RULES"
      ],
      "default": "PROJECT_RULES_STATUS_UNSET"
    },
    "chef.automate.api.iam.v2beta.Rule": {
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
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.RuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2beta.Condition"
          }
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.RuleStatus"
        }
      }
    },
    "chef.automate.api.iam.v2beta.RuleStatus": {
      "type": "string",
      "enum": [
        "RULE_STATUS_UNSET",
        "STAGED",
        "APPLIED"
      ],
      "default": "RULE_STATUS_UNSET"
    },
    "chef.automate.api.iam.v2beta.RuleType": {
      "type": "string",
      "enum": [
        "RULE_TYPE_UNSET",
        "NODE",
        "EVENT"
      ],
      "default": "RULE_TYPE_UNSET"
    },
    "chef.automate.api.iam.v2beta.UpdateRuleReq": {
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
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.RuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2beta.Condition"
          }
        }
      }
    },
    "chef.automate.api.iam.v2beta.UpdateRuleResp": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Rule"
        }
      }
    }
  }
}
`)
}
