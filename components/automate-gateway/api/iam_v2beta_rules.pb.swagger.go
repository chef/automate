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
              "$ref": "#/definitions/v2betaApplyRulesStatusResp"
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
              "$ref": "#/definitions/v2betaApplyRulesCancelResp"
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
              "$ref": "#/definitions/v2betaApplyRulesStartResp"
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
              "$ref": "#/definitions/v2betaListRulesForProjectResp"
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
    "/iam/v2beta/rules": {
      "get": {
        "summary": "TODO (tc): Right now we don't have plans to use this in the UI\nand it doesn't return the staged rules like all the other query APIs.\nShould probably delete it or update it to return staged rules.",
        "operationId": "ListRules",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListRulesResp"
            }
          }
        },
        "tags": [
          "Rules"
        ]
      },
      "post": {
        "operationId": "CreateRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaCreateRuleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaCreateRuleReq"
            }
          }
        ],
        "tags": [
          "Rules"
        ]
      }
    },
    "/iam/v2beta/rules/{id}": {
      "get": {
        "operationId": "GetRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetRuleResp"
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
      },
      "delete": {
        "operationId": "DeleteRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaDeleteRuleResp"
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
      },
      "put": {
        "operationId": "UpdateRule",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaUpdateRuleResp"
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
              "$ref": "#/definitions/v2betaUpdateRuleReq"
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
    "v2betaApplyRulesCancelResp": {
      "type": "object"
    },
    "v2betaApplyRulesStartResp": {
      "type": "object"
    },
    "v2betaApplyRulesStatusResp": {
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
        }
      }
    },
    "v2betaCondition": {
      "type": "object",
      "properties": {
        "attribute": {
          "$ref": "#/definitions/v2betaConditionAttribute"
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "operator": {
          "$ref": "#/definitions/v2betaConditionOperator"
        }
      }
    },
    "v2betaConditionAttribute": {
      "type": "string",
      "enum": [
        "CONDITION_ATTRIBUTE_UNSET",
        "CHEF_SERVERS",
        "CHEF_ORGS",
        "CHEF_ENVIRONMENTS",
        "ROLES",
        "CHEF_TAGS",
        "POLICY_GROUP",
        "POLICY_NAME"
      ],
      "default": "CONDITION_ATTRIBUTE_UNSET"
    },
    "v2betaConditionOperator": {
      "type": "string",
      "enum": [
        "CONDITION_OPERATOR_UNSET",
        "MEMBER_OF",
        "EQUALS"
      ],
      "default": "CONDITION_OPERATOR_UNSET"
    },
    "v2betaCreateRuleReq": {
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
          "$ref": "#/definitions/v2betaRuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaCondition"
          }
        }
      }
    },
    "v2betaCreateRuleResp": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/v2betaRule"
        }
      }
    },
    "v2betaDeleteRuleResp": {
      "type": "object"
    },
    "v2betaGetRuleResp": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/v2betaRule"
        }
      }
    },
    "v2betaListRulesForProjectResp": {
      "type": "object",
      "properties": {
        "rules": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaRule"
          }
        },
        "status": {
          "$ref": "#/definitions/v2betaProjectRulesStatus"
        }
      }
    },
    "v2betaListRulesResp": {
      "type": "object",
      "properties": {
        "rules": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaRule"
          }
        }
      }
    },
    "v2betaProjectRulesStatus": {
      "type": "string",
      "enum": [
        "PROJECT_RULES_STATUS_UNSET",
        "RULES_APPLIED",
        "EDITS_PENDING",
        "NO_RULES"
      ],
      "default": "PROJECT_RULES_STATUS_UNSET"
    },
    "v2betaRule": {
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
          "$ref": "#/definitions/v2betaRuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaCondition"
          }
        },
        "status": {
          "$ref": "#/definitions/v2betaRuleStatus"
        }
      }
    },
    "v2betaRuleStatus": {
      "type": "string",
      "enum": [
        "RULE_STATUS_UNSET",
        "STAGED",
        "APPLIED"
      ],
      "default": "RULE_STATUS_UNSET"
    },
    "v2betaRuleType": {
      "type": "string",
      "enum": [
        "RULE_TYPE_UNSET",
        "NODE",
        "EVENT"
      ],
      "default": "RULE_TYPE_UNSET"
    },
    "v2betaUpdateRuleReq": {
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
          "$ref": "#/definitions/v2betaRuleType"
        },
        "conditions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaCondition"
          }
        }
      }
    },
    "v2betaUpdateRuleResp": {
      "type": "object",
      "properties": {
        "rule": {
          "$ref": "#/definitions/v2betaRule"
        }
      }
    }
  }
}
`)
}
