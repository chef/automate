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
    "/iam/v2beta/rules": {
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
    }
  },
  "definitions": {
    "v2betaCondition": {
      "type": "object",
      "properties": {
        "type": {
          "$ref": "#/definitions/v2betaConditionType"
        },
        "values": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaConditionType": {
      "type": "string",
      "enum": [
        "CONDITION_TYPE_UNSET",
        "CHEF_SERVERS",
        "CHEF_ORGS",
        "CHEF_ENVIRONMENTS",
        "ROLES",
        "CHEF_TAGS",
        "POLICY_GROUP",
        "POLICY_NAME"
      ],
      "default": "CONDITION_TYPE_UNSET"
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
        }
      }
    },
    "v2betaRuleType": {
      "type": "string",
      "enum": [
        "RULE_TYPE_UNSET",
        "NODE",
        "EVENT"
      ],
      "default": "RULE_TYPE_UNSET"
    }
  }
}
`)
}
