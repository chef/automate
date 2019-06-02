package api

func init() {
	Swagger.Add("iam_v2beta_policy", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2beta/policy.proto",
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
    "/iam/v2beta/introspect_projects": {
      "get": {
        "operationId": "IntrospectAllProjects",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListProjectsResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/policies": {
      "get": {
        "operationId": "ListPolicies",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListPoliciesResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      },
      "post": {
        "operationId": "CreatePolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaCreatePolicyResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaCreatePolicyReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/policies/{id}": {
      "get": {
        "operationId": "GetPolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetPolicyResp"
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
          "Policies"
        ]
      },
      "delete": {
        "operationId": "DeletePolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaDeletePolicyResp"
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
          "Policies"
        ]
      },
      "put": {
        "operationId": "UpdatePolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaUpdatePolicyResp"
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
              "$ref": "#/definitions/v2betaUpdatePolicyReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/policies/{id}/members": {
      "get": {
        "operationId": "ListPolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListPolicyMembersResp"
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
          "Policies"
        ]
      },
      "put": {
        "operationId": "ReplacePolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaReplacePolicyMembersResp"
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
              "$ref": "#/definitions/v2betaReplacePolicyMembersReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/policies/{id}/members:add": {
      "post": {
        "operationId": "AddPolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaAddPolicyMembersResp"
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
              "$ref": "#/definitions/v2betaAddPolicyMembersReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/policies/{id}/members:remove": {
      "post": {
        "operationId": "RemovePolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaRemovePolicyMembersResp"
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
              "$ref": "#/definitions/v2betaRemovePolicyMembersReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/policy_version": {
      "get": {
        "operationId": "GetPolicyVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetPolicyVersionResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/projects": {
      "get": {
        "operationId": "ListProjects",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListProjectsResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      },
      "post": {
        "operationId": "CreateProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaCreateProjectResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaCreateProjectReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/projects/{id}": {
      "get": {
        "operationId": "GetProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetProjectResp"
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
          "Policies"
        ]
      },
      "delete": {
        "operationId": "DeleteProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaDeleteProjectResp"
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
          "Policies"
        ]
      },
      "put": {
        "operationId": "UpdateProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaUpdateProjectResp"
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
              "$ref": "#/definitions/v2betaUpdateProjectReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/roles": {
      "get": {
        "operationId": "ListRoles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaListRolesResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      },
      "post": {
        "operationId": "CreateRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaCreateRoleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/v2betaCreateRoleReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/roles/{id}": {
      "get": {
        "operationId": "GetRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaGetRoleResp"
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
          "Policies"
        ]
      },
      "delete": {
        "operationId": "DeleteRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaDeleteRoleResp"
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
          "Policies"
        ]
      },
      "put": {
        "operationId": "UpdateRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/v2betaUpdateRoleResp"
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
              "$ref": "#/definitions/v2betaUpdateRoleReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    }
  },
  "definitions": {
    "StatementEffect": {
      "type": "string",
      "enum": [
        "ALLOW",
        "DENY"
      ],
      "default": "ALLOW"
    },
    "VersionVersionNumber": {
      "type": "string",
      "enum": [
        "V0",
        "V1",
        "V2"
      ],
      "default": "V0"
    },
    "iamv2betaType": {
      "type": "string",
      "enum": [
        "CHEF_MANAGED",
        "CUSTOM"
      ],
      "default": "CHEF_MANAGED"
    },
    "v2betaAddPolicyMembersReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaAddPolicyMembersResp": {
      "type": "object",
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaCreatePolicyReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "statements": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaStatement"
          }
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      },
      "description": "Does not contain type as the enduser can only create 'custom' policies."
    },
    "v2betaCreatePolicyResp": {
      "type": "object",
      "properties": {
        "policy": {
          "$ref": "#/definitions/v2betaPolicy"
        }
      }
    },
    "v2betaCreateProjectReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        }
      }
    },
    "v2betaCreateProjectResp": {
      "type": "object",
      "properties": {
        "project": {
          "$ref": "#/definitions/v2betaProject"
        }
      }
    },
    "v2betaCreateRoleReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      },
      "description": "Does not contain type as the enduser can only create 'custom' roles."
    },
    "v2betaCreateRoleResp": {
      "type": "object",
      "properties": {
        "role": {
          "$ref": "#/definitions/v2betaRole"
        }
      }
    },
    "v2betaDeletePolicyResp": {
      "type": "object"
    },
    "v2betaDeleteProjectResp": {
      "type": "object"
    },
    "v2betaDeleteRoleResp": {
      "type": "object"
    },
    "v2betaFlag": {
      "type": "string",
      "enum": [
        "VERSION_2_0",
        "VERSION_2_1"
      ],
      "default": "VERSION_2_0",
      "title": "passed to UpgradeToV2 to set version"
    },
    "v2betaGetPolicyResp": {
      "type": "object",
      "properties": {
        "policy": {
          "$ref": "#/definitions/v2betaPolicy"
        }
      }
    },
    "v2betaGetPolicyVersionResp": {
      "type": "object",
      "properties": {
        "version": {
          "$ref": "#/definitions/v2betaVersion"
        }
      }
    },
    "v2betaGetProjectResp": {
      "type": "object",
      "properties": {
        "project": {
          "$ref": "#/definitions/v2betaProject"
        }
      }
    },
    "v2betaGetRoleResp": {
      "type": "object",
      "properties": {
        "role": {
          "$ref": "#/definitions/v2betaRole"
        }
      }
    },
    "v2betaListPoliciesResp": {
      "type": "object",
      "properties": {
        "policies": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaPolicy"
          }
        }
      }
    },
    "v2betaListPolicyMembersResp": {
      "type": "object",
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaListProjectsResp": {
      "type": "object",
      "properties": {
        "projects": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaProject"
          }
        }
      }
    },
    "v2betaListRolesResp": {
      "type": "object",
      "properties": {
        "roles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaRole"
          }
        }
      }
    },
    "v2betaPolicy": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/iamv2betaType"
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "statements": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaStatement"
          }
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaProject": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/iamv2betaType"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaRemovePolicyMembersReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaRemovePolicyMembersResp": {
      "type": "object",
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaReplacePolicyMembersReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaReplacePolicyMembersResp": {
      "type": "object",
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaResetToV1Resp": {
      "type": "object"
    },
    "v2betaRole": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/iamv2betaType"
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaStatement": {
      "type": "object",
      "properties": {
        "effect": {
          "$ref": "#/definitions/StatementEffect"
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "title": "inline definitions"
        },
        "role": {
          "type": "string",
          "title": "references"
        },
        "resources": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "title": "Note: these are for display only, not to be set in CreatePolicy/UpdatePolicy"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaUpdatePolicyReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "statements": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/v2betaStatement"
          }
        },
        "name": {
          "type": "string"
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      },
      "description": "Does not contain type as the enduser can only create 'custom' policies."
    },
    "v2betaUpdatePolicyResp": {
      "type": "object",
      "properties": {
        "policy": {
          "$ref": "#/definitions/v2betaPolicy"
        }
      }
    },
    "v2betaUpdateProjectReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        }
      }
    },
    "v2betaUpdateProjectResp": {
      "type": "object",
      "properties": {
        "project": {
          "$ref": "#/definitions/v2betaProject"
        }
      }
    },
    "v2betaUpdateRoleReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          }
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaUpdateRoleResp": {
      "type": "object",
      "properties": {
        "role": {
          "$ref": "#/definitions/v2betaRole"
        }
      }
    },
    "v2betaUpgradeToV2Resp": {
      "type": "object",
      "properties": {
        "reports": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "v2betaVersion": {
      "type": "object",
      "properties": {
        "major": {
          "$ref": "#/definitions/VersionVersionNumber"
        },
        "minor": {
          "$ref": "#/definitions/VersionVersionNumber"
        }
      },
      "title": "the only values that may be returned by GetPolicyVersion"
    }
  }
}
`)
}
