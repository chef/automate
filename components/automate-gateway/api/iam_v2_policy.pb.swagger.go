package api

func init() {
	Swagger.Add("iam_v2_policy", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2/policy.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/iam/v2/introspect_projects": {
      "get": {
        "operationId": "IntrospectAllProjects",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListProjectsResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/policies": {
      "get": {
        "operationId": "ListPolicies",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListPoliciesResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreatePolicyResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreatePolicyReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/policies/{id}": {
      "get": {
        "operationId": "GetPolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetPolicyResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeletePolicyResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdatePolicyResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdatePolicyReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/policies/{id}/members": {
      "get": {
        "operationId": "ListPolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListPolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.ReplacePolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.ReplacePolicyMembersReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/policies/{id}/members:add": {
      "post": {
        "operationId": "AddPolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddPolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddPolicyMembersReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/policies/{id}/members:remove": {
      "post": {
        "operationId": "RemovePolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemovePolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemovePolicyMembersReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/policy_version": {
      "get": {
        "operationId": "GetPolicyVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetPolicyVersionResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/projects": {
      "get": {
        "operationId": "ListProjects",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListProjectsResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateProjectResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateProjectReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/projects/{id}": {
      "get": {
        "operationId": "GetProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetProjectResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteProjectResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateProjectResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateProjectReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/roles": {
      "get": {
        "operationId": "ListRoles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListRolesResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRoleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRoleReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2/roles/{id}": {
      "get": {
        "operationId": "GetRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetRoleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteRoleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRoleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRoleReq"
            }
          }
        ],
        "tags": [
          "Policies"
        ]
      }
    },
    "/iam/v2beta/introspect_projects": {
      "get": {
        "operationId": "IntrospectAllProjects2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListProjectsResp"
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
        "operationId": "ListPolicies2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListPoliciesResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      },
      "post": {
        "operationId": "CreatePolicy2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreatePolicyResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreatePolicyReq"
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
        "operationId": "GetPolicy2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetPolicyResp"
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
        "operationId": "DeletePolicy2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeletePolicyResp"
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
        "operationId": "UpdatePolicy2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdatePolicyResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdatePolicyReq"
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
        "operationId": "ListPolicyMembers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListPolicyMembersResp"
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
        "operationId": "ReplacePolicyMembers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ReplacePolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.ReplacePolicyMembersReq"
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
        "operationId": "AddPolicyMembers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddPolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddPolicyMembersReq"
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
        "operationId": "RemovePolicyMembers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemovePolicyMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemovePolicyMembersReq"
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
        "operationId": "GetPolicyVersion2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetPolicyVersionResp"
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
        "operationId": "ListProjects2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListProjectsResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      },
      "post": {
        "operationId": "CreateProject2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateProjectResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateProjectReq"
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
        "operationId": "GetProject2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetProjectResp"
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
        "operationId": "DeleteProject2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteProjectResp"
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
        "operationId": "UpdateProject2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateProjectResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateProjectReq"
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
        "operationId": "ListRoles2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListRolesResp"
            }
          }
        },
        "tags": [
          "Policies"
        ]
      },
      "post": {
        "operationId": "CreateRole2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRoleResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRoleReq"
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
        "operationId": "GetRole2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetRoleResp"
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
        "operationId": "DeleteRole2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteRoleResp"
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
        "operationId": "UpdateRole2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRoleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRoleReq"
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
    "chef.automate.api.iam.v2.AddPolicyMembersReq": {
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
    "chef.automate.api.iam.v2.AddPolicyMembersResp": {
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
    "chef.automate.api.iam.v2.CreatePolicyReq": {
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
            "$ref": "#/definitions/chef.automate.api.iam.v2.Statement"
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
    "chef.automate.api.iam.v2.CreatePolicyResp": {
      "type": "object",
      "properties": {
        "policy": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
        }
      }
    },
    "chef.automate.api.iam.v2.CreateProjectReq": {
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
    "chef.automate.api.iam.v2.CreateProjectResp": {
      "type": "object",
      "properties": {
        "project": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
        }
      }
    },
    "chef.automate.api.iam.v2.CreateRoleReq": {
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
    "chef.automate.api.iam.v2.CreateRoleResp": {
      "type": "object",
      "properties": {
        "role": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Role"
        }
      }
    },
    "chef.automate.api.iam.v2.DeletePolicyResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.DeleteProjectResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.DeleteRoleResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.GetPolicyResp": {
      "type": "object",
      "properties": {
        "policy": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
        }
      }
    },
    "chef.automate.api.iam.v2.GetPolicyVersionResp": {
      "type": "object",
      "properties": {
        "version": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Version"
        }
      }
    },
    "chef.automate.api.iam.v2.GetProjectResp": {
      "type": "object",
      "properties": {
        "project": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
        }
      }
    },
    "chef.automate.api.iam.v2.GetRoleResp": {
      "type": "object",
      "properties": {
        "role": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Role"
        }
      }
    },
    "chef.automate.api.iam.v2.ListPoliciesResp": {
      "type": "object",
      "properties": {
        "policies": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.ListPolicyMembersResp": {
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
    "chef.automate.api.iam.v2.ListProjectsResp": {
      "type": "object",
      "properties": {
        "projects": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.ListRolesResp": {
      "type": "object",
      "properties": {
        "roles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Role"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.Policy": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Type"
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
            "$ref": "#/definitions/chef.automate.api.iam.v2.Statement"
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
    "chef.automate.api.iam.v2.Project": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Type"
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
    "chef.automate.api.iam.v2.RemovePolicyMembersReq": {
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
    "chef.automate.api.iam.v2.RemovePolicyMembersResp": {
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
    "chef.automate.api.iam.v2.ReplacePolicyMembersReq": {
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
    "chef.automate.api.iam.v2.ReplacePolicyMembersResp": {
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
    "chef.automate.api.iam.v2.Role": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "id": {
          "type": "string"
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Type"
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
    "chef.automate.api.iam.v2.Statement": {
      "type": "object",
      "properties": {
        "effect": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Statement.Effect"
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
    "chef.automate.api.iam.v2.Statement.Effect": {
      "type": "string",
      "enum": [
        "ALLOW",
        "DENY"
      ],
      "default": "ALLOW"
    },
    "chef.automate.api.iam.v2.Type": {
      "type": "string",
      "enum": [
        "CHEF_MANAGED",
        "CUSTOM"
      ],
      "default": "CHEF_MANAGED"
    },
    "chef.automate.api.iam.v2.UpdatePolicyReq": {
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
            "$ref": "#/definitions/chef.automate.api.iam.v2.Statement"
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
    "chef.automate.api.iam.v2.UpdatePolicyResp": {
      "type": "object",
      "properties": {
        "policy": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateProjectReq": {
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
    "chef.automate.api.iam.v2.UpdateProjectResp": {
      "type": "object",
      "properties": {
        "project": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateRoleReq": {
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
    "chef.automate.api.iam.v2.UpdateRoleResp": {
      "type": "object",
      "properties": {
        "role": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Role"
        }
      }
    },
    "chef.automate.api.iam.v2.Version": {
      "type": "object",
      "properties": {
        "major": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Version.VersionNumber"
        },
        "minor": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Version.VersionNumber"
        }
      },
      "title": "the only values that may be returned by GetPolicyVersion"
    },
    "chef.automate.api.iam.v2.Version.VersionNumber": {
      "type": "string",
      "enum": [
        "V0",
        "V1",
        "V2"
      ],
      "default": "V0"
    }
  }
}
`)
}
