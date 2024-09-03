package api

func init() {
	Swagger.Add("policy", `{
  "swagger": "2.0",
  "info": {
    "title": "external/iam/v2/policy.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/apis/iam/v2/introspect_projects": {
      "get": {
        "operationId": "Policies_IntrospectAllProjects",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListProjectsResp"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "hidden"
        ]
      }
    },
    "/apis/iam/v2/policies": {
      "get": {
        "summary": "Lists all policies",
        "description": "Lists all policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policies:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_ListPolicies",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListPoliciesResp"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "policies"
        ]
      },
      "post": {
        "summary": "Creates a custom policy",
        "description": "Creates a custom IAM policy used to control permissions in Automate.\nA policy is composed of one or more statements that grant permissions to a set of members.\nEach statement contains a role as well as a list of projects.\n\nThe role defines a set of actions that the statement is scoped to.\nThe project list defines the set of resources that the statement is scoped to.\nPass ` + "`" + `\"projects\": [\"*\"]` + "`" + ` to scope a statement to every project.\n\nA policy's *top-level* projects list defines which projects the policy belongs to (for filtering policies by their projects),\nwhereas the *statement-level* projects list defines which projects the statement applies to.\n\nThe example creates a new policy not associated with any project (because the top-level ` + "`" + `projects` + "`" + ` property is empty) that grants the ` + "`" + `viewer` + "`" + ` role\non a few projects for all LDAP teams and a custom role ` + "`" + `qa` + "`" + ` on a specific project.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policies:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_CreatePolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreatePolicyResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreatePolicyReq"
            }
          }
        ],
        "tags": [
          "policies"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My Viewer Policy\",\"id\": \"custom-viewer-policy\",\"members\": [\"team:ldap:*\"], \"statements\": [{\"role\": \"viewer\",\"projects\": [\"project1\", \"project2\"], \"effect\": \"ALLOW\"},{\"role\": \"qa\",\"projects\": [\"acceptanceProject\"], \"effect\": \"ALLOW\"}],\"projects\": []}"
          }
        ]
      }
    },
    "/apis/iam/v2/policies/{id}": {
      "get": {
        "summary": "Gets a policy",
        "description": "Returns the details for a policy.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policies:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_GetPolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetPolicyResp"
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
            "description": "ID of the policy.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "policies"
        ]
      },
      "delete": {
        "summary": "Deletes a custom policy",
        "description": "Deletes a specified custom policy. You cannot delete Chef-managed policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policies:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_DeletePolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeletePolicyResp"
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
            "description": "ID of the policy.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "policies"
        ]
      },
      "put": {
        "summary": "Updates a custom policy",
        "description": "This operation overwrites all fields excepting ID,\nincluding those omitted from the request, so be sure to specify all properties.\nProperties that you do not include are reset to empty values.\nThe only exception is the policy ID, which is immutable; it can only be set at creation time.\n\nWhile you can use this endpoint to update members on a policy, if that is the only\nproperty you wish to modify you might find it more convenient to use one of these endpoints instead:\nAdd policy members, Remove policy members, or Replace policy members.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policies:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_UpdatePolicy",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdatePolicyResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdatePolicyReq"
            }
          }
        ],
        "tags": [
          "policies"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My Updated Viewer Policy\", \"members\": [\"user:ldap:newuser\", \"team:ldap:newteam\"], \"statements\": [{\"role\": \"viewer\",\"projects\":[\"project1\", \"project2\"], \"effect\": \"ALLOW\"},{\"role\": \"qa\",\"projects\": [\"acceptanceProject\"], \"effect\": \"ALLOW\"}],\"projects\": []}"
          }
        ]
      }
    },
    "/apis/iam/v2/policies/{id}/members": {
      "get": {
        "summary": "Lists policy members",
        "description": "Lists all members of a specific policy.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policyMembers:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_ListPolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListPolicyMembersResp"
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
            "description": "ID of the policy.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "policies"
        ]
      },
      "put": {
        "summary": "Replaces policy members",
        "description": "Replaces the entire member list of a specific policy with a new list.\nYou may use this endpoint to update members of either Custom or Chef-managed policies.\n\nEnsure each element of the members array is in the correct\n[Member Expression](https://automate.chef.io/docs/iam-v2-guide/#member-expressions) format.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policyMembers:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_ReplacePolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ReplacePolicyMembersResp"
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
            "description": "ID of the policy.",
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
          "policies"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"members\": [\"team:local:viewers\", \"user:local:test\"]}"
          }
        ]
      }
    },
    "/apis/iam/v2/policies/{id}/members:add": {
      "post": {
        "summary": "Adds policy members",
        "description": "Adds members to the member list of a specific policy.\nYou may use this endpoint to update members of either Custom or Chef-managed policies.\n\nEnsure each element of the members array is in the correct\n[Member Expression](https://automate.chef.io/docs/iam-v2-guide/#member-expressions) format.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policyMembers:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_AddPolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddPolicyMembersResp"
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
            "description": "ID of the policy.",
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
          "policies"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"members\": [\"team:local:viewers\", \"user:local:test\"]}"
          }
        ]
      }
    },
    "/apis/iam/v2/policies/{id}/members:remove": {
      "post": {
        "summary": "Removes policy members",
        "description": "Removes members from the member list of a specific policy. Silently ignores\nmembers that are not already part of the member list.\nYou may use this endpoint to update members of either Custom or Chef-managed policies.\n\nEnsure each element of the members array is in the correct\n[Member Expression](https://automate.chef.io/docs/iam-v2-guide/#member-expressions) format.\n\nThe removed members will still exist within Chef Automate, but are no longer associated with this policy.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policyMembers:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_RemovePolicyMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemovePolicyMembersResp"
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
            "description": "ID of the policy.",
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
          "policies"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"members\": [\"team:local:viewers\", \"user:local:test\"]}"
          }
        ]
      }
    },
    "/apis/iam/v2/policy_version": {
      "get": {
        "summary": "Gets IAM version",
        "description": "Returns the major and minor version of IAM that your automate installation is running.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:policies:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_GetPolicyVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetPolicyVersionResp"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "policies"
        ]
      }
    },
    "/apis/iam/v2/projects": {
      "get": {
        "summary": "Lists all projects",
        "description": "Lists all projects.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_ListProjects",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListProjectsResp"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "projects"
        ]
      },
      "post": {
        "summary": "Creates a project",
        "description": "Creates a new project to be used in the policies that control permissions in Automate.\n\nA project defines the scope of resources in a policy statement. Resources can be in more than one project.\n\nWhen a project is created, the system also creates three policies associated with the new project,\none for each of the following roles: editor, viewer, and project owner.\nYou can optionally pass the ` + "`" + `skip_policies` + "`" + ` flag set to ` + "`" + `true` + "`" + ` to skip the creation of these policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_CreateProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateProjectResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateProjectReq"
            }
          }
        ],
        "tags": [
          "projects"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My Custom Project\", \"id\": \"custom-project\", \"skip_policies\": true}"
          }
        ]
      }
    },
    "/apis/iam/v2/projects/{id}": {
      "get": {
        "summary": "Gets a project",
        "description": "Returns the details for a project.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_GetProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetProjectResp"
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
            "description": "ID of the project.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "projects"
        ]
      },
      "delete": {
        "summary": "Deletes a project",
        "description": "Deletes a project from any resources tagged with it.\n\nAlso deletes this project from any project list in any policy statements.\nIf the resulting project list for a given statement is empty, it is deleted.\nIf the resulting policy has no statements, it is also deleted.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_DeleteProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteProjectResp"
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
            "description": "ID of the project.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "projects"
        ]
      },
      "put": {
        "summary": "Updates a project",
        "description": "Updates the name of an existing project.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:projects:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_UpdateProject",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateProjectResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateProjectReq"
            }
          }
        ],
        "tags": [
          "projects"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My Custom Updated Project Name\"}"
          }
        ]
      }
    },
    "/apis/iam/v2/roles": {
      "get": {
        "summary": "Lists all roles",
        "description": "Lists all *Chef-managed* and *Custom* roles.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:roles:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_ListRoles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListRolesResp"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "tags": [
          "roles"
        ]
      },
      "post": {
        "summary": "Creates a custom role",
        "description": "Creates a new role to be used in the policies that control permissions in Automate.\n\nA role defines the scope of actions in a policy statement.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:roles:create\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_CreateRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRoleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateRoleReq"
            }
          }
        ],
        "tags": [
          "roles"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"id\": \"custom-role\", \"name\": \"My Custom Secret Manager Role\", \"actions\": [\"secrets:*\", \"iam:projects:list\"]}"
          }
        ]
      }
    },
    "/apis/iam/v2/roles/{id}": {
      "get": {
        "summary": "Gets a role",
        "description": "Returns the details for a role.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:roles:get\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_GetRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetRoleResp"
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
            "description": "ID of the role.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "roles"
        ]
      },
      "delete": {
        "summary": "Deletes a custom role",
        "description": "Deletes a specified custom role (you cannot delete Chef-managed roles) and remove it from any statements that may have been using it.\nIf such a statement has no other associated actions, the statement is deleted as well.\nSimilarly, if that statement removal results in a policy with no other statements,\nthat policy is removed as well.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:roles:delete\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_DeleteRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteRoleResp"
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
            "description": "ID of the role.",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "roles"
        ]
      },
      "put": {
        "summary": "Updates a custom role",
        "description": "This operation overwrites all fields excepting ID,\nincluding those omitted from the request, so be sure to specify all properties.\nProperties that you do not include are reset to empty values.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:roles:update\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "Policies_UpdateRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRoleResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateRoleReq"
            }
          }
        ],
        "tags": [
          "roles"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My Updated Custom Secret Manager Role\", \"actions\": [\"secrets:*\", \"iam:projects:list\"]}"
          }
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.AddPolicyMembersReq": {
      "type": "object",
      "example": {
        "members": [
          "team:local:viewers",
          "user:local:test"
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the policy."
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of members to add to the policy."
        }
      },
      "required": [
        "id",
        "members"
      ]
    },
    "chef.automate.api.iam.v2.AddPolicyMembersResp": {
      "type": "object",
      "example": {
        "members": [
          "team:local:viewers",
          "user:local:test"
        ]
      },
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
      "example": {
        "name": "My Viewer Policy",
        "id": "custom-viewer-policy",
        "members": [
          "team:ldap:*"
        ],
        "statements": [
          {
            "role": "viewer",
            "projects": [
              "project1",
              "project2"
            ],
            "effect": "ALLOW"
          },
          {
            "role": "qa",
            "projects": [
              "acceptanceProject"
            ],
            "effect": "ALLOW"
          }
        ],
        "projects": []
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the policy."
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Members affected by this policy."
        },
        "statements": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Statement"
          },
          "description": "Statements for the policy."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this policy belongs to."
        }
      },
      "description": "Does not contain type as the enduser can only create 'custom' policies.",
      "required": [
        "id",
        "name",
        "statements"
      ]
    },
    "chef.automate.api.iam.v2.CreatePolicyResp": {
      "type": "object",
      "example": {
        "name": "My Viewer Policy",
        "id": "custom-viewer-policy",
        "members": [
          "team:ldap:*"
        ],
        "statements": [
          {
            "role": "viewer",
            "projects": [
              "project1",
              "project2"
            ],
            "effect": "ALLOW"
          },
          {
            "role": "qa",
            "projects": [
              "acceptanceProject"
            ],
            "effect": "ALLOW"
          }
        ],
        "projects": []
      },
      "properties": {
        "policy": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
        }
      }
    },
    "chef.automate.api.iam.v2.CreateProjectReq": {
      "type": "object",
      "example": {
        "name": "My Custom Project",
        "id": "custom-project",
        "skip_policies": true
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the new project."
        },
        "skip_policies": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean flag to skip adding policies associated with the project. Set to false by default."
        }
      },
      "required": [
        "id",
        "name"
      ]
    },
    "chef.automate.api.iam.v2.CreateProjectResp": {
      "type": "object",
      "example": {
        "name": "My Custom Project",
        "id": "custom-project"
      },
      "properties": {
        "project": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
        }
      }
    },
    "chef.automate.api.iam.v2.CreateRoleReq": {
      "type": "object",
      "example": {
        "id": "custom-role",
        "name": "My Custom Secret Manager Role",
        "actions": [
          "secrets:*",
          "iam:projects:list"
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the role."
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of actions that this role scopes to."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this role belongs to."
        }
      },
      "description": "Does not contain type as the enduser can only create 'custom' roles.",
      "required": [
        "id",
        "name",
        "actions"
      ]
    },
    "chef.automate.api.iam.v2.CreateRoleResp": {
      "type": "object",
      "example": {
        "id": "custom-role",
        "name": "My Custom Secret Manager Role",
        "actions": [
          "secrets:*",
          "iam:projects:list"
        ]
      },
      "properties": {
        "role": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Role"
        }
      }
    },
    "chef.automate.api.iam.v2.DeletePolicyResp": {
      "type": "object",
      "example": {
        "name": "My Viewer Policy",
        "id": "custom-viewer-policy",
        "members": [
          "team:ldap:*"
        ],
        "statements": [
          {
            "role": "viewer",
            "projects": [
              "project1",
              "project2"
            ],
            "effect": "ALLOW"
          },
          {
            "role": "qa",
            "projects": [
              "acceptanceProject"
            ],
            "effect": "ALLOW"
          }
        ],
        "projects": []
      }
    },
    "chef.automate.api.iam.v2.DeleteProjectResp": {
      "type": "object",
      "example": {
        "name": "My Custom Project",
        "id": "custom-project"
      }
    },
    "chef.automate.api.iam.v2.DeleteRoleResp": {
      "type": "object",
      "example": {
        "id": "custom-role",
        "name": "My Custom Secret Manager Role",
        "actions": [
          "secrets:*",
          "iam:projects:list"
        ]
      }
    },
    "chef.automate.api.iam.v2.GetPolicyResp": {
      "type": "object",
      "example": {
        "name": "My Viewer Policy",
        "id": "custom-viewer-policy",
        "members": [
          "team:ldap:*"
        ],
        "statements": [
          {
            "role": "viewer",
            "projects": [
              "project1",
              "project2"
            ],
            "effect": "ALLOW"
          },
          {
            "role": "qa",
            "projects": [
              "acceptanceProject"
            ],
            "effect": "ALLOW"
          }
        ],
        "projects": []
      },
      "properties": {
        "policy": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
        }
      }
    },
    "chef.automate.api.iam.v2.GetPolicyVersionResp": {
      "type": "object",
      "example": {
        "version": {
          "major": "V2",
          "minor": "V1"
        }
      },
      "properties": {
        "version": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Version"
        }
      }
    },
    "chef.automate.api.iam.v2.GetProjectResp": {
      "type": "object",
      "example": {
        "name": "My Custom Project",
        "id": "custom-project"
      },
      "properties": {
        "project": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
        }
      }
    },
    "chef.automate.api.iam.v2.GetRoleResp": {
      "type": "object",
      "example": {
        "id": "custom-role",
        "name": "My Custom Secret Manager Role",
        "actions": [
          "secrets:*",
          "iam:projects:list"
        ]
      },
      "properties": {
        "role": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Role"
        }
      }
    },
    "chef.automate.api.iam.v2.ListPoliciesResp": {
      "type": "object",
      "example": {
        "policies": [
          {
            "name": "My Viewer Policy 1",
            "id": "custom-viewer-policy-1",
            "members": [
              "team:ldap:*"
            ],
            "statements": [
              {
                "role": "viewer",
                "projects": [
                  "project1",
                  "project2"
                ],
                "effect": "ALLOW"
              },
              {
                "role": "qa",
                "projects": [
                  "acceptanceProject"
                ],
                "effect": "ALLOW"
              }
            ],
            "projects": []
          },
          {
            "name": "My Custom Policy 2",
            "id": "custom-policy-2",
            "members": [
              "team:local:test"
            ],
            "statements": [
              {
                "role": "auditor",
                "projects": [
                  "project1"
                ],
                "effect": "ALLOW"
              }
            ],
            "projects": []
          }
        ]
      },
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
      "example": {
        "members": [
          "team:local:viewers",
          "user:local:test"
        ]
      },
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of policy members."
        }
      }
    },
    "chef.automate.api.iam.v2.ListProjectsResp": {
      "type": "object",
      "example": {
        "projects": [
          {
            "name": "My Custom Project",
            "id": "custom-project"
          },
          {
            "name": "My Custom Project 2",
            "id": "custom-project-2"
          }
        ]
      },
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
      "example": {
        "roles": [
          {
            "id": "custom-role",
            "name": "My Custom Secret Manager Role",
            "actions": [
              "secrets:*",
              "iam:projects:list"
            ]
          },
          {
            "id": "custom-role-2",
            "name": "My Custom Secret Role 2",
            "actions": [
              "infra:*"
            ]
          }
        ]
      },
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
          "type": "string",
          "description": "Name for the policy."
        },
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Type",
          "description": "This doc-comment is ignored for an enum."
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Members affected by this policy. May be empty."
        },
        "statements": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Statement"
          },
          "description": "Statements for the policy. Will contain one or more."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this policy belongs to. May be empty."
        }
      }
    },
    "chef.automate.api.iam.v2.Project": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name for the project."
        },
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Type",
          "description": "Whether this policy is user created (` + "`" + `CUSTOM` + "`" + `) or chef managed (` + "`" + `CHEF_MANAGED` + "`" + `)."
        },
        "status": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.ProjectRulesStatus",
          "description": "The current status of the rules for this project."
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
      "example": {
        "members": [
          "user:local:test"
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the policy."
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of members to remove from the policy."
        }
      },
      "required": [
        "id",
        "members"
      ]
    },
    "chef.automate.api.iam.v2.RemovePolicyMembersResp": {
      "type": "object",
      "example": {
        "members": [
          "user:local:test"
        ]
      },
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Resulting list of policy members."
        }
      }
    },
    "chef.automate.api.iam.v2.ReplacePolicyMembersReq": {
      "type": "object",
      "example": {
        "members": [
          "team:local:viewers",
          "user:local:test"
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the policy."
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of members that replaces previous policy member list."
        }
      },
      "required": [
        "id"
      ]
    },
    "chef.automate.api.iam.v2.ReplacePolicyMembersResp": {
      "type": "object",
      "example": {
        "members": [
          "team:local:viewers",
          "user:local:test"
        ]
      },
      "properties": {
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Resulting list of policy members."
        }
      }
    },
    "chef.automate.api.iam.v2.Role": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name for the role."
        },
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "type": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Type",
          "description": "Whether this policy is user created (` + "`" + `CUSTOM` + "`" + `) or chef managed (` + "`" + `CHEF_MANAGED` + "`" + `)."
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of actions this role scopes to. Will contain one or more."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this role belongs to. May be empty."
        }
      }
    },
    "chef.automate.api.iam.v2.Statement": {
      "type": "object",
      "properties": {
        "effect": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Statement.Effect",
          "description": "This doc-comment is ignored for an enum."
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Actions defined inline. May be empty.\nBest practices recommend that you use custom roles rather than inline actions where practical."
        },
        "role": {
          "type": "string",
          "description": "The role defines a set of actions that the statement is scoped to."
        },
        "resources": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "DEPRECATED: Resources defined inline. Use projects instead."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "The project list defines the set of resources that the statement is scoped to. May be empty."
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
      "example": {
        "name": "My Updated Viewer Policy",
        "members": [
          "user:ldap:newuser",
          "team:ldap:newteam"
        ],
        "statements": [
          {
            "role": "viewer",
            "projects": [
              "project1",
              "project2"
            ],
            "effect": "ALLOW"
          },
          {
            "role": "qa",
            "projects": [
              "acceptanceProject"
            ],
            "effect": "ALLOW"
          }
        ],
        "projects": []
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "members": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Members affected by this policy."
        },
        "statements": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Statement"
          },
          "description": "Statements for the policy."
        },
        "name": {
          "type": "string",
          "description": "Name for this policy."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this policy belongs to."
        }
      },
      "description": "Does not contain type as the enduser can only create 'custom' policies.",
      "required": [
        "id",
        "name",
        "statements"
      ]
    },
    "chef.automate.api.iam.v2.UpdatePolicyResp": {
      "type": "object",
      "example": {
        "name": "My Updated Viewer Policy",
        "members": [
          "user:ldap:newuser",
          "team:ldap:newteam"
        ],
        "statements": [
          {
            "role": "viewer",
            "projects": [
              "project1",
              "project2"
            ],
            "effect": "ALLOW"
          },
          {
            "role": "qa",
            "projects": [
              "acceptanceProject"
            ],
            "effect": "ALLOW"
          }
        ],
        "projects": []
      },
      "properties": {
        "policy": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Policy"
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateProjectReq": {
      "type": "object",
      "example": {
        "name": "My Custom Project"
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the project."
        }
      },
      "required": [
        "id",
        "name"
      ]
    },
    "chef.automate.api.iam.v2.UpdateProjectResp": {
      "type": "object",
      "example": {
        "name": "My Custom Project",
        "id": "custom-project"
      },
      "properties": {
        "project": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Project"
        }
      }
    },
    "chef.automate.api.iam.v2.UpdateRoleReq": {
      "type": "object",
      "example": {
        "name": "My Updated Custom Secret Manager Role",
        "actions": [
          "secrets:*",
          "iam:projects:list"
        ]
      },
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique ID. Cannot be changed."
        },
        "name": {
          "type": "string",
          "description": "Name for the role."
        },
        "actions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of actions that this role scopes to."
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of projects this role belongs to."
        }
      },
      "required": [
        "id",
        "name",
        "actions"
      ]
    },
    "chef.automate.api.iam.v2.UpdateRoleResp": {
      "type": "object",
      "example": {
        "id": "custom-role",
        "name": "My Custom Secret Manager Role",
        "actions": [
          "secrets:*",
          "iam:projects:list"
        ]
      },
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
    },
    "google.protobuf.Any": {
      "type": "object",
      "properties": {
        "type_url": {
          "type": "string",
          "description": "A URL/resource name that uniquely identifies the type of the serialized\nprotocol buffer message. This string must contain at least\none \"/\" character. The last segment of the URL's path must represent\nthe fully qualified name of the type (as in\n` + "`" + `path/google.protobuf.Duration` + "`" + `). The name should be in a canonical form\n(e.g., leading \".\" is not accepted).\n\nIn practice, teams usually precompile into the binary all types that they\nexpect it to use in the context of Any. However, for URLs which use the\nscheme ` + "`" + `http` + "`" + `, ` + "`" + `https` + "`" + `, or no scheme, one can optionally set up a type\nserver that maps type URLs to message definitions as follows:\n\n* If no scheme is provided, ` + "`" + `https` + "`" + ` is assumed.\n* An HTTP GET on the URL must yield a [google.protobuf.Type][]\n  value in binary format, or produce an error.\n* Applications are allowed to cache lookup results based on the\n  URL, or have them precompiled into a binary to avoid any\n  lookup. Therefore, binary compatibility needs to be preserved\n  on changes to types. (Use versioned type names to manage\n  breaking changes.)\n\nNote: this functionality is not currently available in the official\nprotobuf release, and it is not used for type URLs beginning with\ntype.googleapis.com.\n\nSchemes other than ` + "`" + `http` + "`" + `, ` + "`" + `https` + "`" + ` (or the empty scheme) might be\nused with implementation specific semantics."
        },
        "value": {
          "type": "string",
          "format": "byte",
          "description": "Must be a valid serialized protocol buffer of the above specified type."
        }
      },
      "description": "` + "`" + `Any` + "`" + ` contains an arbitrary serialized protocol buffer message along with a\nURL that describes the type of the serialized message.\n\nProtobuf library provides support to pack/unpack Any values in the form\nof utility functions or additional generated methods of the Any type.\n\nExample 1: Pack and unpack a message in C++.\n\n    Foo foo = ...;\n    Any any;\n    any.PackFrom(foo);\n    ...\n    if (any.UnpackTo(\u0026foo)) {\n      ...\n    }\n\nExample 2: Pack and unpack a message in Java.\n\n    Foo foo = ...;\n    Any any = Any.pack(foo);\n    ...\n    if (any.is(Foo.class)) {\n      foo = any.unpack(Foo.class);\n    }\n\n Example 3: Pack and unpack a message in Python.\n\n    foo = Foo(...)\n    any = Any()\n    any.Pack(foo)\n    ...\n    if any.Is(Foo.DESCRIPTOR):\n      any.Unpack(foo)\n      ...\n\n Example 4: Pack and unpack a message in Go\n\n     foo := \u0026pb.Foo{...}\n     any, err := anypb.New(foo)\n     if err != nil {\n       ...\n     }\n     ...\n     foo := \u0026pb.Foo{}\n     if err := any.UnmarshalTo(foo); err != nil {\n       ...\n     }\n\nThe pack methods provided by protobuf library will by default use\n'type.googleapis.com/full.type.name' as the type URL and the unpack\nmethods only use the fully qualified type name after the last '/'\nin the type URL, for example \"foo.bar.com/x/y.z\" will yield type\nname \"y.z\".\n\n\nJSON\n====\nThe JSON representation of an ` + "`" + `Any` + "`" + ` value uses the regular\nrepresentation of the deserialized, embedded message, with an\nadditional field ` + "`" + `@type` + "`" + ` which contains the type URL. Example:\n\n    package google.profile;\n    message Person {\n      string first_name = 1;\n      string last_name = 2;\n    }\n\n    {\n      \"@type\": \"type.googleapis.com/google.profile.Person\",\n      \"firstName\": \u003cstring\u003e,\n      \"lastName\": \u003cstring\u003e\n    }\n\nIf the embedded message type is well-known and has a custom JSON\nrepresentation, that representation will be embedded adding a field\n` + "`" + `value` + "`" + ` which holds the custom JSON in addition to the ` + "`" + `@type` + "`" + `\nfield. Example (for message [google.protobuf.Duration][]):\n\n    {\n      \"@type\": \"type.googleapis.com/google.protobuf.Duration\",\n      \"value\": \"1.212s\"\n    }"
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
