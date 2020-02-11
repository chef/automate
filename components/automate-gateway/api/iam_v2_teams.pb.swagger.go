package api

func init() {
	Swagger.Add("iam_v2_teams", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2/teams.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/iam/v2/teams": {
      "get": {
        "operationId": "ListTeams",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListTeamsResp"
            }
          }
        },
        "tags": [
          "Teams"
        ]
      },
      "post": {
        "operationId": "CreateTeam",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateTeamResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateTeamReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2/teams/{id}": {
      "get": {
        "operationId": "GetTeam",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTeamResp"
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
          "Teams"
        ]
      },
      "delete": {
        "operationId": "DeleteTeam",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteTeamResp"
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
          "Teams"
        ]
      },
      "put": {
        "operationId": "UpdateTeam",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateTeamReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2/teams/{id}/users": {
      "get": {
        "operationId": "GetTeamMembership",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTeamMembershipResp"
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
          "Teams"
        ]
      }
    },
    "/iam/v2/teams/{id}/users:add": {
      "post": {
        "operationId": "AddTeamMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddTeamMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddTeamMembersReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2/teams/{id}/users:remove": {
      "post": {
        "operationId": "RemoveTeamMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemoveTeamMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemoveTeamMembersReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2/users/{id}/teams": {
      "get": {
        "operationId": "GetTeamsForMember",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTeamsForMemberResp"
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
          "Teams"
        ]
      }
    },
    "/iam/v2beta/teams": {
      "get": {
        "operationId": "ListTeams2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.ListTeamsResp"
            }
          }
        },
        "tags": [
          "Teams"
        ]
      },
      "post": {
        "operationId": "CreateTeam2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateTeamResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.CreateTeamReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2beta/teams/{id}": {
      "get": {
        "operationId": "GetTeam2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTeamResp"
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
          "Teams"
        ]
      },
      "delete": {
        "operationId": "DeleteTeam2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.DeleteTeamResp"
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
          "Teams"
        ]
      },
      "put": {
        "operationId": "UpdateTeam2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.UpdateTeamReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2beta/teams/{id}/users": {
      "get": {
        "operationId": "GetTeamMembership2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTeamMembershipResp"
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
          "Teams"
        ]
      }
    },
    "/iam/v2beta/teams/{id}/users:add": {
      "post": {
        "operationId": "AddTeamMembers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddTeamMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.AddTeamMembersReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2beta/teams/{id}/users:remove": {
      "post": {
        "operationId": "RemoveTeamMembers2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemoveTeamMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2.RemoveTeamMembersReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/iam/v2beta/users/{id}/teams": {
      "get": {
        "operationId": "GetTeamsForMember2",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2.GetTeamsForMemberResp"
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
          "Teams"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.AddTeamMembersReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.AddTeamMembersResp": {
      "type": "object",
      "properties": {
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.ApplyV2DataMigrationsResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.CreateTeamReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
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
      }
    },
    "chef.automate.api.iam.v2.CreateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    },
    "chef.automate.api.iam.v2.DeleteTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    },
    "chef.automate.api.iam.v2.GetTeamMembershipResp": {
      "type": "object",
      "properties": {
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.GetTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    },
    "chef.automate.api.iam.v2.GetTeamsForMemberResp": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.ListTeamsResp": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.RemoveTeamMembersReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.RemoveTeamMembersResp": {
      "type": "object",
      "properties": {
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.ResetAllTeamProjectsResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2.Team": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
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
      }
    },
    "chef.automate.api.iam.v2.UpdateTeamReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
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
      }
    },
    "chef.automate.api.iam.v2.UpdateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    }
  }
}
`)
}
