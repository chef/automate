package api

func init() {
	Swagger.Add("iam_v2beta_teams", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/iam/v2beta/teams.proto",
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
    "/iam/v2beta/teams": {
      "get": {
        "operationId": "ListTeams",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.ListTeamsResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.CreateTeamResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.CreateTeamReq"
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
        "operationId": "GetTeam",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.GetTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.DeleteTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.UpdateTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.UpdateTeamReq"
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
        "operationId": "GetTeamMembership",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.GetTeamMembershipResp"
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
        "operationId": "AddTeamMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.AddTeamMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.AddTeamMembersReq"
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
        "operationId": "RemoveTeamMembers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.RemoveTeamMembersResp"
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
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.RemoveTeamMembersReq"
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
        "operationId": "GetTeamsForMember",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.iam.v2beta.GetTeamsForMemberResp"
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
    "chef.automate.api.iam.v2beta.AddTeamMembersReq": {
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
    "chef.automate.api.iam.v2beta.AddTeamMembersResp": {
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
    "chef.automate.api.iam.v2beta.ApplyV2DataMigrationsResp": {
      "type": "object"
    },
    "chef.automate.api.iam.v2beta.CreateTeamReq": {
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
    "chef.automate.api.iam.v2beta.CreateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Team"
        }
      }
    },
    "chef.automate.api.iam.v2beta.DeleteTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Team"
        }
      }
    },
    "chef.automate.api.iam.v2beta.GetTeamMembershipResp": {
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
    "chef.automate.api.iam.v2beta.GetTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Team"
        }
      }
    },
    "chef.automate.api.iam.v2beta.GetTeamsForMemberResp": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2beta.Team"
          }
        }
      }
    },
    "chef.automate.api.iam.v2beta.ListTeamsResp": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.iam.v2beta.Team"
          }
        }
      }
    },
    "chef.automate.api.iam.v2beta.RemoveTeamMembersReq": {
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
    "chef.automate.api.iam.v2beta.RemoveTeamMembersResp": {
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
    "chef.automate.api.iam.v2beta.Team": {
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
    "chef.automate.api.iam.v2beta.UpdateTeamReq": {
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
    "chef.automate.api.iam.v2beta.UpdateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2beta.Team"
        }
      }
    }
  }
}
`)
}
