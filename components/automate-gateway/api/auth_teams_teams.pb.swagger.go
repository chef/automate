package api

func init() {
	Swagger.Add("auth_teams_teams", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/auth/teams/teams.proto",
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
    "/auth/teams": {
      "get": {
        "operationId": "GetTeams",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.response.Teams"
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
              "$ref": "#/definitions/chef.automate.api.teams.response.CreateTeamResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.request.CreateTeamReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/auth/teams/version": {
      "get": {
        "operationId": "GetVersion",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.common.version.VersionInfo"
            }
          }
        },
        "tags": [
          "Teams"
        ]
      }
    },
    "/auth/teams/{id}": {
      "get": {
        "operationId": "GetTeam",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.response.GetTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.teams.response.DeleteTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.teams.response.UpdateTeamResp"
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
              "$ref": "#/definitions/chef.automate.api.teams.request.UpdateTeamReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/auth/teams/{id}/users": {
      "get": {
        "operationId": "GetUsers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.response.GetUsersResp"
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
      "post": {
        "operationId": "AddUsers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.response.AddUsersResp"
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
              "$ref": "#/definitions/chef.automate.api.teams.request.AddUsersReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      },
      "put": {
        "operationId": "RemoveUsers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.response.RemoveUsersResp"
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
              "$ref": "#/definitions/chef.automate.api.teams.request.RemoveUsersReq"
            }
          }
        ],
        "tags": [
          "Teams"
        ]
      }
    },
    "/auth/users/{id}/teams": {
      "get": {
        "operationId": "GetTeamsForUser",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.teams.response.GetTeamsForUserResp"
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
    "chef.automate.api.common.version.VersionInfo": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "sha": {
          "type": "string"
        },
        "built": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.teams.request.AddUsersReq": {
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
    "chef.automate.api.teams.request.CreateTeamReq": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "description": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.teams.request.RemoveUsersReq": {
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
    "chef.automate.api.teams.request.UpdateTeamReq": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "description": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.teams.response.AddUsersResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.teams.response.Team"
        }
      }
    },
    "chef.automate.api.teams.response.CreateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.teams.response.Team"
        }
      }
    },
    "chef.automate.api.teams.response.DeleteTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.teams.response.Team"
        }
      }
    },
    "chef.automate.api.teams.response.GetTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.teams.response.Team"
        }
      }
    },
    "chef.automate.api.teams.response.GetTeamsForUserResp": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.teams.response.Team"
          }
        }
      }
    },
    "chef.automate.api.teams.response.GetUsersResp": {
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
    "chef.automate.api.teams.response.RemoveUsersResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.teams.response.Team"
        }
      }
    },
    "chef.automate.api.teams.response.Team": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "description": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.teams.response.Teams": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.teams.response.Team"
          }
        }
      }
    },
    "chef.automate.api.teams.response.UpdateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.teams.response.Team"
        }
      }
    }
  }
}
`)
}
