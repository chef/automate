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
              "$ref": "#/definitions/responseTeams"
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
              "$ref": "#/definitions/responseCreateTeamResp"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/requestCreateTeamReq"
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
              "$ref": "#/definitions/versionVersionInfo"
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
              "$ref": "#/definitions/responseGetTeamResp"
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
              "$ref": "#/definitions/responseDeleteTeamResp"
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
              "$ref": "#/definitions/responseUpdateTeamResp"
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
              "$ref": "#/definitions/requestUpdateTeamReq"
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
              "$ref": "#/definitions/responseGetUsersResp"
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
              "$ref": "#/definitions/responseAddUsersResp"
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
              "$ref": "#/definitions/requestAddUsersReq"
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
              "$ref": "#/definitions/responseRemoveUsersResp"
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
              "$ref": "#/definitions/requestRemoveUsersReq"
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
              "$ref": "#/definitions/responseGetTeamsForUserResp"
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
    "requestAddUsersReq": {
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
    "requestCreateTeamReq": {
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
    "requestRemoveUsersReq": {
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
    "requestUpdateTeamReq": {
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
    "responseAddUsersResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/teamsresponseTeam"
        }
      }
    },
    "responseCreateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/teamsresponseTeam"
        }
      }
    },
    "responseDeleteTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/teamsresponseTeam"
        }
      }
    },
    "responseGetTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/teamsresponseTeam"
        }
      }
    },
    "responseGetTeamsForUserResp": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/teamsresponseTeam"
          }
        }
      }
    },
    "responseGetUsersResp": {
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
    "responseRemoveUsersResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/teamsresponseTeam"
        }
      }
    },
    "responseTeams": {
      "type": "object",
      "properties": {
        "teams": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/teamsresponseTeam"
          }
        }
      }
    },
    "responseUpdateTeamResp": {
      "type": "object",
      "properties": {
        "team": {
          "$ref": "#/definitions/teamsresponseTeam"
        }
      }
    },
    "teamsresponseTeam": {
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
    "versionVersionInfo": {
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
    }
  }
}
`)
}
