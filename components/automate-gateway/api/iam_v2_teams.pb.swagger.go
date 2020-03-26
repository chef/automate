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
        "summary": "Lists all local teams",
        "description": "Lists all local teams.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teams:list\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ]
      },
      "post": {
        "summary": "Creates a local team",
        "description": "Creates a local team that is used to group local users as members of IAM policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teams:create\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"id\": \"test-id\", \"name\": \"My Test Team\", \"projects\": [\"project1\", \"project2\"]}"
          }
        ]
      }
    },
    "/iam/v2/teams/{id}": {
      "get": {
        "summary": "Get a team",
        "description": "Returns the details for a team.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teams:get\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ]
      },
      "delete": {
        "summary": "Deletes a local team",
        "description": "Deletes a local team and removes it from any policies.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teams:delete\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ]
      },
      "put": {
        "summary": "Updates a local team",
        "description": "Updates a local team.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teams:update\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{\"name\": \"My Update Test Team\", \"projects\": [\"project1\", \"projectnew\"]}"
          }
        ]
      }
    },
    "/iam/v2/teams/{id}/users": {
      "get": {
        "summary": "Gets local team membership",
        "description": "Lists all users of a local team. Users are listed by their membership_id.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teamUsers:list\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ]
      }
    },
    "/iam/v2/teams/{id}/users:add": {
      "post": {
        "summary": "Adds local team membership",
        "description": "Adds a list of users to a local team. Users are added by their membership_id.\nThe request currently does not validate that membership_id maps to a real user.\n\nThe membership_id for users can be found via GET /apis/iam/v2/users/\u003cuser_id\u003e.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teamUsers:create\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{user_ids: [\"527ed96f-2ecb-4f8f-abd7-0bf6511459ac\", \"987c8475-5747-4f9b-a766-c337f73965ae\"]}"
          }
        ]
      }
    },
    "/iam/v2/teams/{id}/users:remove": {
      "post": {
        "summary": "Removes local team membership",
        "description": "Removes a list of users from a local team. Users are removed by their membership_id.\nThe request currently does not validate that membership_id maps to a real user.\n\nThe membership_id for users can be found via GET /apis/iam/v2/users/\u003cuser_id\u003e.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:teamUsers:delete\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ],
        "x-code-samples": [
          {
            "lang": "JSON",
            "source": "{user_ids: [\"527ed96f-2ecb-4f8f-abd7-0bf6511459ac\", \"987c8475-5747-4f9b-a766-c337f73965ae\"]}"
          }
        ]
      }
    },
    "/iam/v2/users/{id}/teams": {
      "get": {
        "summary": "Gets team membership for a user",
        "description": "Lists all local teams for a specific user. You must use their membership_id in the request URL.\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\niam:userTeams:get\n` + "`" + `` + "`" + `` + "`" + `",
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
          "teams"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.iam.v2.AddTeamMembersReq": {
      "type": "object",
      "example": {
        "user_ids": [
          "527ed96f-2ecb-4f8f-abd7-0bf6511459ac",
          "353a62d4-85fa-4423-b12a-f6608a562ae9"
        ]
      },
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
      },
      "required": [
        "id",
        "user_ids"
      ]
    },
    "chef.automate.api.iam.v2.AddTeamMembersResp": {
      "type": "object",
      "example": {
        "user_ids": [
          "527ed96f-2ecb-4f8f-abd7-0bf6511459ac",
          "353a62d4-85fa-4423-b12a-f6608a562ae9"
        ]
      },
      "properties": {
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.iam.v2.CreateTeamReq": {
      "type": "object",
      "example": {
        "id": "test-id",
        "name": "My Test Team",
        "projects": [
          "project1",
          "project2"
        ]
      },
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
      },
      "required": [
        "id",
        "name"
      ]
    },
    "chef.automate.api.iam.v2.CreateTeamResp": {
      "type": "object",
      "example": {
        "team": {
          "id": "test-id",
          "name": "My Test Team",
          "projects": [
            "project1",
            "project2"
          ]
        }
      },
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    },
    "chef.automate.api.iam.v2.DeleteTeamResp": {
      "type": "object",
      "example": {
        "team": {
          "id": "test-id",
          "name": "My Test Team",
          "projects": [
            "project1",
            "project2"
          ]
        }
      },
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    },
    "chef.automate.api.iam.v2.GetTeamMembershipResp": {
      "type": "object",
      "example": {
        "user_ids": [
          "527ed96f-2ecb-4f8f-abd7-0bf6511459ac",
          "353a62d4-85fa-4423-b12a-f6608a562ae9"
        ]
      },
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
      "example": {
        "team": {
          "id": "test-id",
          "name": "My Test Team",
          "projects": [
            "project1",
            "project2"
          ]
        }
      },
      "properties": {
        "team": {
          "$ref": "#/definitions/chef.automate.api.iam.v2.Team"
        }
      }
    },
    "chef.automate.api.iam.v2.GetTeamsForMemberResp": {
      "type": "object",
      "example": {
        "teams": [
          {
            "id": "test-1",
            "name": "My Test Team",
            "projects": [
              "project1",
              "project2"
            ]
          },
          {
            "id": "test-2",
            "name": "My Test Team 2",
            "projects": [
              "project1"
            ]
          }
        ]
      },
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
      "example": {
        "teams": [
          {
            "id": "test-1",
            "name": "My Test Team",
            "projects": [
              "project1",
              "project2"
            ]
          },
          {
            "id": "test-2",
            "name": "My Test Team 2",
            "projects": [
              "project1"
            ]
          }
        ]
      },
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
      "example": {
        "user_ids": [
          "527ed96f-2ecb-4f8f-abd7-0bf6511459ac",
          "353a62d4-85fa-4423-b12a-f6608a562ae9"
        ]
      },
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
      },
      "required": [
        "id",
        "user_ids"
      ]
    },
    "chef.automate.api.iam.v2.RemoveTeamMembersResp": {
      "type": "object",
      "example": {
        "user_ids": [
          "527ed96f-2ecb-4f8f-abd7-0bf6511459ac",
          "353a62d4-85fa-4423-b12a-f6608a562ae9"
        ]
      },
      "properties": {
        "user_ids": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
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
      "example": {
        "name": "My Test Team",
        "projects": [
          "project1",
          "project2"
        ]
      },
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
      },
      "required": [
        "id",
        "name"
      ]
    },
    "chef.automate.api.iam.v2.UpdateTeamResp": {
      "type": "object",
      "example": {
        "team": {
          "id": "test-id",
          "name": "My Test Team",
          "projects": [
            "project1",
            "project2"
          ]
        }
      },
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
