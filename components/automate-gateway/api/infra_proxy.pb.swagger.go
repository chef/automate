package api

func init() {
	Swagger.Add("infra_proxy", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/infra_proxy/infra_proxy.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/infra_proxy/servers": {
      "get": {
        "operationId": "GetServers",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.GetServers"
            }
          }
        },
        "tags": [
          "InfraProxy"
        ]
      },
      "post": {
        "operationId": "CreateServer",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CreateServer"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.request.CreateServer"
            }
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra_proxy/servers/{id}": {
      "get": {
        "operationId": "GetServer",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.GetServer"
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
          "InfraProxy"
        ]
      },
      "delete": {
        "operationId": "DeleteServer",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.DeleteServer"
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
          "InfraProxy"
        ]
      },
      "put": {
        "operationId": "UpdateServer",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.UpdateServer"
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
              "$ref": "#/definitions/chef.automate.api.infra_proxy.request.UpdateServer"
            }
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra_proxy/servers/{name}": {
      "get": {
        "operationId": "GetServerByName",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.GetServer"
            }
          }
        },
        "parameters": [
          {
            "name": "name",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra_proxy/servers/{server_id}/orgs": {
      "get": {
        "operationId": "GetOrgs",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.GetOrgs"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      },
      "post": {
        "operationId": "CreateOrg",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CreateOrg"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.request.CreateOrg"
            }
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra_proxy/servers/{server_id}/orgs/{id}": {
      "get": {
        "operationId": "GetOrg",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.GetOrg"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      },
      "delete": {
        "operationId": "DeleteOrg",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.DeleteOrg"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      },
      "put": {
        "operationId": "UpdateOrg",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.UpdateOrg"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
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
              "$ref": "#/definitions/chef.automate.api.infra_proxy.request.UpdateOrg"
            }
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra_proxy/servers/{server_id}/orgs/{name}": {
      "get": {
        "operationId": "GetOrgByName",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.GetOrg"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra_proxy/version": {
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
          "InfraProxy"
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
    "chef.automate.api.infra_proxy.request.CreateOrg": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "admin_user": {
          "type": "string"
        },
        "admin_key": {
          "type": "string"
        },
        "server_id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.request.CreateServer": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "description": {
          "type": "string"
        },
        "fqdn": {
          "type": "string"
        },
        "ip_address": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.request.UpdateOrg": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "admin_user": {
          "type": "string"
        },
        "admin_key": {
          "type": "string"
        },
        "server_id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.request.UpdateServer": {
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
        },
        "fqdn": {
          "type": "string"
        },
        "ip_address": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CreateOrg": {
      "type": "object",
      "properties": {
        "org": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Org"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CreateServer": {
      "type": "object",
      "properties": {
        "server": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Server"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.DeleteOrg": {
      "type": "object",
      "properties": {
        "org": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Org"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.DeleteServer": {
      "type": "object",
      "properties": {
        "server": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Server"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.GetOrg": {
      "type": "object",
      "properties": {
        "org": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Org"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.GetOrgs": {
      "type": "object",
      "properties": {
        "orgs": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Org"
          }
        }
      }
    },
    "chef.automate.api.infra_proxy.response.GetServer": {
      "type": "object",
      "properties": {
        "server": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Server"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.GetServers": {
      "type": "object",
      "properties": {
        "servers": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Server"
          }
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Org": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "admin_user": {
          "type": "string"
        },
        "admin_key": {
          "type": "string"
        },
        "server_id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Server": {
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
        },
        "fqdn": {
          "type": "string"
        },
        "ip_address": {
          "type": "string"
        },
        "orgs_count": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.UpdateOrg": {
      "type": "object",
      "properties": {
        "org": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Org"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.UpdateServer": {
      "type": "object",
      "properties": {
        "server": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Server"
        }
      }
    }
  }
}
`)
}
