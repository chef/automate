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
    "/infra/servers": {
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
    "/infra/servers/{id}": {
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
    "/infra/servers/{name}": {
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
    "/infra/servers/{server_id}/orgs": {
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
    "/infra/servers/{server_id}/orgs/{id}": {
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
    "/infra/servers/{server_id}/orgs/{name}": {
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
    "/infra/servers/{server_id}/orgs/{org_id}/affected-nodes/{chef_type}/{name}": {
      "get": {
        "operationId": "GetAffectedNodes",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.AffectedNodes"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "ID of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "ID of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "chef_type",
            "description": "Type of the chef object (e.g. 'cookbooks', 'roles', 'chef_environment').",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the chef object.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "version",
            "description": "Version of the chef object.",
            "in": "query",
            "required": false,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra/servers/{server_id}/orgs/{org_id}/clients": {
      "get": {
        "operationId": "GetClients",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Clients"
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
            "name": "org_id",
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
    "/infra/servers/{server_id}/orgs/{org_id}/clients/{name}": {
      "get": {
        "operationId": "GetClient",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Client"
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
            "name": "org_id",
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
    "/infra/servers/{server_id}/orgs/{org_id}/cookbooks": {
      "get": {
        "operationId": "GetCookbooks",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Cookbooks"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "ID of the Server",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "ID of the Org.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/cookbooks/{name}": {
      "get": {
        "operationId": "GetCookbookVersions",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookVersions"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "ID of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "ID of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the cookbook.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/cookbooks/{name}/{version}": {
      "get": {
        "operationId": "GetCookbook",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Cookbook"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "ID of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "ID of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the cookbook.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "version",
            "description": "Version of the cookbook.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/cookbooks/{name}/{version}/file-content": {
      "get": {
        "operationId": "GetCookbookFileContent",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookFileContent"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "ID of the server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "ID of the org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the cookbook.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "version",
            "description": "Version of the cookbook.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "url",
            "description": "Cookbook data file URL.",
            "in": "query",
            "required": false,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}": {
      "get": {
        "operationId": "GetDataBags",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.DataBags"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "Id of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "Id of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the data bag.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item}": {
      "get": {
        "operationId": "GetDataBagItem",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.DataBag"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "Id of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "Id of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the data bag.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "item",
            "description": "Name of the data bag item.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/environments": {
      "get": {
        "operationId": "GetEnvironments",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Environments"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "Id of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "Id of the Org.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/environments/{name}": {
      "get": {
        "operationId": "GetEnvironment",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Environment"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "Id of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "Id of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the environment.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/policyfiles": {
      "get": {
        "operationId": "GetPolicyfiles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Policyfiles"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "Id of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "Id of the Org.",
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
    "/infra/servers/{server_id}/orgs/{org_id}/policyfiles/{name}": {
      "get": {
        "operationId": "GetPolicyfile",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Policyfile"
            }
          }
        },
        "parameters": [
          {
            "name": "server_id",
            "description": "Id of the Server.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "org_id",
            "description": "Id of the Org.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "name",
            "description": "Name of the policy file.",
            "in": "path",
            "required": true,
            "type": "string"
          },
          {
            "name": "revision_id",
            "description": "Revision ID of the policy file.",
            "in": "query",
            "required": false,
            "type": "string"
          }
        ],
        "tags": [
          "InfraProxy"
        ]
      }
    },
    "/infra/servers/{server_id}/orgs/{org_id}/roles": {
      "get": {
        "operationId": "GetRoles",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Roles"
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
            "name": "org_id",
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
    "/infra/servers/{server_id}/orgs/{org_id}/roles/{name}": {
      "get": {
        "operationId": "GetRole",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.infra_proxy.response.Role"
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
            "name": "org_id",
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
    "/infra/version": {
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
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
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
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
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
    "chef.automate.api.infra_proxy.response.AffectedNodes": {
      "type": "object",
      "properties": {
        "nodes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.NodeAttribute"
          },
          "description": "List of the nodes which are affected by the chef object."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Client": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "client_name": {
          "type": "string"
        },
        "org_name": {
          "type": "string"
        },
        "admin": {
          "type": "boolean",
          "format": "boolean"
        },
        "validator": {
          "type": "boolean",
          "format": "boolean"
        },
        "certificate": {
          "type": "string"
        },
        "public_key": {
          "type": "string"
        },
        "private_key": {
          "type": "string"
        },
        "uri": {
          "type": "string"
        },
        "json_class": {
          "type": "string"
        },
        "chef_type": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.ClientListItem": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Clients": {
      "type": "object",
      "properties": {
        "clients": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.ClientListItem"
          }
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Cookbook": {
      "type": "object",
      "properties": {
        "cookbook_name": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "chef_type": {
          "type": "string"
        },
        "frozen": {
          "type": "boolean",
          "format": "boolean"
        },
        "json_class": {
          "type": "string"
        },
        "files": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "templates": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "attributes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "recipes": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "definitions": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "libraries": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "providers": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "resources": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "root_files": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookItem"
          }
        },
        "metadata": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookMeta"
        },
        "access": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookAccess"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookAccess": {
      "type": "object",
      "properties": {
        "read": {
          "type": "boolean",
          "format": "boolean"
        },
        "create": {
          "type": "boolean",
          "format": "boolean"
        },
        "grant": {
          "type": "boolean",
          "format": "boolean"
        },
        "update": {
          "type": "boolean",
          "format": "boolean"
        },
        "delete": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookFileContent": {
      "type": "object",
      "properties": {
        "content": {
          "type": "string",
          "description": "Cookbook data file content."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookItem": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        },
        "path": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "checksum": {
          "type": "string"
        },
        "specificity": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookLock": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the cookbook."
        },
        "version": {
          "type": "string",
          "description": "Version of the cookbook."
        },
        "identifier": {
          "type": "string",
          "description": "Identifier for the cookbook."
        },
        "dotted_identifier": {
          "type": "string",
          "description": "Decimal number identifier for the cookbook."
        },
        "source": {
          "type": "string",
          "description": "Source of the cookbook."
        },
        "cache_key": {
          "type": "string",
          "description": "Cache key for the cookbook."
        },
        "SCMDetail": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.SCMDetail",
          "title": "SCM detail"
        },
        "source_options": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.SourceOptions",
          "description": "Source path of the cookbook."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookMeta": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "version": {
          "type": "string"
        },
        "description": {
          "type": "string"
        },
        "long_description": {
          "type": "string"
        },
        "maintainer": {
          "type": "string"
        },
        "maintainer_email": {
          "type": "string"
        },
        "license": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookVersion": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the cookbook."
        },
        "version": {
          "type": "string",
          "description": "Version of the cookbook."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.CookbookVersions": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the cookbook."
        },
        "versions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of all versions avaiable for cookbook."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Cookbooks": {
      "type": "object",
      "properties": {
        "cookbooks": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookVersion"
          },
          "description": "List of cookbooks with name and version."
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
    "chef.automate.api.infra_proxy.response.DataBag": {
      "type": "object",
      "properties": {
        "data": {
          "type": "string",
          "description": "Stringified json of the data bag item."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.DataBagListItem": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the data bag item."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.DataBags": {
      "type": "object",
      "properties": {
        "data_bags": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.DataBagListItem"
          },
          "description": "List of data bags item."
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
    "chef.automate.api.infra_proxy.response.Environment": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the Environment."
        },
        "chef_type": {
          "type": "string",
          "description": "Type of the object."
        },
        "description": {
          "type": "string",
          "description": "Description of the node."
        },
        "json_class": {
          "type": "string",
          "description": "Name of the class."
        },
        "cookbook_versions": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "title": "List of versioned cookbooks"
        },
        "default_attributes": {
          "type": "string",
          "description": "Stringified json of the default attributes."
        },
        "override_attributes": {
          "type": "string",
          "description": "Stringified json of the override attributes."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.EnvironmentListItem": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the Environment."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Environments": {
      "type": "object",
      "properties": {
        "environments": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.EnvironmentListItem"
          },
          "description": "List of the EnvironmentListItem."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.ExpandedRunList": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the run list collection."
        },
        "run_list": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.RunList"
          },
          "description": "List of the run list."
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
    "chef.automate.api.infra_proxy.response.IncludedPolicyLock": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the included policy file."
        },
        "revision_id": {
          "type": "string",
          "description": "Revision id of the included policy file."
        },
        "source_options": {
          "$ref": "#/definitions/chef.automate.api.infra_proxy.response.SourceOptions",
          "description": "Source options of the included policy file."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.NamedRunList": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the run list."
        },
        "run_list": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Run list associated with the policy."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.NodeAttribute": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "description": "ID of the node."
        },
        "name": {
          "type": "string",
          "description": "Name of the node."
        },
        "check_in": {
          "type": "string",
          "description": "Last checked in of the node."
        },
        "uptime": {
          "type": "string",
          "description": "Uptime of the node."
        },
        "platform": {
          "type": "string",
          "description": "Name of the platform of the node."
        },
        "environment": {
          "type": "string",
          "description": "Environment name of the node."
        },
        "policy_group": {
          "type": "string",
          "description": "Policy group name of the node."
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
        },
        "projects": {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Policyfile": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the policy file."
        },
        "policy_group": {
          "type": "string",
          "description": "Name of the policy group."
        },
        "revision_id": {
          "type": "string",
          "description": "Revision Id of the policy."
        },
        "run_list": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Run list associated with the policy."
        },
        "named_run_list": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.NamedRunList"
          },
          "description": "Named the run list associated with the policy."
        },
        "included_policy_locks": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.IncludedPolicyLock"
          },
          "description": "Included policy locks files."
        },
        "cookbook_locks": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.CookbookLock"
          },
          "description": "List of cookbook locks under this policy."
        },
        "default_attributes": {
          "type": "string",
          "description": "Stringified json of the default attributes."
        },
        "override_attributes": {
          "type": "string",
          "description": "Stringified json of the override attributes."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.PolicyfileListItem": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the policy file."
        },
        "revision_id": {
          "type": "string",
          "description": "Revision ID of the policy file."
        },
        "policy_group": {
          "type": "string",
          "description": "Policy group of the policy file."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Policyfiles": {
      "type": "object",
      "properties": {
        "policies": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.PolicyfileListItem"
          },
          "description": "List of policy files."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Role": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the role."
        },
        "chef_type": {
          "type": "string",
          "description": "Type of the chef object."
        },
        "description": {
          "type": "string",
          "description": "Descrption of the role."
        },
        "default_attributes": {
          "type": "string",
          "description": "Stringified json of the default attributes."
        },
        "override_attributes": {
          "type": "string",
          "description": "Stringified json of the override attributes."
        },
        "json_class": {
          "type": "string",
          "description": "Json class name."
        },
        "run_list": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Run list for the role."
        },
        "expanded_run_list": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.ExpandedRunList"
          },
          "description": "List of expanded run list for the role."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.RoleListItem": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the role."
        },
        "description": {
          "type": "string",
          "description": "Desscription of the role."
        },
        "environments": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "Environment for the role."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.Roles": {
      "type": "object",
      "properties": {
        "roles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.RoleListItem"
          },
          "description": "List of the roles item."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.RunList": {
      "type": "object",
      "properties": {
        "type": {
          "type": "string",
          "description": "Type of run list item (e.g. 'recipe')."
        },
        "name": {
          "type": "string",
          "description": "Name of run list item."
        },
        "version": {
          "type": "string",
          "description": "Version of run list item."
        },
        "skipped": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean denoting whether or not the run list item was skipped."
        },
        "children": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.infra_proxy.response.RunList"
          },
          "description": "List of the run list."
        }
      }
    },
    "chef.automate.api.infra_proxy.response.SCMDetail": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the SCM."
        },
        "remote": {
          "type": "string",
          "description": "Remote location of the SCM."
        },
        "revision": {
          "type": "string",
          "description": "Revision detail for the SCM."
        },
        "working_tree_clean": {
          "type": "boolean",
          "format": "boolean",
          "description": "Boolean that denotes whether or not the working tree is cleaned."
        },
        "published": {
          "type": "boolean",
          "format": "boolean",
          "description": "Published info of the source."
        },
        "synchronized_remote_branches": {
          "type": "array",
          "items": {
            "type": "string"
          },
          "description": "List of the synchronized remote branches."
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
    "chef.automate.api.infra_proxy.response.SourceOptions": {
      "type": "object",
      "properties": {
        "path": {
          "type": "string",
          "description": "Path of the source options."
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
