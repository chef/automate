package api

func init() {
	Swagger.Add("data_feed", `{
  "swagger": "2.0",
  "info": {
    "title": "api/external/data_feed/data_feed.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/datafeed/destination": {
      "post": {
        "operationId": "AddDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.AddDestinationResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.AddDestinationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/datafeed/destination/{id}": {
      "get": {
        "operationId": "GetDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.GetDestinationResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string",
            "format": "int64"
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      },
      "delete": {
        "operationId": "DeleteDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.DeleteDestinationResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string",
            "format": "int64"
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      },
      "patch": {
        "operationId": "UpdateDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.UpdateDestinationResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.UpdateDestinationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/datafeed/destinations": {
      "post": {
        "operationId": "ListDestinations",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.ListDestinationResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.ListDestinationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    },
    "/datafeed/destinations/test": {
      "post": {
        "operationId": "TestDestination",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.TestDestinationResponse"
            }
          }
        },
        "parameters": [
          {
            "name": "body",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/chef.automate.api.datafeed.URLValidationRequest"
            }
          }
        ],
        "tags": [
          "DatafeedService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.datafeed.AddDestinationRequest": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.AddDestinationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean"
        },
        "id": {
          "type": "string",
          "format": "int64"
        }
      }
    },
    "chef.automate.api.datafeed.DeleteDestinationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.datafeed.GetDestinationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean"
        },
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.ListDestinationRequest": {
      "type": "object"
    },
    "chef.automate.api.datafeed.ListDestinationResponse": {
      "type": "object",
      "properties": {
        "destinations": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.datafeed.GetDestinationResponse"
          }
        }
      }
    },
    "chef.automate.api.datafeed.SecretId": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.TestDestinationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean"
        }
      }
    },
    "chef.automate.api.datafeed.URLValidationRequest": {
      "type": "object",
      "properties": {
        "url": {
          "type": "string"
        },
        "username_password": {
          "$ref": "#/definitions/chef.automate.api.datafeed.UsernamePassword"
        },
        "secret_id": {
          "$ref": "#/definitions/chef.automate.api.datafeed.SecretId"
        }
      }
    },
    "chef.automate.api.datafeed.UpdateDestinationRequest": {
      "type": "object",
      "properties": {
        "id": {
          "type": "string",
          "format": "int64"
        },
        "name": {
          "type": "string"
        },
        "url": {
          "type": "string"
        },
        "secret": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.datafeed.UpdateDestinationResponse": {
      "type": "object",
      "properties": {
        "success": {
          "type": "boolean",
          "format": "boolean"
        },
        "id": {
          "type": "string",
          "format": "int64"
        }
      }
    },
    "chef.automate.api.datafeed.UsernamePassword": {
      "type": "object",
      "properties": {
        "username": {
          "type": "string"
        },
        "password": {
          "type": "string"
        }
      }
    }
  }
}
`)
}
