package api

func init() {
	Swagger.Add("event_feed_event_feed", `{
  "swagger": "2.0",
  "info": {
    "title": "components/automate-gateway/api/event_feed/event_feed.proto",
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
    "/event_task_counts": {
      "get": {
        "operationId": "GetEventTaskCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseEventCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "start",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "end",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          }
        ],
        "tags": [
          "EventFeed"
        ]
      }
    },
    "/event_type_counts": {
      "get": {
        "operationId": "GetEventTypeCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseEventCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "start",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "end",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          }
        ],
        "tags": [
          "EventFeed"
        ]
      }
    },
    "/eventfeed": {
      "get": {
        "operationId": "GetEventFeed",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/responseEvents"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          },
          {
            "name": "start",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "end",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "page_size",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "after",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "before",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "cursor",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "collapse",
            "in": "query",
            "required": false,
            "type": "boolean",
            "format": "boolean"
          }
        ],
        "tags": [
          "EventFeed"
        ]
      }
    },
    "/eventstrings": {
      "get": {
        "operationId": "GetEventStringBuckets",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/event_feedresponseEventStrings"
            }
          }
        },
        "parameters": [
          {
            "name": "start",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "end",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "timezone",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "hours_between",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "filter",
            "in": "query",
            "required": false,
            "type": "array",
            "items": {
              "type": "string"
            },
            "collectionFormat": "multi"
          }
        ],
        "tags": [
          "EventFeed"
        ]
      }
    }
  },
  "definitions": {
    "event_feedresponseEventStrings": {
      "type": "object",
      "properties": {
        "strings": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseEventString"
          }
        },
        "start": {
          "type": "string"
        },
        "end": {
          "type": "string"
        },
        "hours_between": {
          "type": "integer",
          "format": "int32"
        }
      }
    },
    "responseEvent": {
      "type": "object",
      "properties": {
        "event_type": {
          "type": "string"
        },
        "task": {
          "type": "string"
        },
        "start_time": {
          "type": "string",
          "format": "date-time"
        },
        "entity_name": {
          "type": "string"
        },
        "requestor_type": {
          "type": "string"
        },
        "requestor_name": {
          "type": "string"
        },
        "service_hostname": {
          "type": "string"
        },
        "start_id": {
          "type": "string"
        },
        "event_count": {
          "type": "integer",
          "format": "int32"
        },
        "parent_name": {
          "type": "string"
        },
        "parent_type": {
          "type": "string"
        },
        "end_time": {
          "type": "string",
          "format": "date-time"
        },
        "end_id": {
          "type": "string"
        }
      }
    },
    "responseEventCollection": {
      "type": "object",
      "properties": {
        "events_count": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseEventCount"
          }
        }
      }
    },
    "responseEventCount": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "count": {
          "type": "string",
          "format": "int64"
        }
      }
    },
    "responseEventCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "string",
          "format": "int64"
        },
        "counts": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseEventCount"
          }
        }
      }
    },
    "responseEventString": {
      "type": "object",
      "properties": {
        "collection": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseEventCollection"
          }
        },
        "event_action": {
          "type": "string"
        }
      }
    },
    "responseEvents": {
      "type": "object",
      "properties": {
        "events": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/responseEvent"
          }
        },
        "total_events": {
          "type": "string",
          "format": "int64"
        }
      }
    }
  }
}
`)
}
