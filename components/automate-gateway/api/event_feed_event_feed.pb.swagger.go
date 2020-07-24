package api

func init() {
	Swagger.Add("event_feed_event_feed", `{
  "swagger": "2.0",
  "info": {
    "title": "automate-gateway/api/event_feed/event_feed.proto",
    "version": "version not set"
  },
  "consumes": [
    "application/json"
  ],
  "produces": [
    "application/json"
  ],
  "paths": {
    "/api/v0/event_task_counts": {
      "get": {
        "summary": "List Counts Per Event Task Occurrence",
        "description": "Returns totals for update, create, and delete event tasks, which are the actions taken on the event.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\nevent_task_counts?start=1592546400000\u0026end=1593151199999\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetEventTaskCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.EventCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "description": "Filters to be applied to the request.",
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
            "description": "Earliest events to return.",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "end",
            "description": "Latest events to return.",
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
    "/api/v0/event_type_counts": {
      "get": {
        "summary": "List Count of Event Type Occurrence",
        "description": "Returns totals for role, cookbook, and organization event types.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\nevent_type_counts?start=1592546400000\u0026end=1593151199999\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetEventTypeCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.EventCounts"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "description": "Filters to be applied to the request.",
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
            "description": "Earliest events to return.",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "end",
            "description": "Latest events to return.",
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
    "/api/v0/eventfeed": {
      "get": {
        "summary": "List Events",
        "description": "Returns a list of recorded events in Chef Automate, such as Infra Server events (cookbook creation, policyfile updates, and node creation) and Chef Automate internal events (profile installs and scan job creation).\nAdding a filter makes a list of all events that meet the filter criteria.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\neventfeed?collapse=true\u0026filter=organization:The%2520Watchmen\u0026page_size=100\u0026start=1592546400000\u0026end=1593151199999\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetEventFeed",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.Events"
            }
          }
        },
        "parameters": [
          {
            "name": "filter",
            "description": "Filters to be applied to the request.",
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
            "description": "Earliest events to return.",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "end",
            "description": "Latest events to return.",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "page_size",
            "description": "Count of events to return per page.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "after",
            "description": "Used for pagination, to request results in ascending order.",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "before",
            "description": "Used for pagination, to request results in descending order.",
            "in": "query",
            "required": false,
            "type": "string",
            "format": "int64"
          },
          {
            "name": "cursor",
            "description": "Used for pagination, to bookmark next set of results.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "collapse",
            "description": "Used to group similar events together.",
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
    "/api/v0/eventstrings": {
      "get": {
        "summary": "List Summary Stats About Events",
        "description": "Returns data that populates the guitar strings visual on the top of the event feed.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\neventstrings?timezone=America/Denver\u0026hours_between=1\u0026start=2020-06-19\u0026end=2020-06-25\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "GetEventStringBuckets",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.EventStrings"
            }
          }
        },
        "parameters": [
          {
            "name": "start",
            "description": "Earliest events to return.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "end",
            "description": "Latest events to return.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "timezone",
            "description": "User timezone to apply to request.",
            "in": "query",
            "required": false,
            "type": "string"
          },
          {
            "name": "hours_between",
            "description": "Interval for returned results, for example: 1 hour buckets.",
            "in": "query",
            "required": false,
            "type": "integer",
            "format": "int32"
          },
          {
            "name": "filter",
            "description": "Filters to be applied to the request.",
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
    "chef.automate.api.event_feed.response.Event": {
      "type": "object",
      "properties": {
        "event_type": {
          "type": "string",
          "description": "Type of event (cookbook, role, etc)."
        },
        "task": {
          "type": "string",
          "description": "Type of event task (create, update, delete)."
        },
        "start_time": {
          "type": "string",
          "format": "date-time",
          "description": "Event start time."
        },
        "entity_name": {
          "type": "string"
        },
        "requestor_type": {
          "type": "string",
          "description": "Event record requestor type."
        },
        "requestor_name": {
          "type": "string",
          "description": "Event record requestor name."
        },
        "service_hostname": {
          "type": "string",
          "description": "Hostname from which the record was gathered."
        },
        "start_id": {
          "type": "string",
          "description": "Used for grouping events together."
        },
        "event_count": {
          "type": "integer",
          "format": "int32",
          "description": "Used for grouping events together."
        },
        "parent_name": {
          "type": "string",
          "description": "Used for grouping events together."
        },
        "parent_type": {
          "type": "string",
          "description": "Used for grouping events together."
        },
        "end_time": {
          "type": "string",
          "format": "date-time",
          "title": "Used for grouping events together; equal to start_time if not grouped"
        },
        "end_id": {
          "type": "string",
          "title": "Used for grouping events together; equal to start_id if not grouped"
        }
      }
    },
    "chef.automate.api.event_feed.response.EventCollection": {
      "type": "object",
      "properties": {
        "events_count": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.event_feed.response.EventCount"
          }
        }
      }
    },
    "chef.automate.api.event_feed.response.EventCount": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "Event name."
        },
        "count": {
          "type": "string",
          "format": "int64",
          "description": "Count of events."
        }
      }
    },
    "chef.automate.api.event_feed.response.EventCounts": {
      "type": "object",
      "properties": {
        "total": {
          "type": "string",
          "format": "int64",
          "description": "Total count of events."
        },
        "counts": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.event_feed.response.EventCount"
          },
          "description": "Total count of events per type."
        }
      }
    },
    "chef.automate.api.event_feed.response.EventString": {
      "type": "object",
      "properties": {
        "collection": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.event_feed.response.EventCollection"
          }
        },
        "event_action": {
          "type": "string"
        }
      }
    },
    "chef.automate.api.event_feed.response.EventStrings": {
      "type": "object",
      "properties": {
        "strings": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.event_feed.response.EventString"
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
    "chef.automate.api.event_feed.response.Events": {
      "type": "object",
      "properties": {
        "events": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/chef.automate.api.event_feed.response.Event"
          },
          "description": "List of events."
        },
        "total_events": {
          "type": "string",
          "format": "int64",
          "description": "Total count of events."
        }
      }
    }
  }
}
`)
}
