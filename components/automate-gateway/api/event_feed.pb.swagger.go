package api

func init() {
	Swagger.Add("event_feed", `{
  "swagger": "2.0",
  "info": {
    "title": "external/event_feed/event_feed.proto",
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
        "summary": "List Counts of Individual Event Tasks",
        "description": "Returns the total counts of actions taken in an event. The counted actions are: update, create, and delete.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\nevent_task_counts?start=1592546400000\u0026end=1593151199999\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "EventFeedService_GetEventTaskCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.GetEventTaskCountsResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
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
          "EventFeedService"
        ]
      }
    },
    "/api/v0/event_type_counts": {
      "get": {
        "summary": "List Counts of Event Types",
        "description": "Returns totals for role, cookbook, and organization events.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\nevent_type_counts?start=1592546400000\u0026end=1593151199999\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "EventFeedService_GetEventTypeCounts",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.GetEventTypeCountsResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
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
          "EventFeedService"
        ]
      }
    },
    "/api/v0/eventexport": {
      "get": {
        "operationId": "EventFeedService_EventExport",
        "responses": {
          "200": {
            "description": "A successful response.(streaming responses)",
            "schema": {
              "type": "object",
              "properties": {
                "result": {
                  "$ref": "#/definitions/chef.automate.api.event_feed.response.EventExportResponse"
                },
                "error": {
                  "$ref": "#/definitions/grpc.gateway.runtime.StreamError"
                }
              },
              "title": "Stream result of chef.automate.api.event_feed.response.EventExportResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
            }
          }
        },
        "parameters": [
          {
            "name": "output_type",
            "description": "JSON or CSV.",
            "in": "query",
            "required": false,
            "type": "string"
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
            "name": "order",
            "description": "Order the results should be returned in.",
            "in": "query",
            "required": false,
            "type": "string",
            "enum": [
              "ASC",
              "DESC"
            ],
            "default": "ASC"
          }
        ],
        "tags": [
          "hidden"
        ]
      }
    },
    "/api/v0/eventfeed": {
      "get": {
        "summary": "List Events",
        "description": "Returns a list of recorded events in Chef Automate, such as Infra Server events (cookbook creation, policyfile updates, and node creation) and Chef Automate internal events (profile installs and scan job creation).\nAdding a filter makes a list of all events that meet the filter criteria.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\neventfeed?collapse=true\u0026filter=organization:4thcafe\u0026page_size=100\u0026start=1592546400000\u0026end=1593151199999\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "EventFeedService_GetEventFeed",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.GetEventFeedResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
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
          "EventFeedService"
        ]
      }
    },
    "/api/v0/eventstrings": {
      "get": {
        "summary": "List Summary Data of Events",
        "description": "Returns data that populates the guitar strings visual on the top of the event feed.\n\nExample:\n` + "`" + `` + "`" + `` + "`" + `\neventstrings?timezone=America/Denver\u0026hours_between=1\u0026start=2020-06-19\u0026end=2020-06-25\n` + "`" + `` + "`" + `` + "`" + `\n\nAuthorization Action:\n` + "`" + `` + "`" + `` + "`" + `\nevent:events:list\n` + "`" + `` + "`" + `` + "`" + `",
        "operationId": "EventFeedService_GetEventStringBuckets",
        "responses": {
          "200": {
            "description": "A successful response.",
            "schema": {
              "$ref": "#/definitions/chef.automate.api.event_feed.response.GetEventStringBucketsResponse"
            }
          },
          "default": {
            "description": "An unexpected error response",
            "schema": {
              "$ref": "#/definitions/grpc.gateway.runtime.Error"
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
          "EventFeedService"
        ]
      }
    }
  },
  "definitions": {
    "chef.automate.api.common.query.SortOrder": {
      "type": "string",
      "enum": [
        "ASC",
        "DESC"
      ],
      "default": "ASC"
    },
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
        },
        "chef_organization": {
          "type": "string",
          "title": "Event's Chef Organization"
        },
        "chef_infra_server": {
          "type": "string",
          "title": "Event's Chef Infra Server"
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
    "chef.automate.api.event_feed.response.EventExportResponse": {
      "type": "object",
      "properties": {
        "content": {
          "type": "string",
          "format": "byte",
          "description": "Exported reports in JSON or CSV."
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
    "chef.automate.api.event_feed.response.GetEventFeedResponse": {
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
    },
    "chef.automate.api.event_feed.response.GetEventStringBucketsResponse": {
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
    "chef.automate.api.event_feed.response.GetEventTaskCountsResponse": {
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
    "chef.automate.api.event_feed.response.GetEventTypeCountsResponse": {
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
    "google.protobuf.Any": {
      "type": "object",
      "properties": {
        "type_url": {
          "type": "string",
          "description": "A URL/resource name that uniquely identifies the type of the serialized\nprotocol buffer message. This string must contain at least\none \"/\" character. The last segment of the URL's path must represent\nthe fully qualified name of the type (as in\n` + "`" + `path/google.protobuf.Duration` + "`" + `). The name should be in a canonical form\n(e.g., leading \".\" is not accepted).\n\nIn practice, teams usually precompile into the binary all types that they\nexpect it to use in the context of Any. However, for URLs which use the\nscheme ` + "`" + `http` + "`" + `, ` + "`" + `https` + "`" + `, or no scheme, one can optionally set up a type\nserver that maps type URLs to message definitions as follows:\n\n* If no scheme is provided, ` + "`" + `https` + "`" + ` is assumed.\n* An HTTP GET on the URL must yield a [google.protobuf.Type][]\n  value in binary format, or produce an error.\n* Applications are allowed to cache lookup results based on the\n  URL, or have them precompiled into a binary to avoid any\n  lookup. Therefore, binary compatibility needs to be preserved\n  on changes to types. (Use versioned type names to manage\n  breaking changes.)\n\nNote: this functionality is not currently available in the official\nprotobuf release, and it is not used for type URLs beginning with\ntype.googleapis.com.\n\nSchemes other than ` + "`" + `http` + "`" + `, ` + "`" + `https` + "`" + ` (or the empty scheme) might be\nused with implementation specific semantics."
        },
        "value": {
          "type": "string",
          "format": "byte",
          "description": "Must be a valid serialized protocol buffer of the above specified type."
        }
      },
      "description": "` + "`" + `Any` + "`" + ` contains an arbitrary serialized protocol buffer message along with a\nURL that describes the type of the serialized message.\n\nProtobuf library provides support to pack/unpack Any values in the form\nof utility functions or additional generated methods of the Any type.\n\nExample 1: Pack and unpack a message in C++.\n\n    Foo foo = ...;\n    Any any;\n    any.PackFrom(foo);\n    ...\n    if (any.UnpackTo(\u0026foo)) {\n      ...\n    }\n\nExample 2: Pack and unpack a message in Java.\n\n    Foo foo = ...;\n    Any any = Any.pack(foo);\n    ...\n    if (any.is(Foo.class)) {\n      foo = any.unpack(Foo.class);\n    }\n\n Example 3: Pack and unpack a message in Python.\n\n    foo = Foo(...)\n    any = Any()\n    any.Pack(foo)\n    ...\n    if any.Is(Foo.DESCRIPTOR):\n      any.Unpack(foo)\n      ...\n\n Example 4: Pack and unpack a message in Go\n\n     foo := \u0026pb.Foo{...}\n     any, err := anypb.New(foo)\n     if err != nil {\n       ...\n     }\n     ...\n     foo := \u0026pb.Foo{}\n     if err := any.UnmarshalTo(foo); err != nil {\n       ...\n     }\n\nThe pack methods provided by protobuf library will by default use\n'type.googleapis.com/full.type.name' as the type URL and the unpack\nmethods only use the fully qualified type name after the last '/'\nin the type URL, for example \"foo.bar.com/x/y.z\" will yield type\nname \"y.z\".\n\n\nJSON\n====\nThe JSON representation of an ` + "`" + `Any` + "`" + ` value uses the regular\nrepresentation of the deserialized, embedded message, with an\nadditional field ` + "`" + `@type` + "`" + ` which contains the type URL. Example:\n\n    package google.profile;\n    message Person {\n      string first_name = 1;\n      string last_name = 2;\n    }\n\n    {\n      \"@type\": \"type.googleapis.com/google.profile.Person\",\n      \"firstName\": \u003cstring\u003e,\n      \"lastName\": \u003cstring\u003e\n    }\n\nIf the embedded message type is well-known and has a custom JSON\nrepresentation, that representation will be embedded adding a field\n` + "`" + `value` + "`" + ` which holds the custom JSON in addition to the ` + "`" + `@type` + "`" + `\nfield. Example (for message [google.protobuf.Duration][]):\n\n    {\n      \"@type\": \"type.googleapis.com/google.protobuf.Duration\",\n      \"value\": \"1.212s\"\n    }"
    },
    "grpc.gateway.runtime.Error": {
      "type": "object",
      "properties": {
        "error": {
          "type": "string"
        },
        "code": {
          "type": "integer",
          "format": "int32"
        },
        "message": {
          "type": "string"
        },
        "details": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/google.protobuf.Any"
          }
        }
      }
    },
    "grpc.gateway.runtime.StreamError": {
      "type": "object",
      "properties": {
        "grpc_code": {
          "type": "integer",
          "format": "int32"
        },
        "http_code": {
          "type": "integer",
          "format": "int32"
        },
        "message": {
          "type": "string"
        },
        "http_status": {
          "type": "string"
        },
        "details": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/google.protobuf.Any"
          }
        }
      }
    }
  }
}
`)
}
