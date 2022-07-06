package mappings

// ComplianceControlRepData Compliance mapping used to create the `comp-<version>-control-<date>` index
var ComplianceControlRepData = Mapping{
	Index:      IndexNameControl,
	Timeseries: true,
	Mapping: `
{
  "index_patterns": [
    "` + IndexNameControl + `-20*"
  ],
  "settings": {
    "analysis": {
      "analyzer": {
        "autocomplete": {
          "filter": [
            "lowercase"
          ],
          "tokenizer": "autocomplete_tokenizer"
        },
        "autocomplete_version_numbers": {
          "filter": [
            "lowercase"
          ],
          "tokenizer": "autocomplete_version_number_tokenizer"
        }
      },
      "tokenizer": {
        "autocomplete_tokenizer": {
          "max_gram": 20,
          "min_gram": 2,
          "token_chars": [
            "letter",
            "digit"
          ],
          "type": "edge_ngram"
        },
        "autocomplete_version_number_tokenizer": {
          "max_gram": 20,
          "min_gram": 2,
          "token_chars": [
            "letter",
            "digit",
            "punctuation"
          ],
          "type": "edge_ngram"
        }
      },
      "normalizer": {
        "case_insensitive": {
          "type": "custom",
          "char_filter": [],
          "filter": [
            "lowercase",
            "asciifolding"
          ]
        }
      }
    },
    "index": {
      "refresh_interval": "1s"
    }
  },
  "mappings": {
    "properties": {
      "control_id": {
        "type": "keyword"
      },
      "title": {
        "type": "keyword",
        "fields": {
          "engram": {
            "type": "text",
            "analyzer": "autocomplete"
          },
          "lower": {
            "type": "keyword",
            "normalizer": "case_insensitive"
          }
        }
      },
      "waived_str": {
        "type": "keyword"
      },
      "waiver_data": {
        "properties": {
          "expiration_date": {
            "type": "keyword"
          },
          "justification": {
            "type": "keyword"
          },
          "message": {
            "type": "keyword"
          },
          "run": {
            "type": "boolean"
          },
          "skipped_due_to_waiver": {
            "type": "boolean"
          }
        }
      },
      "impact": {
        "type": "double"
      },
      "string_tags": {
        "type": "nested",
        "properties": {
          "key": {
            "type": "keyword",
            "fields": {
              "engram": {
                "type": "text",
                "analyzer": "autocomplete"
              },
              "lower": {
                "type": "keyword",
                "normalizer": "case_insensitive"
              }
            }
          },
          "values": {
            "type": "keyword",
            "fields": {
              "engram": {
                "type": "text",
                "analyzer": "autocomplete"
              },
              "lower": {
                "type": "keyword",
                "normalizer": "case_insensitive"
              }
            }
          }
        }
      },
      "status": {
        "type": "keyword"
      },
      "daily_latest": {
        "type": "boolean"
      },
      "day_latest": {
        "type": "boolean"
      },
      "end_time": {
        "type": "date"
      },
      "nodes": {
        "properties": {
          "node_uuid": {
            "type": "keyword"
          },
          "environment": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "node_name": {
            "type": "keyword",
            "fields": {
              "engram": {
                "type": "text",
                "analyzer": "autocomplete"
              },
              "lower": {
                "type": "keyword",
                "normalizer": "case_insensitive"
              }
            }
          },
          "platform": {
            "properties": {
              "name": {
                "fields": {
                  "engram": {
                    "analyzer": "autocomplete",
                    "type": "text"
                  },
                  "lower": {
                    "normalizer": "case_insensitive",
                    "type": "keyword"
                  }
                },
                "type": "keyword"
              },
              "release": {
                "fields": {
                  "engram": {
                    "analyzer": "autocomplete",
                    "type": "text"
                  },
                  "lower": {
                    "normalizer": "case_insensitive",
                    "type": "keyword"
                  }
                },
                "type": "keyword"
              },
              "full": {
                "fields": {
                  "engram": {
                    "analyzer": "autocomplete_version_numbers",
                    "type": "text"
                  },
                  "lower": {
                    "normalizer": "case_insensitive",
                    "type": "keyword"
                  }
                },
                "type": "keyword"
              }
            }
          },
          "recipes": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "roles": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "policy_name": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "policy_group": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "organization_name": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "source_fqdn": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "chef_tags": {
            "fields": {
              "engram": {
                "analyzer": "autocomplete",
                "type": "text"
              },
              "lower": {
                "normalizer": "case_insensitive",
                "type": "keyword"
              }
            },
            "type": "keyword"
          },
          "end_time": {
            "type": "date"
          },
          "status": {
            "type": "keyword"
          },
          "daily_latest": {
            "type": "boolean"
          },
          "day_latest": {
            "type": "boolean"
          },
          "report_uuid": {
            "type": "keyword"
          }
        },
        "type": "nested"
      },
      "profile": {
        "properties": {
          "profile": {
            "type": "keyword"
          },
          "sha256": {
            "type": "keyword"
          }
        }
      },
      "report_uuid": {
        "type": "keyword"
      }
    }
  }
}`,
}
