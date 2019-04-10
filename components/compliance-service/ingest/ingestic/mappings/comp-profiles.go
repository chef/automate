package mappings

// ComplianceProfiles mapping used to create the `compliance-profiles` index
var ComplianceProfiles = Mapping{
	Index:      IndexNameProf,
	Type:       DocType,
	Timeseries: false,
	Mapping: `
{
  "template": "` + IndexNameProf + `",
  "settings": {
    "index": {
      "refresh_interval": "1s"
    },
    "analysis": {
      "analyzer": {
        "autocomplete": {
          "tokenizer": "autocomplete_tokenizer",
          "filter": [
            "lowercase"
          ]
        }
      },
      "tokenizer": {
        "autocomplete_tokenizer": {
          "type": "edge_ngram",
          "min_gram": 2,
          "max_gram": 20,
          "token_chars": [
            "letter",
            "digit"
          ]
        }
      },
      "normalizer": {
        "case_insensitive": {
          "type": "custom",
          "char_filter": [],
          "filter": ["lowercase", "asciifolding"]
        }
      }
    }
  },
  "mappings": {
    "` + DocType + `": {
      "properties": {
        "name": {
          "type": "keyword",
          "fields": {
			"lower": {
			  "normalizer": "case_insensitive",
			  "type": "keyword"
			}
		  }
        },
        "title": {
          "type": "keyword",
          "fields": {
            "engram": {
              "type": "text",
              "analyzer": "autocomplete"
            },
            "lower": {
              "normalizer": "case_insensitive",
              "type": "keyword"
            }
          }
        },
        "maintainer": {
          "type": "keyword"
        },
        "copyright": {
          "type": "keyword"
        },
        "copyright_email": {
          "type": "keyword"
        },
        "license": {
          "type": "keyword"
        },
        "summary": {
          "type": "keyword"
        },
        "version": {
          "type": "keyword"
        },
        "supports": {
          "type": "object",
          "properties": {
            "os-family": {
              "type": "keyword"
            },
            "os-name": {
              "type": "keyword"
            },
            "platform": {
              "type": "keyword"
            },
            "platform-name": {
              "type": "keyword"
            },
            "platform-family": {
              "type": "keyword"
            },
            "release": {
              "type": "keyword"
            },
            "inspec": {
              "type": "keyword"
            }
          }
        },
        "controls": {
          "type": "nested",
          "properties": {
            "title": {
              "type": "keyword"
            },
            "desc": {
              "type": "keyword"
            },
            "description": {
              "properties": {
                "label": {
                  "type": "keyword"
                },
                "data": {
                  "type": "keyword"
                }
              }
            },
            "impact": {
              "type": "double"
            },
            "refs": {
              "type": "keyword"
            },
            "tags": {
              "type": "keyword"
            },
            "code": {
              "type": "keyword"
            },
            "source_location": {
              "type": "object",
              "properties": {
                "ref": {
                  "type": "keyword"
                },
                "line": {
                  "type": "integer"
                }
              }
            },
            "id": {
              "type": "keyword",
              "fields": {
                "engram": {
                  "type": "text",
                  "analyzer": "autocomplete"
                },
                "lower": {
                  "normalizer": "case_insensitive",
                  "type": "keyword"
                }
              }
            }
          }
        },
        "groups": {
          "type": "object",
          "properties": {
            "title": {
              "type": "keyword"
            },
            "controls": {
              "type": "keyword"
            },
            "id": {
              "type": "keyword"
            },
            "attributes": {
              "type": "object",
              "properties": {
                "name": {
                  "type": "keyword"
                },
                "options": {
                  "type": "object",
                  "properties": {
                    "default": {
                      "type": "keyword"
                    },
                    "description": {
                      "type": "keyword"
                    }
                  }
                }
              }
            },
            "sha256": {
              "type": "keyword"
            }
          }
        }
      }
    }
  }
}
	`,
}
