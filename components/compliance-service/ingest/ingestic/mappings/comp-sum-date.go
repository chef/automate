package mappings

// Compliance mapping used to create the `comp-<version>-s-<date>` index
var ComplianceSumDate = Mapping{
	Index:      IndexNameSum,
	Type:       DocType,
	Timeseries: true,
	Mapping: `
{
  "index_patterns": ["` + IndexNameSum + `-20*"],
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
          "filter": ["lowercase", "asciifolding"]
        }
      }
    },
    "index": {
      "refresh_interval": "1s"
    }
  },
  "mappings": {
    "` + DocType + `": {
      "properties": {
        "controls_sums": {
          "properties": {
            "failed": {
              "properties": {
                "critical": {
                  "type": "integer"
                },
                "major": {
                  "type": "integer"
                },
                "minor": {
                  "type": "integer"
                },
                "total": {
                  "type": "integer"
                }
              },
              "type": "object"
            },
            "passed": {
              "properties": {
                "total": {
                  "type": "integer"
                }
              },
              "type": "object"
            },
            "skipped": {
              "properties": {
                "total": {
                  "type": "integer"
                }
              },
              "type": "object"
            },
            "total": {
              "type": "integer"
            }
          },
          "type": "object"
        },
        "daily_latest": {
          "type": "boolean"
        },
        "doc_version": {
          "type": "keyword"
        },
        "end_time": {
          "type": "date"
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
        "job_uuid": {
          "type": "keyword"
        },
        "node_name": {
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
        "node_uuid": {
          "type": "keyword"
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
              "type": "keyword"
            }
          }
        },
        "profiles": {
          "properties": {
            "controls_sums": {
              "properties": {
                "failed": {
                  "properties": {
                    "critical": {
                      "type": "integer"
                    },
                    "major": {
                      "type": "integer"
                    },
                    "minor": {
                      "type": "integer"
                    },
                    "total": {
                      "type": "integer"
                    }
                  },
                  "type": "object"
                },
                "passed": {
                  "properties": {
                    "total": {
                      "type": "integer"
                    }
                  },
                  "type": "object"
                },
                "skipped": {
                  "properties": {
                    "total": {
                      "type": "integer"
                    }
                  },
                  "type": "object"
                },
                "total": {
                  "type": "integer"
                }
              },
              "type": "object"
            },
            "profile": {
              "type": "keyword"
            },
            "name": {
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
                  "normalizer": "case_insensitive",
                  "type": "keyword"
                }
              }
            },
            "sha256": {
              "type": "keyword"
            },
            "version": {
              "type": "keyword"
            },
            "status": {
              "type": "keyword"
            }
          },
          "type": "nested"
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
        "report_uuid": {
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
        "status": {
          "type": "keyword"
        },
        "projects": {
          "type": "keyword"
        },
        "statistics": {
          "properties": {
            "duration": {
              "type": "double"
            }
          }
        },
        "version": {
          "type": "keyword",
          "fields": {
            "engram": {
              "type": "text",
              "analyzer": "autocomplete_version_numbers"
            },
            "lower": {
              "normalizer": "case_insensitive",
              "type": "keyword"
            }
          }
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
				}
      }
    }
  }
}`,
}
