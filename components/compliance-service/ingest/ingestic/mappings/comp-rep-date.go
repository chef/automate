package mappings

// Compliance mapping used to create the `comp-<version>-r-<date>` index
var ComplianceRepDate = Mapping{
	Index:      IndexNameRep,
	Type:       DocType,
	Timeseries: true,
	Mapping: `
{
  "index_patterns": ["` + IndexNameRep + `-20*"],
  "settings": {
    "analysis": {
      "analyzer": {
        "autocomplete": {
          "filter": [
            "lowercase"
          ],
          "tokenizer": "autocomplete_tokenizer"
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
        "depends": {
          "type": "object",
          "properties": {
	        "name": {
              "type": "keyword"
            },
            "path": {
              "type": "keyword"
		    },
		    "status": {
		      "type": "keyword"
		    },
		    "skip_message": {
		      "type": "keyword"
		    }
		  }
		},
        "doc_version": {
          "type": "keyword"
        },
        "end_time": {
          "type": "date"
        },
        "environment": {
          "type": "keyword",
          "normalizer": "case_insensitive",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          }
        },
        "job_uuid": {
          "type": "keyword"
        },
        "node_name": {
          "type": "keyword",
          "normalizer": "case_insensitive",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          }
        },
        "node_uuid": {
          "type": "keyword"
        },
        "other_checks": {
          "type": "keyword"
        },
        "platform": {
          "properties": {
            "name": {
              "type": "keyword",
              "fields": {
                "engram": {
                  "analyzer": "autocomplete",
                  "type": "text"
                }
              },
              "normalizer": "case_insensitive"
            },
            "release": {
              "type": "keyword",
              "fields": {
                "engram": {
                  "analyzer": "autocomplete",
                  "type": "text"
                }
              },
              "normalizer": "case_insensitive"
            },
            "full": {
              "type": "keyword"
            }
          }
        },
        "profiles": {
          "properties": {
            "controls": {
              "properties": {
                "id": {
                  "type": "keyword"
                },
                "impact": {
                  "type": "double"
                },
                "title": {
                  "type": "keyword",
                  "fields": {
                    "engram": {
                      "analyzer": "autocomplete",
                      "type": "text"
                    }
                  },
                  "normalizer": "case_insensitive"
                },
                "results": {
                  "properties": {
                    "code_desc": {
                      "type": "keyword"
                    },
                    "run_time": {
                      "type": "double"
                    },
                    "status": {
                      "type": "keyword"
                    }
                  }
                },
                "status": {
                  "type": "keyword"
                }
              },
              "type": "nested"
            },
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
            "name": {
              "type": "keyword"
            },
            "title": {
              "type": "keyword",
              "fields": {
                "engram": {
                  "analyzer": "autocomplete",
                  "type": "text"
                }
              },
              "normalizer": "case_insensitive"
            },
            "profile": {
              "type": "keyword"
            },
            "sha256": {
              "type": "keyword"
            },
            "skip_message": {
              "type": "keyword"
            },
            "status": {
              "type": "keyword"
            },
            "version": {
              "type": "keyword"
            }
          },
          "type": "nested"
        },
        "recipes": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
        },
        "report_uuid": {
          "type": "keyword"
        },
        "roles": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
        },
        "statistics": {
          "properties": {
            "duration": {
              "type": "double"
            }
          }
        },
        "status": {
          "type": "keyword"
        },
        "projects": {
          "type": "keyword"
        },
        "version": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
        },
				"policy_name": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
				},
				"policy_group": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
				},
				"organization_name": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
				},
				"source_fqdn": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
				},
				"chef_tags": {
          "type": "keyword",
          "fields": {
            "engram": {
              "analyzer": "autocomplete",
              "type": "text"
            }
          },
          "normalizer": "case_insensitive"
				},
				"ipaddress": {
					"type": "ip"
				},
				"fqdn": {
					"type": "keyword",
					"doc_values": false,
					"ignore_above": 256
				}
      }
    }
  }
}`,
}
