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
        "other_checks": {
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
                      "type": "text",
                      "analyzer": "autocomplete"
                    },
                    "lower": {
                      "normalizer": "case_insensitive",
                      "type": "keyword"
                    }
                  }
                },
                "refs": {
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
                  "type": "text",
                  "analyzer": "autocomplete"
                },
                "lower": {
                  "normalizer": "case_insensitive",
                  "type": "keyword"
                }
              }
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
