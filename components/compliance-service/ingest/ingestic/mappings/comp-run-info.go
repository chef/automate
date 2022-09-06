package mappings

// ComplianceRunInfo mapping used to create the `compliance-run-info index
var ComplianceRunInfo = Mapping{
	Index:      IndexNameComplianceRunInfo,
	Timeseries: false,
	Mapping: `
    {
    "index_patterns": ["` + IndexNameComplianceRunInfo + `"],
    "settings": {
        "index": {
            "refresh_interval": "1s",
            "number_of_shards": "5"
        },
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
        }
    },
    "mappings": {
        "properties": {
            "node_uuid": {
                "type": "keyword"
            },
            "resource_uuid": {
                "type": "keyword"
            },
            "resource_type": {
                "type": "keyword"
            },
            "status": {
                "type": "keyword"
            },
            "first_run": {
                "type": "date"
            },
            "last_run": {
                "type": "date"
            },
            "chef_server": {
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
            "organization": {
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
            "version": {
                "type": "keyword",
                "fields": {
                    "engram": {
                        "type": "text",
                        "analyzer": "autocomplete_version_numbers"
                    },
                    "lower": {
                        "type": "keyword",
                        "normalizer": "case_insensitive"
                    }
                }
            },
            "policy_name": {
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
            "policy_group": {
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
            "profiles": {
                "type": "nested",
                "properties": {
                    "controls": {
                        "type": "nested",
                        "properties": {
                            "id": {
                                "type": "keyword"
                            },
                            "control_tags": {
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
                                "type": "keyword",
                                "normalizer": "case_insensitive"
                            }
                        }
                    },
                    "sha256": {
                        "type": "keyword"
                    },
                    "name": {
                        "type": "keyword"
                    },
                    "full": {
                        "type": "keyword",
                        "fields": {
                            "engram": {
                                "type": "text",
                                "analyzer": "autocomplete_version_numbers"
                            },
                            "lower": {
                                "type": "keyword",
                                "normalizer": "case_insensitive"
                            }
                        }
                    }
                }
            },
            "recipes": {
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
            "roles": {
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
            "platform_version": {
                "properties": {
                    "full": {
                        "type": "keyword",
                        "fields": {
                            "engram": {
                                "type": "text",
                                "analyzer": "autocomplete_version_numbers"
                            },
                            "lower": {
                                "type": "keyword",
                                "normalizer": "case_insensitive"
                            }
                        }
                    },
                    "name": {
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
                    "release": {
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
	`,
}
