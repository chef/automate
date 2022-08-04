package mappings

// ComplianceRunInfo mapping used to create the `compliance-run-info index
var ComplianceRunInfo = Mapping{
	Index:      IndexNameComplianceRunInfo,
	Timeseries: false,
	Mapping: `
    {
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
                "platform_with_version": {
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
                "control_tag": {
                    "properties": {
                        "key": {
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
                        "values": {
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
                    },
                    "type": "nested"
                },
                "chef_server": {
                    "type": "keyword"
                },
                "organization": {
                    "type": "keyword"
                },
                "controls": {
                    "type": "nested",
                    "properties": {
                        "controls_id": {
                            "type": "keyword"
                        }
                    }
                },
                "inspec_version": {
                    "type": "keyword"
                },
                "policy_name": {
                    "type": "keyword"
                },
                "profiles": {
                    "type": "nested",
                    "properties": {
                        "profile_id": {
                            "type": "keyword"
                        }
                    }
                },
                "recipe": {
                    "type": "keyword"
                },
                "role": {
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
	`,
}
