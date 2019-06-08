##### GRPC SETUP #####
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  it "works" do
    reporting = Reporting::ReportingService

    # Test filter by job_id
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['74a54a28-c628-4f82-86df-333333333333']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                    "endTime" => "2018-03-04T09:18:41Z",
                    "status" => "passed",
                    "controls" => {
                        "total" => 18,
                        "passed" => {
                            "total" => 3
                        },
                        "skipped" => {
                            "total" => 15
                        },
                        "failed" => {}
                    }
                }
            }
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Test filter by profile_id, one node back  CONTROL!
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "control", values: ["nginx-04"]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes =
        {
            "nodes" => [
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "name" => "centos-beta",
                    "platform" => {
                        "name" => "centos",
                        "release" => "5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                        "endTime" => "2018-03-04T09:18:41Z",
                        "status" => "passed",
                        "controls" => {
                            "total"=> 1,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {},
                            "failed" => {}
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total"=> 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total"=> 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                        "endTime" => "2018-03-04T09:18:43Z",
                        "status" => "failed",
                        "controls" => {
                            "total"=> 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            }
                        }
                    }
                }
            ],
            "total" => 4
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    actual = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['non-existent'])
    ])
    assert_equal(Reporting::Nodes.new(), actual)

    # Test filter by profile_id, one node back
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']
        )
    ])
    expected_nodes =
        {
            "nodes" => [
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "name" => "centos-beta",
                    "platform" => {
                        "name" => "centos",
                        "release" => "5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                        "endTime" => "2018-03-04T09:18:41Z",
                        "status" => "passed",
                        "controls" => {
                            "total" => 4,
                            "passed" => {
                                "total" => 3
                            },
                            "skipped" => {
                                "total" => 1
                            },
                            "failed" => {}
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 4,
                            "passed" => {
                                "total" => 2
                            },
                            "skipped" => {
                                "total" => 1
                            },
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 4,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {
                                "total" => 1
                            },
                            "failed" => {
                                "total" => 2,
                                "major" => 1,
                                "critical" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                        "endTime" => "2018-03-04T09:18:43Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 4,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {
                                "total" => 1
                            },
                            "failed" => {
                                "total" => 2,
                                "major" => 1,
                                "critical" => 1
                            }
                        }
                    }
                }
            ],
            "total" => 4
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Test filter by profile_id, multiple nodes
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes =
        {
            "nodes" => [
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "name" => "centos-beta",
                    "platform" => {
                        "name" => "centos",
                        "release" => "5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                        "endTime" => "2018-03-04T09:18:41Z",
                        "status" => "skipped",
                        "controls" => {
                            "total" => 14,
                            "passed" => {},
                            "skipped" => {
                                "total" => 14
                            },
                            "failed" => {}
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 14,
                            "passed" => {},
                            "skipped" => {
                                "total" => 13
                            },
                            "failed" => {
                                "total" => 1,
                                "critical" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "skipped",
                        "controls" => {
                            "total" => 14,
                            "passed" => {},
                            "skipped" => {
                                "total" => 14
                            },
                            "failed" => {}
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                        "endTime" => "2018-03-04T09:18:43Z",
                        "status" => "skipped",
                        "controls" => {
                            "total" => 14,
                            "passed" => {},
                            "skipped" => {
                                "total" => 14
                            },
                            "failed" => {}
                        }
                    }
                },
                {
                    "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "name" => "windows(1)-zeta-apache(s)-skipped",
                    "platform" => {
                        "name" => "windows",
                        "release" => "7"
                    },
                    "environment" => "DevSec Prod Zeta",
                    "latestReport" => {
                        "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                        "endTime" => "2018-03-04T10:18:41Z",
                        "status" => "skipped",
                        "controls" => {
                            "total" => 14,
                            "passed" => {},
                            "skipped" => {
                                "total" => 14
                            },
                            "failed" => {}
                        }
                    }
                }
            ],
            "total" => 5
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Get nodes that are using at least one of the given profiles
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988', 'xxxdcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143yyy']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes =
        {
            "nodes" => [
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "name" => "centos-beta",
                    "platform" => {
                        "name" => "centos",
                        "release" => "5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                        "endTime" => "2018-03-04T09:18:41Z",
                        "status" => "passed",
                        "controls" => {
                            "total" => 18,
                            "passed" => {
                                "total" => 3
                            },
                            "skipped" => {
                                "total" => 15
                            },
                            "failed" => {}
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 18,
                            "passed" => {
                                "total" => 2
                            },
                            "skipped" => {
                                "total" => 14
                            },
                            "failed" => {
                                "total" => 2,
                                "major" => 1,
                                "critical" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                        "endTime" => "2018-03-04T09:18:42Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 18,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {
                                "total" => 15
                            },
                            "failed" => {
                                "total" => 2,
                                "major" => 1,
                                "critical" => 1
                            }
                        }
                    }
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                        "endTime" => "2018-03-04T09:18:43Z",
                        "status" => "failed",
                        "controls" => {
                            "total" => 18,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {
                                "total" => 15
                            },
                            "failed" => {
                                "total" => 2,
                                "major" => 1,
                                "critical" => 1
                            }
                        }
                    }
                }
            ],
            "total" => 4
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end
end
