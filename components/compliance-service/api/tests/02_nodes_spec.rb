##### GRPC SETUP #####
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  it "works" do
    reporting = Reporting::ReportingService
    ##### Failure tests #####
    message = "Parameter 'sort' only supports one of the following fields: [environment latest_report.controls.failed.critical latest_report.controls.failed.total latest_report.end_time latest_report.status name platform status]"
    assert_grpc_error(message, 3) do
      GRPC reporting, :list_nodes, Reporting::Query.new(sort: 'something')
    end

    assert_grpc_error("Not found for id: 123", 5) do
      GRPC reporting, :read_node, Reporting::Id.new(id: '123')
    end

    ##### Success tests #####

    # List all nodes sorted by node name
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']), # start_time is ignored for this call. Only end_time is used
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'name')
    assert_equal([
                     "centos-beta",
                     "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                     "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                     "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                     "windows(1)-zeta-apache(s)-skipped"
                 ],
                 resp['nodes'].map {|x| x['name']},
    )

    # Get all nodes, sorted by latest_report(default), asc(default) order
    # Hits only daily index 2018-03-05
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
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
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                    "endTime" => "2018-03-05T02:02:02Z",
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
              }
        ],
        "total" => 1,
        "totalPassed" => 1
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)

    # Filtering for end_date 2018-03-04 as it has two nodes scanned that day
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "failed",
                    "version" => "2.0.1"
                  }
                ]
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
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
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 5,
        "totalFailed"=>3,
        "totalPassed"=>1,
        "totalSkipped"=>1
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)


    # Filtering for end_date 2018-03-04 as it has two nodes scanned that day
    # Filter for the node named 'windows(1)-zeta-apache(s)-skipped'
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_name', values: ['windows(1)-zeta-apache(s)-skipped']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes = {
        "nodes" => [
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
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalSkipped" => 1
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)


    # Test page size
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(per_page: 1, order: 1, filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes = {
        "nodes" => [
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
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 5,
        "totalFailed"=>3,
        "totalPassed"=>1,
        "totalSkipped"=>1
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)

    # Test pagination(per_page+page+order)
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(per_page: 1, page: 2, order: 1, filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes = {
        "nodes" => [
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 5,
        "totalFailed"=>3,
        "totalPassed"=>1,
        "totalSkipped"=>1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes = {
        "nodes" => [
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
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalSkipped" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Test filters by inspec version
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'inspec_version', values: ['3.1.0']),
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Test filters by inspec version
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'inspec_version', values: ['3.1.3']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    assert_equal(actual_nodes.total, 4)


    # Test filters by env and status
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod beta']),
        Reporting::ListFilter.new(type: 'status', values: ['passed']),
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Test filters by env and status
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'recipe', values: ['apache_extras']),
        Reporting::ListFilter.new(type: 'status', values: ['passed']),
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Testing non matching filters
    actual = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'status', values: ['skipped', 'failed']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
    ])
    assert_equal(Reporting::Nodes.new(), actual)

    # Get nodes with platform 'centos', 'windows' or 'aix'(we don't have a matching aix node, but it doesn't hurt to test this)
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos', 'windows', 'aix']),
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
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
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
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1"
                  }
                ]
            }
        ],
        "total" => 2,
        "totalPassed" => 1,
        "totalSkipped" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Target missing environment that contains special characters
    actual = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['Dev&Test Trouble+Maker:Env'])
    ])
    assert_equal(Reporting::Nodes.new(), actual)

    actual = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['non-existent'])
    ])
    assert_equal(Reporting::Nodes.new(), actual)

    # Get nodes sorted by environment, desc(1) order. `order: 0` is asc
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(sort: 'environment', order: 1, filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ])
    expected_nodes =
        {
            "nodes" => [
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
                    },
                    "profiles" => [
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
                },
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "passed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      },
                      {
                        "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "fake-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "failed",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
                }
            ],
            "total" => 5,
            "totalFailed" => 3,
            "totalPassed" => 1,
            "totalSkipped" => 1
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(sort: 'name', order: 0, filters: [
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "passed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      },
                      {
                        "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "fake-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "failed",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
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
                    },
                    "profiles" => [
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1"
                      }
                    ]
                }
            ],
            "total" => 5,
            "totalFailed" => 3,
            "totalPassed" => 1,
            "totalSkipped" => 1
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Cover the other sort fields:
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])], sort: 'platform', order: 1)
    assert_equal(Reporting::Nodes, resp.class)
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])], sort: 'latest_report.status')
    assert_equal(Reporting::Nodes, resp.class)
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])], sort: 'latest_report.end_time')
    assert_equal(Reporting::Nodes, resp.class)
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])], sort: 'latest_report.controls.failed.total')
    assert_equal(Reporting::Nodes, resp.class)
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])], sort: 'latest_report.controls.failed.critical')
    assert_equal(Reporting::Nodes, resp.class)

    # Node details API
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '9b9f4e51-b049-4b10-9555-10578916e149')
    expected_node = {
        "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
        "name" => "centos-beta",
        "platform" => {
            "name" => "centos",
            "release" => "5.11"
        },
        "environment" => "DevSec Prod beta",
        "latestReport" => {
            "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
            "endTime" => "2018-03-05T07:02:02Z",
            "status" => "failed",
            "controls" => {
                "total" => 18,
                "passed" => {
                    "total" => 3
                },
                "skipped" => {
                    "total" => 13
                },
                "failed" => {
                    "total" => 2,
                    "major" => 2
                }
            }
        },
        "profiles" => [
            {
                "name" => "nginx-baseline",
                "version" => "2.1.0",
                "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                "status": "failed"
            },
            {
                "name" => "apache-baseline",
                "version" => "2.0.1",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "status": "failed"
            }
        ]
    }
    assert_equal_json_sorted( expected_node.to_json, actual_node.to_json)

    # sort by node name ASC when a profile filter is used
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
            'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015',
            '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])

    ], sort: 'name', order: 0, page: 1, per_page: 2)
    expected = [
        "centos-beta",
        "redhat(2)-alpha-nginx(f)-apache(f)-failed"
    ]
    assert_equal(expected, resp['nodes'].map {|x| x['name']})

    # sort by node name DESC when a profile filter is used
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
            'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015',
            '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])

    ], sort: 'name', order: 1, page: 1, per_page: 2)
    expected = [
        "RedHat(2)-beta-nginx(f)-apache(s)-failed",
        "redhat(2)-alpha-nginx(f)-apache(s)-failed"
    ]
    assert_equal(expected, resp['nodes'].map {|x| x['name']})

    # sort by node platform now
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015', '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'platform', order: 1, page: 2, per_page: 2)
    assert_equal(["redhat", "centos"], resp['nodes'].map{ |x| x['platform']['name'] })

    # sort by node environment now
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988', '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'environment', order: 0, page: 1, per_page: 3)
    assert_equal(["DevSec Prod Alpha", "DevSec Prod Alpha", "DevSec Prod beta"], resp['nodes'].map {|x| x['environment']})


    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988', '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'environment', order: 1, page: 1, per_page: 3)
    assert_equal(["DevSec Prod Zeta", "DevSec Prod beta", "DevSec Prod beta"], resp['nodes'].map {|x| x['environment']})


    # sort by node failed controls now
    #todo - RDM this needs fixing!!
    # resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
    #     Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015']),
    #     Reporting::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
    # ], sort: 'latest_report.controls.failed.total', order: 0, page: 1, per_page: 2)
    # assert_equal([22], resp['nodes'].map {|x| x['latest_report']['controls']['failed']['total']})


    # test size(per_page) when filtering by profile. page is not used when filtering by profile due to aggregations
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9', 'yyy']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'environment', order: 0, page: 1, per_page: 2)
    assert_equal([14, 15], resp['nodes'].map {|x| x['latest_report']['controls']['skipped']['total']})
  end
end
