require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "works" do
    #### Failure tests #####
    message = "Parameter 'sort' only supports one of the following fields: [latest_report.controls.failed.critical latest_report.controls.failed.total latest_report.end_time latest_report.status node_name]"
    assert_grpc_error(message, 3) do
      GRPC reporting, :list_reports, Reporting::Query.new(sort: 'wrong')
    end

    assert_grpc_error("Not found for id: 1234", 5) do
      GRPC reporting, :read_report, Reporting::Query.new(id: '1234')
    end

    assert_grpc_error("Only one 'profile_id' filter is allowed", 3) do
      GRPC reporting, :read_report, Reporting::Query.new(filters: [
          Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9', 'one-too-many'])
      ], id: '44024b50-2e0d-42fa-a57c-25e05e48a1b5')
    end


    ##### Success tests #####
    # Get one specific node_id
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['a0ddd774-cbbb-49be-8730-49c92f3fc2a0'])
    ])
    expected_json =
        {
            "ids" => ["3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww"]
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get stuff since march 4th
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-10-25T00:00:00Z'])
    ])
    expected_json =
        {
            "ids" => ["3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc08",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc10"
            ]
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get report ids from a date range if they contain at least one control tagged 'web'
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'control_tag:web', values: []),
    ])
    expected_json =
        {
            "ids" => ["44024b50-2e0d-42fa-a57c-dddddddddddd",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                      "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc09"
            ]
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get all until march 4th up to and including beginning of day
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T00:00:00Z'])
    ])
    expected_json =
        {
            "ids" => [
                "44024b50-2e0d-42fa-a57c-dddddddddddd"
            ]
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Get those from a tiny window of time
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T09:18:41Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T09:18:41Z'])
    ])
    expected_json =
        {
            "ids" => [
                "bb93e1b2-36d6-439e-ac70-cccccccccc04"
            ]
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get all
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new()
    expected_json =
        {
            "ids" => ["3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                      "44024b50-2e0d-42fa-a57c-dddddddddddd",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc08",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                      "bb93e1b2-36d6-439e-ac70-cccccccccc10"
            ]
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    ##### Success tests #####
    # Get all reports
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new()
    expected_json =
        {
            "reports" => [
                {
                    "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                    "ipaddress"=>"192.168.56.33",
                    "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                    "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                    "endTime" => "2018-02-09T09:18:41Z",
                    "status" => "failed",
                    "controls" => {
                        "total" => 59,
                        "passed" => {
                            "total" => 24
                        },
                        "skipped" => {
                            "total" => 13
                        },
                        "failed" => {
                            "total" => 22,
                            "critical" => 22
                        }
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                    "ipaddress"=>"8.8.8.1",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                    "ipaddress"=>"8.8.8.2",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "ipaddress"=>"8.8.8.3",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
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
                {
                    "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                    "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "nodeName" => "windows(1)-zeta-apache(s)-skipped",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc08",
                    "ipaddress"=>"188.38.98.100",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e666",
                    "nodeName" => "ubuntu(1)-alpha-myprofile(s)-skipped",
                    "endTime" => "2018-03-07T03:02:02Z",
                    "status" => "skipped",
                    "controls" => {
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {}
                    }
                }
            ],
            "total" => 9
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Get first two reports(on page 1)
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(page: 1, per_page: 2)
    expected_json = {
        "reports" => [
            {
                "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                "ipaddress"=>"192.168.56.33",
                "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                "endTime" => "2018-02-09T09:18:41Z",
                "status" => "failed",
                "controls" => {
                    "total" => 59,
                    "passed" => {
                        "total" => 24
                    },
                    "skipped" => {
                        "total" => 13
                    },
                    "failed" => {
                        "total" => 22,
                        "critical" => 22
                    }
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
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
        ],
        "total" => 9
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Get "three" reports page 2
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(page: 2, per_page: 3)
    expected_json =
        {
            "reports" => [
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                    "ipaddress"=>"8.8.8.2",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
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
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "ipaddress"=>"8.8.8.3",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
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
                {
                    "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                    "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "nodeName" => "windows(1)-zeta-apache(s)-skipped",
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
            ],
            "total" => 9
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Get all reports for a node
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['a0ddd774-cbbb-49be-8730-49c92f3fc2a0'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "nodeName" => "windows(1)-zeta-apache(s)-skipped",
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
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get reports based on control tag filter
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'control_tag:scope', values: ['NGI*'])
    ])
    expected_json = {
        "reports" => [
          {
              "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
              "ipaddress" => "10.3.4.5",
              "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
              "nodeName" => "centos-beta",
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
          {
            "endTime" => "2018-03-04T09:18:43Z",
            "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
            "ipaddress" => "8.8.8.3",
            "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
            "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
            "status" => "failed",
            "controls" => {
              "failed" => {
                "critical" => 1,
                "major" => 1,
                "total" => 2
              },
              "passed" => {
                "total" => 1
              },
              "skipped" => {
                "total" => 15
              },
              "total" => 18
            }
          }
        ],
        "total" => 2
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get no reports as we find no match
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-22222missing'])
    ])
    expected_json = {}.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get all reports for a job
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                "ipaddress"=>"192.168.56.33",
                "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                "endTime" => "2018-02-09T09:18:41Z",
                "status" => "failed",
                "controls" => {
                    "total" => 59,
                    "passed" => {
                        "total" => 24
                    },
                    "skipped" => {
                        "total" => 13
                    },
                    "failed" => {
                        "total" => 22,
                        "critical" => 22
                    }
                }
            }
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Get reports before a specific time, sorted by end_time, ascending
    GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-02-25T00:00:00Z'])
    ], sort: 'latest_report.end_time', order: 0)
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                "ipaddress"=>"192.168.56.33",
                "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                "endTime" => "2018-02-09T09:18:41Z",
                "status" => "failed",
                "controls" => {
                    "total" => 59,
                    "passed" => {
                        "total" => 24
                    },
                    "skipped" => {
                        "total" => 13
                    },
                    "failed" => {
                        "total" => 22,
                        "critical" => 22
                    }
                }
            }
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Get all reports for a missing environment
    resp = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['missing-in-action'])
    ])
    assert_equal(Reporting::Reports.new(), resp)


    # Get all reports for these two nodes, one missing
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149', 'missing-in-action'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
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
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
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
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
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
            }
        ],
        "total" => 3
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_name', values: ['windows(1)-zeta-apache(s)-skipped', 'missing-in-action'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "nodeName" => "windows(1)-zeta-apache(s)-skipped",
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
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    # Cover the other sort fields, node_name desc:
    resp = GRPC reporting, :list_reports, Reporting::Query.new(sort: 'node_name', order: 1)
    assert_equal(Reporting::Reports, resp.class)
    expected = [
        "windows(1)-zeta-apache(s)-skipped",
        "ubuntu(1)-alpha-myprofile(s)-skipped",
        "RedHat(2)-beta-nginx(f)-apache(s)-failed",
        "redhat(2)-alpha-nginx(f)-apache(s)-failed",
        "redhat(2)-alpha-nginx(f)-apache(f)-failed",
        "debian(2)-zeta-linux(f)-apache(p)-failed",
        "centos-beta",
        "centos-beta",
        "centos-beta"
    ]
    assert_equal(expected, resp['reports'].map {|x| x['node_name']})

    resp = GRPC reporting, :list_reports, Reporting::Query.new(sort: 'node_name', order: 0)
    assert_equal(Reporting::Reports, resp.class)
    expected = [
        "centos-beta",
        "centos-beta",
        "centos-beta",
        "debian(2)-zeta-linux(f)-apache(p)-failed",
        "redhat(2)-alpha-nginx(f)-apache(f)-failed",
        "redhat(2)-alpha-nginx(f)-apache(s)-failed",
        "RedHat(2)-beta-nginx(f)-apache(s)-failed",
        "ubuntu(1)-alpha-myprofile(s)-skipped",
        "windows(1)-zeta-apache(s)-skipped"
    ]
    assert_equal(expected, resp['reports'].map {|x| x['node_name']})

    resp = GRPC reporting, :list_reports, Reporting::Query.new(sort: 'latest_report.status')
    assert_equal(Reporting::Reports, resp.class)
    assert_equal(["failed", "failed", "failed", "failed", "failed", "passed", "passed", "skipped", "skipped"], resp['reports'].map {|x| x['status']})

    resp = GRPC reporting, :list_reports, Reporting::Query.new(sort: 'latest_report.controls.failed.total')
    assert_equal(Reporting::Reports, resp.class)
    assert_equal([0, 0, 0, 0, 2, 2, 2, 2, 22], resp['reports'].map {|x| x['controls']['failed']['total']})

    resp = GRPC reporting, :list_reports, Reporting::Query.new(sort: 'latest_report.controls.failed.critical', order: 1)
    assert_equal(Reporting::Reports, resp.class)
    assert_equal(9, resp['reports'].length)

    # Get a specific report; ensure only the control with the specified filter tag is returned
    res = GRPC reporting, :read_report, Reporting::Query.new(id: '44024b50-2e0d-42fa-a57c-dddddddddddd',       
        filters: [
            Reporting::ListFilter.new(type: 'control_tag:tag1', values: ['value1']),
        ]
    )
    assert_equal(Reporting::Report, res.class)
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    assert_equal(2, res['profiles'].length)
    assert_equal(1, res['profiles'][0]['controls'].length)
    assert_equal(0, res['profiles'][1]['controls'].length)
    assert_equal('os-02', res['profiles'][0]['controls'][0]['id'])

    # Get a specific report1
    res = GRPC reporting, :read_report, Reporting::Query.new(id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04')
    assert_equal(Reporting::Report, res.class)

    puts res['version']
    assert_equal('3.1.0', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('passed', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520155121, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11", full: "centos 5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    puts res['profiles'].class

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(3, res['profiles'].length)
      assert_equal('nginx-baseline', res['profiles'][0]['name'])
      assert_equal(4, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal(["nginx-01", "nginx-02", "nginx-03", "nginx-04"], control_ids)

      assert_equal(Reporting::Report, res.class)

      assert_equal('nginx-01', res['profiles'][0]['controls'][0]['id'])
      assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
      assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
      assert_equal("testing-ref", res['profiles'][0]['controls'][0]['refs'][0]['ref'])
      assert_equal("test-url", res['profiles'][0]['controls'][0]['refs'][0]['url'])

      assert_equal('Running worker process as non-privileged user', res['profiles'][0]['controls'][0]['title'])
      assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)

      passed_control = res['profiles'][0]['controls'][0]['results'].first
      assert_equal('passed', passed_control['status'])

      assert_equal('skipped', res['profiles'][0]['depends'][0]['status'])
      assert_equal("Skipping profile: 'myprofile1' on unsupported platform: 'mac_os_x/17.7.0'.", res['profiles'][0]['depends'][0]['skip_message'])

      assert_equal('skipped', res['profiles'][2]['status'])
      assert_equal("Skipping profile: 'fake-baseline' on unsupported platform: 'amazon/2'.", res['profiles'][2]['skip_message'])
    end

    # Get a specific report
    # actual_data = GRPC reporting, :read_report, Reporting::Query.new(id: 'bb93e1b2-36d6-439e-ac70-cccccccccc05')
    # assert_assert_equal(Reporting::Report, actual_data.class)
    #
    # expected_json = {
    #   "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
    #   "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
    #   "nodeName" => "centos(2)-beta-nginx(p)-apache(s)-passed",
    #   "endTime" => {
    #     "seconds" => 1520215322
    #   },
    #   "status" => "passed",
    #   "environment" => "DevSec Prod beta",
    #   "version" => "3.1.3",
    #   "platform" => {
    #     "name" => "centos",
    #     "release" => "5.11"
    #   },
    #   "statistics" => {
    #     "duration" => 4.1090651
    #   },
    #   "profiles" => [
    #     {
    #       "name" => "nginx-baseline",
    #       "title" => "DevSec Nginx Baseline",
    #       "copyright" => "DevSec Hardening Framework Team",
    #       "copyrightEmail" => "hello@dev-sec.io",
    #       "summary" => "Test-suite for best-practice nginx hardening",
    #       "version" => "2.1.0",
    #
    #
    #       "sha256" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
    #
    #       "controls" => [
    #         {
    #           "id" => "nginx-01",
    #           "code" => "control 'nginx-01' do\n  impact 1.0\n  title 'Running worker process as non-privileged user'\n  desc 'The NGINX worker processes should run as non-privileged user. In case of compromise of the process, an attacker has full access to the system.'\n  describe user(nginx_lib.valid_users) do\n    it { should exist }\n  end\n  describe parse_config_file(nginx_conf, options) do\n    its('user') { should eq nginx_lib.valid_users }\n  end\n\n  describe parse_config_file(nginx_conf, options) do\n    its('group') { should_not eq 'root' }\n  end\nend\n",
    #           "desc" => "The NGINX worker processes should run as non-privileged user. In case of compromise of the process, an attacker has full access to the system.",
    #           "impact" => 1,
    #           "title" => "Running worker process as non-privileged user",
    #           "sourceLocation" => {
    #             "ref" => "nginx-baseline-master/controls/nginx_spec.rb",
    #             "line" => 99
    #           },
    #           "results" => [
    #             {
    #               "status" => "passed",
    #               "codeDesc" => "Worked like a charm baby!",
    #               "runTime" => 0.300134
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "nginx-02",
    #           "code" => "control 'nginx-02' do\n  impact 1.0\n  title 'Check NGINX config file owner, group and permissions.'\n  desc 'The NGINX config file should owned by root, only be writable by owner and not write- and readable by others.'\n  describe file(nginx_conf) do\n    it { should be_owned_by 'root' }\n    it { should be_grouped_into 'root' }\n    it { should_not be_readable.by('others') }\n    it { should_not be_writable.by('others') }\n    it { should_not be_executable.by('others') }\n  end\nend\n",
    #           "desc" => "The NGINX config file should owned by root, only be writable by owner and not write- and readable by others.",
    #           "impact" => 1,
    #           "title" => "Check NGINX config file owner, group and permissions.",
    #           "sourceLocation" => {
    #             "ref" => "nginx-baseline-master/controls/nginx_spec.rb",
    #             "line" => 115
    #           },
    #           "results" => [
    #             {
    #               "status" => "passed",
    #               "codeDesc" => "Worked like a charm baby!",
    #               "runTime" => 0.700194
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         }
    #       ],
    #       "attributes" => []
    #     },
    #     {
    #       "name" => "apache-baseline",
    #       "title" => "DevSec Apache Baseline",
    #       "copyright" => "Hardening Framework Team",
    #       "copyrightEmail" => "hello@dev-sec.io",
    #       "summary" => "Test-suite for best-practice apache hardening",
    #       "version" => "2.0.1",
    #
    #
    #       "sha256" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
    #
    #       "controls" => [
    #         {
    #           "id" => "apache-01",
    #           "code" => "control 'apache-01' do\n  impact 1.0\n  title 'Apache should be running'\n  desc 'Apache should be running.'\n  describe service(apache.service) do\n    it { should be_installed }\n    it { should be_running }\n  end\nend\n",
    #           "desc" => "Apache should be running.",
    #           "impact" => 1,
    #           "title" => "Apache should be running",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 29
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000060000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-02",
    #           "code" => "control 'apache-02' do\n  impact 1.0\n  title 'Apache should be enabled'\n  desc 'Configure apache service to be automatically started at boot time'\n  only_if { os[:family] != 'ubuntu' && os[:release] != '16.04' } || only_if { os[:family] != 'debian' && os[:release] != '8' }\n  describe service(apache.service) do\n    it { should be_enabled }\n  end\nend\n",
    #           "desc" => "Configure apache service to be automatically started at boot time",
    #           "impact" => 1,
    #           "title" => "Apache should be enabled",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 39
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-03",
    #           "code" => "control 'apache-03' do\n  title 'Apache should start max. 1 root-task'\n  desc 'The Apache service in its own non-privileged account. If the web server process runs with administrative privileges, an attack who obtains control over the apache process may control the entire system.'\n  total_tasks = command(\"ps aux | grep \#{apache.service} | grep -v grep | grep root | wc -l | tr -d [:space:]\").stdout.to_i\n  describe total_tasks do\n    it { should eq 1 }\n  end\nend\n",
    #           "desc" => "The Apache service in its own non-privileged account. If the web server process runs with administrative privileges, an attack who obtains control over the apache process may control the entire system.",
    #           "impact" => 0.5,
    #           "title" => "Apache should start max. 1 root-task",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 49
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-04",
    #           "code" => "control 'apache-04' do\n  impact 1.0\n  title 'Check Apache config folder owner, group and permissions.'\n  desc 'The Apache config folder should owned and grouped by root, be writable, readable and executable by owner. It should be readable, executable by group and not readable, not writeable by others.'\n  describe file(apache.conf_dir) do\n    it { should be_owned_by 'root' }\n    it { should be_grouped_into 'root' }\n    it { should be_readable.by('owner') }\n    it { should be_writable.by('owner') }\n    it { should be_executable.by('owner') }\n    it { should be_readable.by('group') }\n    it { should_not be_writable.by('group') }\n    it { should be_executable.by('group') }\n    it { should_not be_readable.by('others') }\n    it { should_not be_writable.by('others') }\n    it { should be_executable.by('others') }\n  end\nend\n",
    #           "desc" => "The Apache config folder should owned and grouped by root, be writable, readable and executable by owner. It should be readable, executable by group and not readable, not writeable by others.",
    #           "impact" => 1,
    #           "title" => "Check Apache config folder owner, group and permissions.",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 58
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-05",
    #           "code" => "control 'apache-05' do\n  impact 1.0\n  title 'Check Apache config file owner, group and permissions.'\n  desc 'The Apache config file should owned and grouped by root, only be writable and readable by owner and not write- and readable by others.'\n  describe file(apache.conf_path) do\n    it { should be_owned_by 'root' }\n    it { should be_grouped_into 'root' }\n    it { should be_readable.by('owner') }\n    it { should be_writable.by('owner') }\n    it { should_not be_executable.by('owner') }\n    it { should be_readable.by('group') }\n    it { should_not be_writable.by('group') }\n    it { should_not be_executable.by('group') }\n    it { should_not be_readable.by('others') }\n    it { should_not be_writable.by('others') }\n    it { should_not be_executable.by('others') }\n  end\n  describe file(File.join(apache.conf_dir, '/conf-enabled/hardening.conf')) do\n    it { should be_owned_by 'root' }\n    it { should be_grouped_into 'root' }\n    it { should be_readable.by('owner') }\n    it { should be_writable.by('owner') }\n    it { should_not be_executable.by('owner') }\n    it { should be_readable.by('group') }\n    it { should_not be_writable.by('group') }\n    it { should_not be_executable.by('group') }\n    it { should_not be_readable.by('others') }\n    it { should_not be_writable.by('others') }\n    it { should_not be_executable.by('others') }\n  end\nend\n",
    #           "desc" => "The Apache config file should owned and grouped by root, only be writable and readable by owner and not write- and readable by others.",
    #           "impact" => 1,
    #           "title" => "Check Apache config file owner, group and permissions.",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 77
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-06",
    #           "code" => "control 'apache-06' do\n  impact 1.0\n  title 'User and group should be set properly'\n  desc 'For security reasons it is recommended to run Apache in its own non-privileged account.'\n  describe apache_conf do\n    its('User') { should eq [apache.user] }\n    its('Group') { should eq [apache.user] }\n  end\nend\n",
    #           "desc" => "For security reasons it is recommended to run Apache in its own non-privileged account.",
    #           "impact" => 1,
    #           "title" => "User and group should be set properly",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 109
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-07",
    #           "code" => "control 'apache-07' do\n  impact 1.0\n  title 'Set the apache server token'\n  desc '\\'ServerTokens Prod\\' tells Apache to return only Apache as product in the server response header on the every page request'\n\n  describe file(File.join(apache.conf_dir, '/conf-enabled/security.conf')) do\n    its('content') { should match(/^ServerTokens Prod/) }\n  end\n\n  # open bug https://github.com/chef/inspec/issues/786, if the bug solved use this test\n  # describe apache_conf do\n  #   its('ServerTokens') { should eq 'Prod' }\n  # end\nend\n",
    #           "desc" => "'ServerTokens Prod' tells Apache to return only Apache as product in the server response header on the every page request",
    #           "impact" => 1,
    #           "title" => "Set the apache server token",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 119
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-08",
    #           "code" => "control 'apache-08' do\n  impact 1.0\n  title 'Should not load certain modules'\n  desc 'Apache HTTP should not load legacy modules'\n\n  module_path = File.join(apache.conf_dir, '/mods-enabled/')\n  loaded_modules = command('ls ' << module_path).stdout.split.keep_if { |file_name| /.load/.match(file_name) }\n\n  loaded_modules.each do |id|\n    describe file(File.join(module_path, id)) do\n      its('content') { should_not match(/^\\s*?LoadModule\\s+?dav_module/) }\n      its('content') { should_not match(/^\\s*?LoadModule\\s+?cgid_module/) }\n      its('content') { should_not match(/^\\s*?LoadModule\\s+?cgi_module/) }\n      its('content') { should_not match(/^\\s*?LoadModule\\s+?include_module/) }\n    end\n  end\n\n  # open bug https://github.com/chef/inspec/issues/786, if the bug solved use this test\n  # describe apache_conf do\n  #   its('LoadModule') { should_not eq 'dav_module' }\n  #   its('LoadModule') { should_not eq 'cgid_module' }\n  #   its('LoadModule') { should_not eq 'cgi_module' }\n  #   its('LoadModule') { should_not eq 'include_module' }\n  #   its('content') { should_not match(/^\\s*?LoadModule\\s+?dav_module/) }\n  #   its('content') { should_not match(/^\\s*?LoadModule\\s+?cgid_module/) }\n  #   its('content') { should_not match(/^\\s*?LoadModule\\s+?cgi_module/) }\n  #   its('content') { should_not match(/^\\s*?LoadModule\\s+?include_module/) }\n  # end\nend\n",
    #           "desc" => "Apache HTTP should not load legacy modules",
    #           "impact" => 1,
    #           "title" => "Should not load certain modules",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 134
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-09",
    #           "code" => "control 'apache-09' do\n  impact 1.0\n  title 'Disable TRACE-methods'\n  desc 'The web server doesn’t allow TRACE request and help in blocking Cross Site Tracing attack.'\n\n  describe file(File.join(apache.conf_dir, '/conf-enabled/security.conf')) do\n    its('content') { should match(/^\\s*?TraceEnable\\s+?Off/) }\n  end\n\n  # open bug https://github.com/chef/inspec/issues/786, if the bug solved use this test\n  # describe apache_conf do\n  #   its('TraceEnable') { should eq 'Off' }\n  # end\nend\n",
    #           "desc" => "The web server doesn’t allow TRACE request and help in blocking Cross Site Tracing attack.",
    #           "impact" => 1,
    #           "title" => "Disable TRACE-methods",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 164
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-10",
    #           "code" => "control 'apache-10' do\n  impact 1.0\n  title 'Disable insecure HTTP-methods'\n  desc 'Disable insecure HTTP-methods and allow only necessary methods.'\n\n  describe file(File.join(apache.conf_dir, '/conf-enabled/hardening.conf')) do\n    its('content') { should match(/^\\s*?<LimitExcept\\s+?GET\\s+?POST>/) }\n  end\n\n  # open bug https://github.com/chef/inspec/issues/786, if the bug solved use this test\n  # describe apache_conf do\n  #   its('LimitExcept') { should eq ['GET','POST'] }\n  # end\nend\n",
    #           "desc" => "Disable insecure HTTP-methods and allow only necessary methods.",
    #           "impact" => 1,
    #           "title" => "Disable insecure HTTP-methods",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 179
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-11",
    #           "code" => "control 'apache-11' do\n  impact 1.0\n  title 'Disable Apache’s follows Symbolic Links for directories in alias.conf'\n  desc 'Should include -FollowSymLinks or +SymLinksIfOwnerMatch for directories in alias.conf'\n\n  describe file(File.join(apache.conf_dir, '/mods-enabled/alias.conf')) do\n    its('content') { should match(/-FollowSymLinks/).or match(/\\+SymLinksIfOwnerMatch/) }\n  end\nend\n",
    #           "desc" => "Should include -FollowSymLinks or +SymLinksIfOwnerMatch for directories in alias.conf",
    #           "impact" => 1,
    #           "title" => "Disable Apache’s follows Symbolic Links for directories in alias.conf",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 194
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-12",
    #           "code" => "control 'apache-12' do\n  impact 1.0\n  title 'Disable Directory Listing for directories in alias.conf'\n  desc 'Should include -Indexes for directories in alias.conf'\n\n  describe file(File.join(apache.conf_dir, '/mods-enabled/alias.conf')) do\n    its('content') { should match(/-Indexes/) }\n  end\nend\n",
    #           "desc" => "Should include -Indexes for directories in alias.conf",
    #           "impact" => 1,
    #           "title" => "Disable Directory Listing for directories in alias.conf",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 204
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-13",
    #           "code" => "control 'apache-13' do\n  impact 1.0\n  title 'SSL honor cipher order'\n  desc 'When choosing a cipher during an SSLv3 or TLSv1 handshake, normally the client\\'s preference is used. If this directive is enabled, the server\\'s preference will be used instead.'\n\n  describe file(File.join(apache.conf_dir, '/mods-enabled/ssl.conf')) do\n    its('content') { should match(/^\\s*?SSLHonorCipherOrder\\s+?On/i) }\n  end\n\n  sites_enabled_path = File.join(apache.conf_dir, '/sites-enabled/')\n  loaded_sites = command('ls ' << sites_enabled_path).stdout.split.keep_if { |file_name| /.conf/.match(file_name) }\n\n  loaded_sites.each do |id|\n    virtual_host = file(File.join(sites_enabled_path, id)).content.gsub(/#.*$/, '').scan(%r{<virtualhost.*443(.*?)<\\/virtualhost>}im).flatten\n    next if virtual_host.empty?\n    describe virtual_host do\n      it { should include(/^\\s*?SSLHonorCipherOrder\\s+?On/i) }\n    end\n  end\nend\n",
    #           "desc" => "When choosing a cipher during an SSLv3 or TLSv1 handshake, normally the client's preference is used. If this directive is enabled, the server's preference will be used instead.",
    #           "impact" => 1,
    #           "title" => "SSL honor cipher order",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 214
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.000002,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         },
    #         {
    #           "id" => "apache-14",
    #           "code" => "control 'apache-14' do\n  impact 1.0\n  title 'Enable Apache Logging'\n  desc 'Apache allows you to logging independently of your OS logging. It is wise to enable Apache logging, because it provides more information, such as the commands entered by users that have interacted with your Web server.'\n\n  sites_enabled_path = File.join(apache.conf_dir, '/sites-enabled/')\n  loaded_sites = command('ls ' << sites_enabled_path).stdout.split.keep_if { |file_name| /.conf/.match(file_name) }\n\n  loaded_sites.each do |id|\n    describe file(File.join(sites_enabled_path, id)).content.gsub(/#.*$/, '').scan(%r{<virtualhost(.*?)<\\/virtualhost>}im).flatten do\n      it { should include(/CustomLog.*$/i) }\n    end\n  end\nend\n",
    #           "desc" => "Apache allows you to logging independently of your OS logging. It is wise to enable Apache logging, because it provides more information, such as the commands entered by users that have interacted with your Web server.",
    #           "impact" => 1,
    #           "title" => "Enable Apache Logging",
    #           "sourceLocation" => {
    #             "ref" => "./.tmp/profiles/dist/unpacked/apache-baseline-2.0.1.tar.gz/apache-baseline-2.0.1/controls/apache_spec.rb",
    #             "line" => 235
    #           },
    #           "results" => [
    #             {
    #               "status" => "skipped",
    #               "codeDesc" => "Operating System Detection",
    #               "runTime" => 0.0000030000001,
    #               "skipMessage" => "Skipped control due to only_if condition."
    #             }
    #           ],
    #           "refs" => [],
    #           "tags" => {}
    #         }
    #       ],
    #       "attributes" => []
    #     }
    #   ]
    # }.to_json
    # in case you need to update ^^^ run the print below and escape #{apache.service} as \#{apache.service}
    # puts actual_data.to_json

    expected_json = expected_json.gsub("2.0e-06", "2e-06")
    assert_equal(expected_json, actual_data.to_json)


    res = GRPC reporting, :read_report, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_name', values: ['apache-baseline'])
    ], id: '3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww')
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(1, res['profiles'].length)
      assert_equal('apache-baseline', res['profiles'][0]['name'])
      assert_equal('3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww', res['id'])
    end
  end
end
