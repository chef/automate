require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "works" do

    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id',
                                  values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => "2018-03-04T09:18:41Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                "endTime" => {
                    "seconds" => 1520155122
                },
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
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => {
                    "seconds" => 1520155122
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "endTime" => {
                    "seconds" => 1520155123
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "nodeName" => "windows(1)-zeta-apache(s)-skipped",
                "endTime" => {
                    "seconds" => 1520158721
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => {
                    "seconds" => 1520215322
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => {
                    "seconds" => 1520233322
                },
                "status" => "failed",
                "controls" => {
                    "total" => 14,
                    "passed" => {
                        "total" => 1
                    },
                    "skipped" => {
                        "total" => 12
                    },
                    "failed" => {
                        "total" => 1,
                        "major" => 1
                    }
                },
                "profiles" => []
            }
        ],
        "total" => 7
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id',
                                  values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
    ], per_page: 2)
    expected_json = {
        "reports": [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => "2018-03-04T09:18:41Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                "endTime" => {
                    "seconds" => 1520155122
                },
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
                },
                "profiles" => []
            }
        ],
        "total": 7
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id',
                                  values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
    ], sort: 'node_name', order: 0, page: 3, per_page: 2)
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => {
                    "seconds" => 1520155122
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "endTime" => {
                    "seconds" => 1520155123
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            }
        ],
        "total" => 7
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    #test that we are dedup-ing in case the same profile_id is passed in twice.. we still want dedup so that deep works
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id',
                                  values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9',
                                           '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
    ], sort: 'node_name', order: 0, page: 3, per_page: 2)
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => {
                    "seconds" => 1520155122
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "endTime" => {
                    "seconds" => 1520155123
                },
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {}
                },
                "profiles" => []
            }
        ],
        "total" => 7
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get all reports for these two nodes, one missing
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id',
                                  values: ['9b9f4e51-b049-4b10-9555-10578916e149', 'missing-in-action'])
    ])
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => {
                    "seconds" => 1520215322
                },
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
                },
                "profiles" => []
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => {
                    "seconds" => 1520233322
                },
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
                },
                "profiles" => []
            }
        ],
        "total" => 3
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)


    #######Control depth###############

    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id',
                                  values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'control', values: ['apache-01'])
    ])
    expected_json =
        {
            "reports" => [
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-04T09:18:41Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "endTime" => {
                        "seconds" => 1520155122
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "endTime" => {
                        "seconds" => 1520155122
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "endTime" => {
                        "seconds" => 1520155123
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                    "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "nodeName" => "windows(1)-zeta-apache(s)-skipped",
                    "endTime" => {
                        "seconds" => 1520158721
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => {
                        "seconds" => 1520215322
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => {
                        "seconds" => 1520233322
                    },
                    "status" => "passed",
                    "controls" => {
                        "total" => 1,
                        "passed" => {
                            "total" => 1
                        },
                        "skipped" => {},
                        "failed" => {}
                    },
                    "profiles" => []
                }
            ],
            "total" => 7
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'profile_id',
                                  values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'control', values: ['apache-02'])
    ])
    expected_json =
        {
            "reports" => [
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-04T09:18:41Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "endTime" => {
                        "seconds" => 1520155122
                    },
                    "status" => "failed",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {
                            "total" => 1,
                            "critical" => 1
                        }
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "endTime" => {
                        "seconds" => 1520155122
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "endTime" => {
                        "seconds" => 1520155123
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                    "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "nodeName" => "windows(1)-zeta-apache(s)-skipped",
                    "endTime" => {
                        "seconds" => 1520158721
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => {
                        "seconds" => 1520215322
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => {
                        "seconds" => 1520233322
                    },
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {}
                    },
                    "profiles" => []
                }
            ],
            "total" => 7
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

    # Get a specific report1
    res = GRPC reporting, :read_report, Reporting::Query.new(id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04')
    assert_equal(Reporting::Report, res.class)

    puts res['version']
    assert_equal('3.1.3', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('passed', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520155121, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    puts res['profiles'].class

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(3, res['profiles'].length)
      assert_equal('nginx-baseline', res['profiles'][0]['name'])
      assert_equal(4, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal(control_ids, ["nginx-01", "nginx-02", "nginx-03", "nginx-04"])

      assert_equal(Reporting::Report, res.class)

      assert_equal('nginx-01', res['profiles'][0]['controls'][0]['id'])
      assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
      assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
      assert_equal('Running worker process as non-privileged user', res['profiles'][0]['controls'][0]['title'])
      assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)

      passed_control = res['profiles'][0]['controls'][0]['results'].first
      assert_equal('passed', passed_control['status'])

      assert_equal('skipped', res['profiles'][0]['depends'][0]['status'])
      assert_equal("Skipping profile: 'myprofile1' on unsupported platform: 'mac_os_x/17.7.0'.", res['profiles'][0]['depends'][0]['skip_message'])

      assert_equal('skipped', res['profiles'][2]['status'])
      assert_equal("Skipping profile: 'fake-baseline' on unsupported platform: 'amazon/2'.", res['profiles'][2]['skip_message'])
    end

    # Get a specific report filter by profile_id
    res = GRPC reporting, :read_report, Reporting::Query.new(
        id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04',
        filters: [
            Reporting::ListFilter.new(type: 'profile_id',
                                      values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])]
    )
    assert_equal(Reporting::Report, res.class)

    puts res['version']
    assert_equal('3.1.3', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('skipped', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520155121, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    puts res['profiles'].class

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(1, res['profiles'].length)
      assert_equal('apache-baseline', res['profiles'][0]['name'])
      assert_equal(14, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal([
                       "apache-01", "apache-02", "apache-03", "apache-04", "apache-05",
                       "apache-06", "apache-07", "apache-08", "apache-09", "apache-10",
                       "apache-11", "apache-12", "apache-13", "apache-14"
                   ], control_ids)

      assert_equal(Reporting::Report, res.class)

      assert_equal('apache-01', res['profiles'][0]['controls'][0]['id'])
      assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
      assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
      assert_equal('Apache should be running', res['profiles'][0]['controls'][0]['title'])
      assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)

      passed_control = res['profiles'][0]['controls'][0]['results'].first
      assert_equal('skipped', passed_control['status'])
    end

    # Get a specific report filter by profile_id and control
    res = GRPC reporting, :read_report, Reporting::Query.new(
        id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04',
        filters: [
            Reporting::ListFilter.new(type: 'profile_id',
                                      values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
            Reporting::ListFilter.new(type: 'control', values: ['apache-01'])
        ]
    )
    assert_equal(Reporting::Report, res.class)

    puts res['version']
    assert_equal('3.1.3', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('skipped', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520155121, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    puts res['profiles'].class

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(1, res['profiles'].length)
      assert_equal('apache-baseline', res['profiles'][0]['name'])
      assert_equal(1, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal([
                       "apache-01"
                   ], control_ids)

      assert_equal(Reporting::Report, res.class)

      assert_equal('apache-01', res['profiles'][0]['controls'][0]['id'])
      assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
      assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
      assert_equal('Apache should be running', res['profiles'][0]['controls'][0]['title'])
      assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)

      passed_control = res['profiles'][0]['controls'][0]['results'].first
      assert_equal('skipped', passed_control['status'])
    end

  end
end
