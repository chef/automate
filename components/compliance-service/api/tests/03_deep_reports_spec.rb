require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "list reports deep filtered by a profile that was not scanned in the last 24h" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'
        ])
      ]
    )
    expected_json = {}.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list reports deep filtered by a profile scanned in the last 24h" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4'])
      ]
    )
    actual_data_hash = actual_data.to_h
    actual_data_hash[:reports].each { |r| r[:end_time] = 'SOMETIME_IN_THE_LAST_24H' }
    expected_json = {
      "reports": [
        {
          "chef_organization": "",
          "chef_server": "",
          "chef_tags": [

          ],
          "controls": {
            "failed": {
              "critical": 0,
              "major": 0,
              "minor": 0,
              "total": 0
            },
            "passed": {
              "total": 4
            },
            "skipped": {
              "total": 1
            },
            "total": 5,
            "waived": {
              "total": 0
            }
          },
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "environment": "",
          "fqdn": "",
          "id": "44024b50-2e0d-42fa-cccc-aaaaaaaaa002",
          "ipaddress": "192.168.56.66",
          "job_id": "",
          "node_id": "34cbbb4c-c502-4971-1111-888888888888",
          "node_name": "osx(2)-omega-pro1(f)-pro2(w)-failed",
          "platform": nil,
          "profiles": [

          ],
          "projects": [

          ],
          "roles": [

          ],
          "statistics": nil,
          "status": "passed",
          "status_message": "",
          "version": ""
        },
        {
          "chef_organization": "",
          "chef_server": "",
          "chef_tags": [

          ],
          "controls": {
            "failed": {
              "critical": 0,
              "major": 0,
              "minor": 0,
              "total": 0
            },
            "passed": {
              "total": 4
            },
            "skipped": {
              "total": 1
            },
            "total": 5,
            "waived": {
              "total": 0
            }
          },
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "environment": "",
          "fqdn": "",
          "id": "44024b50-2e0d-42fa-cccc-aaaaaaaaa003",
          "ipaddress": "192.168.56.66",
          "job_id": "",
          "node_id": "34cbbb4c-c502-4971-1111-888888888888",
          "node_name": "osx(2)-omega-pro1(f)-pro2(w)-failed",
          "platform": nil,
          "profiles": [

          ],
          "projects": [

          ],
          "roles": [

          ],
          "statistics": nil,
          "status": "passed",
          "status_message": "",
          "version": ""
        },
        {
          "chef_organization": "",
          "chef_server": "",
          "chef_tags": [

          ],
          "controls": {
            "failed": {
              "critical": 2,
              "major": 0,
              "minor": 0,
              "total": 2
            },
            "passed": {
              "total": 0
            },
            "skipped": {
              "total": 0
            },
            "total": 5,
            "waived": {
              "total": 3
            }
          },
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "environment": "",
          "fqdn": "",
          "id": "44024b50-2e0d-42fa-cccc-aaaaaaaaa001",
          "ipaddress": "192.168.56.66",
          "job_id": "",
          "node_id": "34cbbb4c-c502-4971-1111-888888888888",
          "node_name": "osx(2)-omega-pro1(f)-pro2(w)-failed",
          "platform": nil,
          "profiles": [

          ],
          "projects": [

          ],
          "roles": [

          ],
          "statistics": nil,
          "status": "failed",
          "status_message": "",
          "version": ""
        }
      ],
      "total": 3
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data_hash.to_json)
  end

  it "list reports with deep profile filter, default pages" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ]
    )
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => "2018-03-04T00:00:10Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                "ipaddress"=>"8.8.8.1",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                "endTime" => "2018-03-04T09:19:42Z",
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
                    },
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "ipaddress"=>"8.8.8.2",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:42Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "ipaddress"=>"8.8.8.3",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:43Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
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
                    "failed" => {},
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => "2018-03-05T02:02:02Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
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
                    },
                    "waived"=>{}
                }
            }
        ],
        "total" => 7
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list reports with deep profile filter second page" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      per_page: 2
    )
    expected_json = {
        "reports": [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "ipaddress"=>"8.8.8.2",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:42Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {
                    },
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => "2018-03-04T00:00:10Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            }
        ],
        "total": 5
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list reports with deep profile filter third page" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ],
      sort: 'node_name',
      order: 0,
      page: 3,
      per_page: 2
    )
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "ipaddress"=>"8.8.8.2",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:42Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "ipaddress"=>"8.8.8.3",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:43Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            }
        ],
        "total" => 7
    }
    assert_equal_json_sorted(expected_json.to_json, actual_data.to_json)
  end

  it "list reports with dupe profile_id filter values" do
    #test that we are dedup-ing in case the same profile_id is passed in twice.. we still want dedup so that deep works
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(
          type: 'profile_id',
          values: [
            '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9',
            '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'
          ]),
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ],
      sort: 'node_name',
      order: 0,
      page: 3,
      per_page: 2
    )
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "ipaddress"=>"8.8.8.2",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:42Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "ipaddress"=>"8.8.8.3",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "endTime" => "2018-03-04T09:18:43Z",
                "status" => "skipped",
                "controls" => {
                    "total" => 14,
                    "passed" => {},
                    "skipped" => {
                        "total" => 14
                    },
                    "failed" => {},
                    "waived"=>{}
                }
            }
        ],
        "total" => 7
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list reports deep filtered by a profile failed at runtime" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
      ],
      sort: 'node_name',
      order: 0,
      page: 1,
      per_page: 3
    )
    expected_json = {"reports" =>
      [{"controls" => {"failed" => {}, "passed" => {}, "skipped" => {}, "waived" => {}},
        "endTime" => "2018-04-02T03:02:02Z",
        "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
        "ipaddress" => "188.38.98.100",
        "nodeId" => "999f4e51-b049-4b10-9555-555789999967",
        "nodeName" => "ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed",
        "status" => "failed"}],
     "total" => 1}.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list reports deep filtered by a profile skipped at runtime" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id',values: ['5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
      ],
      sort: 'node_name',
      order: 0,
      page: 1,
      per_page: 3
    )
    expected_json = {"reports" =>
      [{"controls" => {"failed" => {}, "passed" => {}, "skipped" => {}, "waived" => {}},
        "endTime" => "2018-04-02T03:02:02Z",
        "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
        "ipaddress" => "188.38.98.100",
        "nodeId" => "999f4e51-b049-4b10-9555-555789999967",
        "nodeName" => "ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed",
        "status" => "skipped"}],
     "total" => 1}.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "lists reports for two nodes, one missing" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149', 'missing-in-action']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-08-25T23:59:59Z'])
      ]
    )
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress"=>"10.3.4.5",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "nodeName" => "centos-beta",
                "endTime" => "2018-03-04T00:00:10Z",
                "status" => "passed",
                "controls" => {
                    "total" => 18,
                    "passed" => {
                        "total" => 3
                    },
                    "skipped" => {
                        "total" => 15
                    },
                    "failed" => {},
                    "waived"=>{}
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
                    "failed" => {},
                    "waived"=>{}
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
                    },
                    "waived"=>{}
                }
            }
        ],
        "total" => 3
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  #######Control depth###############
  it "lists reports with profile and control (apache-01) filter" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'control', values: ['apache-01']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-08-25T23:59:59Z'])
      ]
    )
    expected_json =
        {
            "reports" => [
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-04T00:00:10Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                    "ipaddress"=>"8.8.8.2",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "endTime" => "2018-03-04T09:18:42Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "ipaddress"=>"8.8.8.3",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "endTime" => "2018-03-04T09:18:43Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                    "ipaddress"=>"8.8.8.1",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "endTime" => "2018-03-04T09:19:42Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                    "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "nodeName" => "windows(1)-zeta-apache(s)-skipped",
                    "endTime" => "2018-03-04T10:18:41Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-05T02:02:02Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-05T07:02:02Z",
                    "status" => "passed",
                    "controls" => {
                        "total" => 1,
                        "passed" => {
                            "total" => 1
                        },
                        "skipped" => {},
                        "failed" => {},
                        "waived"=>{}
                    }
                }
            ],
            "total" => 7
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "lists reports with profile and control (apache-02) filter" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'control', values: ['apache-02']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-08-25T23:59:59Z'])
      ]
    )
    expected_json =
        {
            "reports" => [
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-04T00:00:10Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                    "ipaddress"=>"8.8.8.1",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "endTime" => "2018-03-04T09:19:42Z",
                    "status" => "failed",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {},
                        "failed" => {
                            "total" => 1,
                            "critical" => 1
                        },
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                    "ipaddress"=>"8.8.8.2",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "endTime" => "2018-03-04T09:18:42Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "ipaddress"=>"8.8.8.3",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "nodeName" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "endTime" => "2018-03-04T09:18:43Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
                    "nodeId" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "nodeName" => "windows(1)-zeta-apache(s)-skipped",
                    "endTime" => "2018-03-04T10:18:41Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-05T02:02:02Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                    "ipaddress"=>"10.3.4.5",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "nodeName" => "centos-beta",
                    "endTime" => "2018-03-05T07:02:02Z",
                    "status" => "skipped",
                    "controls" => {
                        "total" => 1,
                        "passed" => {},
                        "skipped" => {
                            "total" => 1
                        },
                        "failed" => {},
                        "waived"=>{}
                    }
                }
            ],
            "total" => 7
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "reads one report" do
    # Get a specific report1
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-08-25T23:59:59Z'])
      ])
    assert_equal(Reporting::Report, res.class)

    assert_equal('3.1.0', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('passed', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520121610, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11", full: "centos 5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(3, res['profiles'].length)
      assert_equal('nginx-baseline', res['profiles'][0]['name'])
      assert_equal(4, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal(["nginx-01", "nginx-02", "nginx-03", "nginx-04"], control_ids)

      assert_equal(Reporting::Report, res.class)

      assert_equal('nginx-01', res['profiles'][0]['controls'][0]['id'])
      assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
      assert_equal('no', res['profiles'][0]['controls'][0]['waived_str'])
      assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
      assert_equal('Running worker process as non-privileged user', res['profiles'][0]['controls'][0]['title'])
      assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)

      passed_control = res['profiles'][0]['controls'][0]['results'].first
      assert_equal('passed', passed_control['status'])

      assert_equal('skipped', res['profiles'][0]['depends'][0]['status'])
      assert_equal("Skipping profile: 'myprofile1' on unsupported platform: 'mac_os_x/17.7.0'.", res['profiles'][0]['depends'][0]['skip_message'])

      assert_equal('skipped', res['profiles'][2]['status'])
      assert_equal("Skipping profile: 'fake-baseline' on unsupported platform: 'amazon/2'.", res['profiles'][2]['status_message'])
    end
  end

  it "reads one report with a profile filter as well" do
    # Get a specific report filter by profile_id
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04',
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    assert_equal(Reporting::Report, res.class)
    assert_equal('3.1.0', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('skipped', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520121610, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11", full: "centos 5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)

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
  end

  it "reads one report with a profile and control filter" do
    # Get a specific report filter by profile_id and control
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04',
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: 'control', values: ['apache-01']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    assert_equal(Reporting::Report, res.class)

    assert_equal('3.1.0', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('skipped', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520121610, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11", full: "centos 5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 3.309065103530884), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(1, res['profiles'].length)
      assert_equal('apache-baseline', res['profiles'][0]['name'])
      assert_equal(1, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal(["apache-01"], control_ids)

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
