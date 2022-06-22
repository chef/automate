require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "list_reports errors on invalid sort option" do
    #### Failure tests #####
    message = "Parameter 'sort' only supports one of the following fields: [latest_report.controls.failed.critical latest_report.controls.failed.total latest_report.end_time latest_report.status node_name]"
    assert_grpc_error(message, 3) do
      GRPC reporting, :list_reports, Reporting::Query.new(sort: 'wrong')
    end
  end

  it "list_reports errors on missing id" do
    assert_grpc_error("Not found for id: 1234", 5) do
      GRPC reporting, :read_report, Reporting::Query.new(id: '1234')
    end
  end

  it "list_reports errors if more than one profile_id filter value is provided" do
    assert_grpc_error("Only one 'profile_id' filter is allowed", 3) do
      GRPC reporting, :read_report, Reporting::Query.new(
        filters: [
          Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9', 'one-too-many'])
        ],
        id: '44024b50-2e0d-42fa-a57c-25e05e48a1b5'
      )
    end
  end

  it "list_report_ids returns nothing for a node NOT scanned in the last 24h" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['a0ddd774-cbbb-49be-8730-49c92f3fc2a0'])
      ])
    expected_json = {}.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_report_ids returns the latest report id for a node scanned in the last 24h" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['34cbbb4c-c502-4971-1111-888888888888'])
      ]
    )
    actual_data_hash = actual_data.to_h
    actual_data_hash[:report_data].each { |r| r[:end_time] = 'SOMETIME_IN_THE_LAST_24H' }
    expected_json = {
      "ids"=>[
        "44024b50-2e0d-42fa-cccc-aaaaaaaaa003"
      ],
      "report_data"=>[
        { "end_time"=>"SOMETIME_IN_THE_LAST_24H", "id"=>"44024b50-2e0d-42fa-cccc-aaaaaaaaa003" }
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data_hash.to_json)
  end

  it "list_report_ids errors on missing id" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['a0ddd774-cbbb-49be-8730-49c92f3fc2a0']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_json = {
      "ids" => ["3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww"],
      "reportData" => [
        {"endTime" => "2018-03-04T10:18:41Z", "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww"}
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_report_ids returns ids since March 04" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-10-25T00:00:00Z'])
      ]
    )
    expected_json = {
      "ids" => [
        "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
        "44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy",
        "44024b50-2e0d-42fa-dddd-wwwwwwwwwwww",
        "bb93e1b2-36d6-439e-ac70-cccccccccc05",
        "bb93e1b2-36d6-439e-ac70-cccccccccc06",
        "bb93e1b2-36d6-439e-ac70-cccccccccc08",
        "bb93e1b2-36d6-439e-ac70-cccccccccc09",
        "bb93e1b2-36d6-439e-ac70-cccccccccc10",
        "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
        "zz93e1b2-36d6-439e-ac70-cccccccccckk"
      ],
      "reportData" => [
        {"endTime" => "2018-03-04T10:18:41Z", "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww"},
        {"endTime" => "2018-04-01T10:18:41Z", "id" => "44024b50-2e0d-42fa-dddd-wwwwwwwwwwww"},
        {"endTime" => "2018-04-01T10:18:41Z", "id" => "44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy"},
        {"endTime" => "2018-03-05T02:02:02Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05"},
        {"endTime" => "2018-03-04T09:18:42Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06"},
        {"endTime" => "2018-03-07T00:00:10Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc08"},
        {"endTime" => "2018-03-04T09:18:43Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09"},
        {"endTime" => "2018-03-04T09:19:42Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10"},
        {"endTime" => "2018-04-02T03:02:02Z", "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20"},
        {"endTime" => "2018-04-03T11:02:02Z", "id" => "zz93e1b2-36d6-439e-ac70-cccccccccckk"}
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_report_ids return ids from a date range if they contain at least one control tagged 'web'" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'control_tag:web', values: []),
      ]
    )
    expected_json = {
      "ids" => [
        "44024b50-2e0d-42fa-a57c-dddddddddddd",
        "bb93e1b2-36d6-439e-ac70-cccccccccc04",
        "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
        "bb93e1b2-36d6-439e-ac70-cccccccccc09"
      ],
      "reportData" => [
        {"endTime" => "2018-02-09T23:59:50Z", "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd"},
        {"endTime" => "2018-03-04T00:00:10Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04"},
        {"endTime" => "2018-03-04T09:18:43Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09"},
        {"endTime" => "2018-03-04T10:18:41Z", "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww"}
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_report_ids get all until march 4th up to and including beginning of day" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T00:00:00Z'])
      ]
    )
    expected_json = {
      "ids" => [
        "44024b50-2e0d-42fa-a57c-dddddddddddd"
      ],
      "reportData" => [
        {"endTime" => "2018-02-09T23:59:50Z", "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd"}
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_report_ids get those from a tiny window of time" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:00:10Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T00:00:10Z'])
      ]
    )
    expected_json = {
      "ids" => [
        "bb93e1b2-36d6-439e-ac70-cccccccccc04"
      ],
      "reportData" => [
        {"endTime" => "2018-03-04T00:00:10Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04"}
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_report_ids get all for 2018" do
    actual_data = GRPC reporting, :list_report_ids, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-12-31T23:59:59Z'])
      ]
    )
    expected_json = {
      "ids" => [
        "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww",
        "44024b50-2e0d-42fa-a57c-dddddddddddd",
        "44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy",
        "44024b50-2e0d-42fa-dddd-wwwwwwwwwwww",
        "bb93e1b2-36d6-439e-ac70-cccccccccc05",
        "bb93e1b2-36d6-439e-ac70-cccccccccc06",
        "bb93e1b2-36d6-439e-ac70-cccccccccc08",
        "bb93e1b2-36d6-439e-ac70-cccccccccc09",
        "bb93e1b2-36d6-439e-ac70-cccccccccc10",
        "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
        "zz93e1b2-36d6-439e-ac70-cccccccccckk"
      ],
      "reportData" => [
        {"endTime" => "2018-02-09T23:59:50Z", "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd"},
        {"endTime" => "2018-03-04T09:18:42Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06"},
        {"endTime" => "2018-03-04T09:18:43Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09"},
        {"endTime" => "2018-03-04T09:19:42Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10"},
        {"endTime" => "2018-03-04T10:18:41Z", "id" => "3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww"},
        {"endTime" => "2018-03-05T02:02:02Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05"},
        {"endTime" => "2018-03-07T00:00:10Z", "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc08"},
        {"endTime" => "2018-04-01T10:18:41Z", "id" => "44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy"},
        {"endTime" => "2018-04-01T10:18:41Z", "id" => "44024b50-2e0d-42fa-dddd-wwwwwwwwwwww"},
        {"endTime" => "2018-04-02T03:02:02Z", "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20"},
        {"endTime" => "2018-04-03T11:02:02Z", "id" => "zz93e1b2-36d6-439e-ac70-cccccccccckk"}
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports get all for 2018" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      per_page: 20,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-12-31T23:59:59Z'])
      ]
    )
    expected_json = {
        "reports" => [
            {
              "controls"=>{
                "failed"=>{},
                "passed"=>{},
                "skipped"=>{},
                "total"=>1,
                "waived"=>{"total"=>1}
              },
              "endTime"=>"2018-04-01T10:18:41Z",
              "id"=>"44024b50-2e0d-42fa-dddd-wwwwwwwwwwww",
              "ipaddress"=>"192.168.56.66",
              "nodeId"=>"34cbbb55-c502-4971-2222-999999999999",
              "nodeName"=>"osx(1)-omega-pro2(w)-waived",
              "status"=>"waived"
            },
            {
              "endTime" => "2018-04-01T10:18:41Z",
              "id" => "44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy",
              "ipaddress" => "192.168.56.66",
              "nodeId" => "34cbbb4c-c502-4971-1111-888888888888",
              "nodeName" => "osx(2)-omega-pro1(f)-pro2(w)-failed",
              "status" => "failed",
              "controls" => {
                "failed" => {"critical" => 2, "total" => 2},
                "passed" => {},
                "skipped" => {},
                "waived" => {"total":4},
                "total" => 6
              }
            },
            {
                "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                "ipaddress" => "192.168.56.33",
                "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                "endTime" => "2018-02-09T23:59:50Z",
                "status" => "failed",
                "controls" => {
                    "total" => 59,
                    "passed" => {
                        "total" => 23
                    },
                    "skipped" => {
                        "total" => 12
                    },
                    "failed" => {
                        "total" => 21,
                        "critical" => 21
                    },
                    "waived" => {
                        "total" => 3
                    }
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                "ipaddress" => "8.8.8.1",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                "endTime" => "2018-03-04T09:19:42Z",
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
                    },
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc06",
                "ipaddress" => "8.8.8.2",
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
                    },
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                "ipaddress" => "8.8.8.3",
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
                    },
                    "waived" => {}
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
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            },
            {
               "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
               "ipaddress" => "188.38.98.100",
               "nodeId" => "999f4e51-b049-4b10-9555-555789999967",
               "nodeName" => "ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed",
               "endTime" => "2018-04-02T03:02:02Z",
               "status" => "failed",
               "controls" => {
                 "failed" => {},
                 "passed" => {"total" => 1},
                 "skipped" => {},
                 "total" => 1,
                 "waived" => {}
               }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc08",
                "ipaddress" => "188.38.98.100",
                "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e666",
                "nodeName" => "ubuntu(1)-alpha-myprofile(s)-skipped",
                "endTime" => "2018-03-07T00:00:10Z",
                "status" => "skipped",
                "controls" => {
                    "passed" => {},
                    "skipped" => {},
                    "failed" => {},
                    "waived" => {}
                }
            },
            {
                "id" => "zz93e1b2-36d6-439e-ac70-cccccccccckk",
                "ipaddress" => "188.38.98.166",
                "nodeId" => "888f4e51-b049-4b10-9555-111222333333",
                "nodeName" => "ubuntu(0)-alpha-failed",
                "endTime" => "2018-04-03T11:02:02Z",
                "status" => "failed",
                "controls" => {
                  "failed" => {},
                  "passed" => {},
                  "skipped" => {},
                  "waived" => {}
                }
            }
        ],
        "total" => 13
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports get all reports for the last 24h" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(per_page: 20)
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
              "total": 0
            },
            "skipped": {
              "total": 0
            },
            "total": 0,
            "waived": {
              "total": 0
            }
          },
          "end_time": "SOMETIME_IN_THE_LAST_24H",
          "environment": "",
          "fqdn": "",
          "id": "zz93e1b2-36d6-439e-ac70-cccccccceemm",
          "ipaddress": "188.38.98.166",
          "job_id": "",
          "node_id": "888f4e51-b049-4b10-9555-111222333333",
          "node_name": "ubuntu(0)-alpha-failed",
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
      "total": 4
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data_hash.to_json)
  end

  it "list_reports get first two reports on page 1" do
    # Get first two reports(on page 1)
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      page: 1,
      per_page: 2,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-12-31T23:59:58Z'])
      ]
    )
    expected_json = {
        "reports" => [
            {
                "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                "ipaddress" => "192.168.56.33",
                "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                "endTime" => "2018-02-09T23:59:50Z",
                "status" => "failed",
                "controls" => {
                    "total" => 59,
                    "passed" => {
                        "total" => 23
                    },
                    "skipped" => {
                        "total" => 12
                    },
                    "failed" => {
                        "total" => 21,
                        "critical" => 21
                    },
                    "waived" => {
                        "total" => 3
                    }
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            }
        ],
        "total" => 13
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports get three reports on page 2" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      page: 2,
      per_page: 3,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-12-31T23:59:58Z'])
      ]
    )
    expected_json =
        {
            "reports" => [
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc09",
                    "ipaddress" => "8.8.8.3",
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
                        },
                        "waived" => {}
                    }
                },
                {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
                    "ipaddress" => "8.8.8.1",
                    "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "nodeName" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "endTime" => "2018-03-04T09:19:42Z",
                    "status" => "failed",
                    "controls" => {
                        "failed" => {
                            "total" => 2,
                            "critical" => 1,
                            "major" => 1
                        },
                        "passed" => {
                            "total" => 2
                        },
                        "skipped" => {
                            "total" => 14
                        },
                        "total" => 18,
                        "waived" => {}
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
                        "waived" => {}
                    }
                }
            ],
            "total" => 13
        }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports get all reports for a node" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['a0ddd774-cbbb-49be-8730-49c92f3fc2a0']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
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
              "failed" => {},
              "waived" => {}
          }
        }
      ],
      "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports based on control tag filter" do
    # Get reports based on control tag filter
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'control_tag:scope', values: ['NGI*']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-02T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-07T23:59:59Z'])
      ]
    )
    expected_json = {
        "reports" => [
          {
              "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
              "ipaddress" => "10.3.4.5",
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
                  "waived" => {}
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
              "waived" => {},
              "total" => 18
            }
          },
          {
            "controls" => {
              "failed" => {
                "major" => 2,
                "total" => 2
              },
              "passed" => {
                "total" => 3
              },
              "skipped" => {
                "total" => 13
              },
              "total" => 18,
              "waived" => {}
            },
            "endTime" => "2018-03-05T07:02:02Z",
            "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
            "ipaddress" => "10.3.4.5",
            "nodeId" => "9b9f4e51-b049-4b10-9555-10578916e149",
            "nodeName" => "centos-beta",
            "status" => "failed"
          }
        ],
        "total" => 3
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports returns nothing for a missing job" do
    # Get no reports as we find no match
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-22222missing'])
      ]
    )
    expected_json = {}.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports gets the report for a job" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-03-07T23:59:59Z'])
      ]
    )
    expected_json = {
      "reports" => [
          {
              "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
              "ipaddress" => "192.168.56.33",
              "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
              "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
              "endTime" => "2018-02-09T23:59:50Z",
              "status" => "failed",
              "controls" => {
                  "total" => 59,
                  "passed" => {
                      "total" => 23
                  },
                  "skipped" => {
                      "total" => 12
                  },
                  "failed" => {
                      "total" => 21,
                      "critical" => 21
                  },
                  "waived" => {
                      "total" => 3
                  }
              }
          }
      ],
      "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports before a specific time, sorted by end_time, ascending" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-02-25T00:00:00Z'])
      ],
      sort: 'latest_report.end_time',
      order: 0
    )
    expected_json = {
        "reports" => [
            {
                "id" => "44024b50-2e0d-42fa-a57c-dddddddddddd",
                "ipaddress" => "192.168.56.33",
                "nodeId" => "34cbbb4c-c502-4971-b193-00e987b4678c",
                "nodeName" => "debian(2)-zeta-linux(f)-apache(p)-failed",
                "endTime" => "2018-02-09T23:59:50Z",
                "status" => "failed",
                "controls" => {
                    "total" => 59,
                    "passed" => {
                        "total" => 23
                    },
                    "skipped" => {
                        "total" => 12
                    },
                    "failed" => {
                        "total" => 21,
                        "critical" => 21
                    },
                    "waived" => {
                        "total" => 3
                    }
                }
            }
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports for a missing environment" do
    resp = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['missing-in-action'])
      ]
    )
    assert_equal(Reporting::Reports.new(), resp)
  end

  it "list_reports for these two nodes, one missing" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149', 'missing-in-action']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-25T00:00:00Z'])
      ]
    )
    expected_json = {
        "reports" => [
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc05",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            },
            {
                "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc07",
                "ipaddress" => "10.3.4.5",
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
                    "waived" => {}
                }
            }
        ],
        "total" => 3
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports for these two nodes, one windows, one missing" do
    actual_data = GRPC reporting, :list_reports, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_name', values: ['windows(1)-zeta-apache(s)-skipped', 'missing-in-action']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:00:00Z'])
      ]
    )
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
                    "failed" => {},
                    "waived" => {}
                }
            }
        ],
        "total" => 1
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)
  end

  it "list_reports sorted by node_name, desc" do
    resp = GRPC reporting, :list_reports, Reporting::Query.new(
      sort: 'node_name',
      order: 1,
      per_page: 12,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Reports, resp.class)
    expected = [
      "windows(1)-zeta-apache(s)-skipped",
      "ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed",
      "ubuntu(1)-alpha-myprofile(s)-skipped",
      "ubuntu(0)-alpha-failed",
      "RedHat(2)-beta-nginx(f)-apache(s)-failed",
      "redhat(2)-alpha-nginx(f)-apache(s)-failed",
      "redhat(2)-alpha-nginx(f)-apache(f)-failed",
      "osx(2)-omega-pro1(f)-pro2(w)-failed",
      "osx(1)-omega-pro2(w)-waived",
      "debian(2)-zeta-linux(f)-apache(p)-failed",
      "centos-beta",
      "centos-beta"
    ]
    assert_equal(expected, resp['reports'].map {|x| x['node_name']})
  end

  it "list_reports sorted by node_name, asc" do
    resp = GRPC reporting, :list_reports, Reporting::Query.new(
      sort: 'node_name',
      order: 0,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Reports, resp.class)
    expected = [
        "centos-beta",
        "centos-beta",
        "centos-beta",
        "debian(2)-zeta-linux(f)-apache(p)-failed",
        "osx(1)-omega-pro2(w)-waived",
        "osx(2)-omega-pro1(f)-pro2(w)-failed",
        "redhat(2)-alpha-nginx(f)-apache(f)-failed",
        "redhat(2)-alpha-nginx(f)-apache(s)-failed",
        "RedHat(2)-beta-nginx(f)-apache(s)-failed",
        "ubuntu(0)-alpha-failed"
    ]
    assert_equal(expected, resp['reports'].map {|x| x['node_name']})
  end

  it "list_reports sorted by status, asc" do
    resp = GRPC reporting, :list_reports, Reporting::Query.new(
      sort: 'latest_report.status',
      per_page: 20,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Reports, resp.class)
    assert_equal(["failed", "failed", "failed", "failed", "failed", "failed", "failed", "failed", "passed", "passed", "skipped", "skipped", "waived"], resp['reports'].map {|x| x['status']})
  end

  it "list_reports sorted by total failed controls, asc" do
    resp = GRPC reporting, :list_reports, Reporting::Query.new(
      sort: 'latest_report.controls.failed.total',
      per_page: 20,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Reports, resp.class)
    assert_equal([0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 21], resp['reports'].map {|x| x['controls']['failed']['total']})
  end

  it "list_reports sorted by critical failed controls, desc" do
    resp = GRPC reporting, :list_reports, Reporting::Query.new(
      sort: 'latest_report.controls.failed.critical',
      order: 1,
      per_page: 50,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Reports, resp.class)
    assert_equal(13, resp['reports'].length)
  end

  it "read_report retrieves a report with a control tag filter" do
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: '44024b50-2e0d-42fa-a57c-dddddddddddd',
      filters: [
        Reporting::ListFilter.new(type: 'control_tag:tag1', values: ['value1']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T07:07:07Z'])
      ]
    )
    assert_equal(Reporting::Report, res.class)
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    assert_equal(2, res['profiles'].length)
    assert_equal(1, res['profiles'][0]['controls'].length)
    assert_equal(0, res['profiles'][1]['controls'].length)
    assert_equal('os-02', res['profiles'][0]['controls'][0]['id'])
  end

  it "read_report retrieves a specific report" do
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: 'bb93e1b2-36d6-439e-ac70-cccccccccc04',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Report, res.class)

    assert_equal('3.1.0', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc04', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos-beta', res['node_name'])
    assert_equal('DevSec Prod beta', res['environment'])
    assert_equal('passed', res['status'])
    assert_equal('localhost', res['chef_server'])
    assert_equal('my-org', res['chef_organization'])
    assert_equal(['fake-tag'], res['chef_tags'])
    assert_equal(['base_linux', 'apache_linux', 'nginx-hardening-prod', 'dot.role'], res['roles'])
    assert_equal([], res['projects'])

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

      control = res['profiles'].first['controls'].first
      assert_equal('nginx-01', control['id'])
      assert_equal(1, control['impact'])
      assert_equal(Google::Protobuf::Map, control['tags'].class)
      ref = control['refs'].first
      assert_equal("testing-ref", ref['ref'])
      assert_equal("test-url", ref['url'])

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

  it "read_report retrieves a report with a profile name filter" do
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: '3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww',
      filters: [
        Reporting::ListFilter.new(type: 'profile_name', values: ['apache-baseline']),
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(1, res['profiles'].length)
      assert_equal('apache-baseline', res['profiles'][0]['name'])
      assert_equal('3ca95021-84c1-43a6-a2e7-wwwwwwwwwwww', res['id'])
    end
  end

  it "read_report retrieves a report with some waived controls" do
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: '44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ]
    )
    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(2, res['profiles'].length)
      assert_equal(5, res['profiles'][0]['controls'].length)
      assert_equal('yes_run', res['profiles'][0]['controls'][0]['waived_str'])
      assert_equal_json_sorted('{"justification": "Sound reasoning", "run": true}', res['profiles'][0]['controls'][0]['waiver_data'].to_json)

      assert_equal('no_expired', res['profiles'][0]['controls'][2]['waived_str'])
      assert_equal_json_sorted('{"expirationDate": "1977-06-01", "justification": "Necessity", "message": "Waiver expired on 1977-06-01, evaluating control normally"}', res['profiles'][0]['controls'][2]['waiver_data'].to_json)
    end
  end

  it "read_report retrieves a report with multiple root level failed/skipped profiles" do
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: 'bb93e1b2-36d6-439e-ac70-ccccccczzz20',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2017-01-01T00:00:00Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2020-04-25T00:00:00Z'])
      ])
    assert_equal(Reporting::Report, res.class)

    assert_equal('4.22.0', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-ccccccczzz20', res['id'])
    assert_equal('999f4e51-b049-4b10-9555-555789999967', res['node_id'])
    assert_equal('ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed', res['node_name'])
    assert_equal('DevSec Prod Alpha', res['environment'])
    assert_equal('failed', res['status'])
    assert_equal([], res['roles'])

    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)

    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(4, res['profiles'].length)
      assert_equal([], res['profiles'][0]['controls'])
      assert_equal('myskippy', res['profiles'][0]['name'])
      assert_equal('skipped', res['profiles'][0]['status'])
      assert_equal("Skipping profile: 'myprofile' on unsupported platform: 'ubuntu/18.04'.", res['profiles'][0]['status_message'])

      assert_equal([], res['profiles'][1]['controls'])
      assert_equal('myfaily', res['profiles'][1]['name'])
      assert_equal('failed', res['profiles'][1]['status'])
      assert_equal("ERROR: cannot load such file -- inspec/resources/something (LoadError)", res['profiles'][1]['status_message'])

      assert_equal([], res['profiles'][2]['controls'])
      assert_equal('apache-baseline', res['profiles'][2]['name'])
      assert_equal('failed', res['profiles'][2]['status'])
      assert_equal("ERROR: DEPRECATION: The apache resource is deprecated This resource was removed in InSpec 4.0. (used at apache-baseline-0b5f21e668940d53e85c2ca36b41ba7ae18fac01/controls/apache_spec.rb:25)", res['profiles'][2]['status_message'])

      assert_equal(1, res['profiles'][3]['controls'].length)
      assert_equal('os-01', res['profiles'][3]['controls'][0]['id'])
      assert_equal('linux-baseline', res['profiles'][3]['name'])
      assert_equal('passed', res['profiles'][3]['status'])
      assert_equal("", res['profiles'][3]['status_message'])
    end
  end

  it "read_report retrieves a report that failed at runtime" do
    res = GRPC reporting, :read_report, Reporting::Query.new(
      id: 'zz93e1b2-36d6-439e-ac70-cccccccccckk',
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-03T00:00:00Z'])
      ]
    )
    assert_equal(Reporting::Report, res.class)

    assert_equal('4.22.0', res['version'])
    assert_equal('zz93e1b2-36d6-439e-ac70-cccccccccckk', res['id'])
    assert_equal('888f4e51-b049-4b10-9555-111222333333', res['node_id'])
    assert_equal('ubuntu(0)-alpha-failed', res['node_name'])
    assert_equal('DevSec Prod Alpha', res['environment'])
    assert_equal('failed', res['status'])
    assert_equal('ERROR: InSpec not found.', res['status_message'])
    assert_equal([], res['roles'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    assert_equal(0, res['profiles'].length)
  end
end
