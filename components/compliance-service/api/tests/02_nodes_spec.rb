##### GRPC SETUP #####
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)
  reporting = Reporting::ReportingService

  it "errors out on bad sort value" do
    message = "Parameter 'sort' only supports one of the following fields: [environment latest_report.controls.failed.critical latest_report.controls.failed.total latest_report.end_time latest_report.status name platform status]"
    assert_grpc_error(message, 3) do
      GRPC reporting, :list_nodes, Reporting::Query.new(sort: 'something')
    end
  end


  it "errors out when node is not found" do
    assert_grpc_error("Not found for id: 123", 5) do
      GRPC reporting, :read_node, Reporting::Id.new(id: '123')
    end
  end


  it "list_nodes returns all nodes sorted by node name" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']), # start_time is ignored for this call. Only end_time is used
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'name'
    )
    assert_equal([
      "centos-beta",
      "redhat(2)-alpha-nginx(f)-apache(f)-failed",
      "redhat(2)-alpha-nginx(f)-apache(s)-failed",
      "RedHat(2)-beta-nginx(f)-apache(s)-failed",
      "windows(1)-zeta-apache(s)-skipped"
    ], resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes with no filters" do
    # List nodes, no filters
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new()
    actual_nodes_hash = actual_nodes.to_h
    actual_nodes_hash[:nodes].each { |c| c[:latest_report][:end_time] = 'SOMETIME_IN_THE_LAST_24H' }
    expected_nodes = {
      "nodes": [
        {
          "environment": "DevSec Prod Alpha",
          "id": "888f4e51-b049-4b10-9555-111222333333",
          "latest_report": {
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
            "id": "zz93e1b2-36d6-439e-ac70-cccccccceemm",
            "status": "failed"
          },
          "name": "ubuntu(0)-alpha-failed",
          "platform": {
            "full": "unknown unknown",
            "name": "unknown",
            "release": "unknown"
          },
          "profiles": [

          ],
          "tags": [

          ]
        },
        {
          "environment": "DevSec Prod Omega",
          "id": "34cbbb4c-c502-4971-1111-888888888888",
          "latest_report": {
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
            "id": "44024b50-2e0d-42fa-cccc-aaaaaaaaa003",
            "status": "passed"
          },
          "name": "osx(2)-omega-pro1(f)-pro2(w)-failed",
          "platform": {
            "full": "mac_os_x 17.7.0",
            "name": "mac_os_x",
            "release": "17.7.0"
          },
          "profiles": [
            {
              "full": "My Profile 1 title, v1.0.1",
              "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
              "name": "myprofile1",
              "status": "passed",
              "version": "1.0.1"
            }
          ],
          "tags": [

          ]
        }
      ],
      "total": 2,
      "total_failed": 1,
      "total_passed": 1,
      "total_skipped": 0,
      "total_waived": 0
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes_hash.to_json)
  end


  it "list_nodes returns all nodes, sorted by latest_report(default), asc(default) order, on 2018-03-05" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full"=>"centos 5.11"
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
                        "failed" => {},
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  }
                ]
              }
        ],
        "total" => 1,
        "totalPassed" => 1
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)
  end


  it "list_nodes returns all nodes on 2018-03-04" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full" => "centos 5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"A fake one, v2.0.1"
                  }
                ]
            },
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                "platform" => {
                    "name" => "redhat",
                    "release" => "6.11",
                    "full" => "redhat 6.11"
                },
                "environment" => "DevSec Prod Alpha",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "failed",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  }
                ]
            },
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                "platform" => {
                    "name" => "redhat",
                    "release" => "6.11",
                    "full" => "redhat 6.11"
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
                        },
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  }
                ]
            },
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "platform" => {
                    "name" => "redhat",
                    "release" => "6.11",
                    "full" => "redhat 6.11"
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
                        },
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  }
                ]
            },
            {
                "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "name" => "windows(1)-zeta-apache(s)-skipped",
                "platform" => {
                    "name" => "windows",
                    "release" => "7",
                    "full" => "windows 7"
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
                        "failed" => {},
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
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
  end


  it "list_nodes on 2018-03-04 filtered by a node name" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_name', values: ['windows(1)-zeta-apache(s)-skipped']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "name" => "windows(1)-zeta-apache(s)-skipped",
                "platform" => {
                    "name" => "windows",
                    "release" => "7",
                    "full" => "windows 7"
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
                        "failed" => {},
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full" => "DevSec Apache Baseline, v2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalSkipped" => 1
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)
  end


  it "list_nodes on 2018-03-04 with per_page limit" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      per_page: 1,
      order: 1,
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "name" => "windows(1)-zeta-apache(s)-skipped",
                "platform" => {
                    "name" => "windows",
                    "release" => "7",
                    "full" => "windows 7"
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
                        "failed" => {},
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
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
  end


  it "list_nodes on 2018-03-04 with per_page limit, page number and order" do
    # Test pagination(per_page+page+order)
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      per_page: 1,
      page: 3,
      order: 1,
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                "platform" => {
                    "name" => "redhat",
                    "release" => "6.11",
                    "full" => "redhat 6.11"
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
                        },
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "failed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
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
  end


  it "list_nodes on 2018-03-04 with env filter" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "name" => "windows(1)-zeta-apache(s)-skipped",
                "platform" => {
                    "name" => "windows",
                    "release" => "7",
                    "full" => "windows 7"
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
                        "failed" => {},
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalSkipped" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes on 2018-03-04 with inspec_version filter" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'inspec_version', values: ['3.1.0']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full" => "centos 5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"A fake one, v2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes on 2018-03-04 with inspec_version filter, take 2" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'inspec_version', values: ['3.1.3']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    assert_equal(actual_nodes.total, 4)
  end


  it "list_nodes on 2018-03-04 with env and passed status filters" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod beta']),
        Reporting::ListFilter.new(type: 'status', values: ['passed']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full" => "centos 5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"A fake one, v2.0.1"
                  }
                ]
            }
        ],
        "total" => 2,
        "totalFailed" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes on 2018-03-04 filtered by env and skipped status" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod beta']),
        Reporting::ListFilter.new(type: 'status', values: ['skipped']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
      "total" => 2,
      "totalFailed" => 1,
      "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes on 2018-03-04 filtered by recipe and passed status" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'recipe', values: ['apache_extras']),
        Reporting::ListFilter.new(type: 'status', values: ['passed']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full" => "centos 5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"A fake one, v2.0.1"
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
  end


  it "list_nodes filtered by job_id and non matching end-date for it" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['74a54a28-c628-4f82-86df-333333333333']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-22T23:59:59Z'])
      ]
    )
    expected_nodes = {}.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes filtered by job_id and matching end-date for it" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['74a54a28-c628-4f82-86df-333333333333']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full" => "centos 5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"A fake one, v2.0.1"
                  }
                ]
            }
        ],
        "total" => 1,
        "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes with non matching filters" do
    actual = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'status', values: ['skipped', 'failed']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ]
    )
    # expect 0 nodes, but expect counts to not be 0
    expected_nodes = {
      "total" => 1,
      "totalPassed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual.to_json)
  end


  it "list_nodes with platform centos, windows or aix" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos', 'windows', 'aix']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes = {
        "nodes" => [
            {
                "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                "name" => "centos-beta",
                "platform" => {
                    "name" => "centos",
                    "release" => "5.11",
                    "full" => "centos 5.11"
                },
                "environment" => "DevSec Prod beta",
                "latestReport" => {
                    "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                "profiles" => [
                  {
                    "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                    "name" => "nginx-baseline",
                    "status" => "passed",
                    "version" => "2.1.0",
                    "full"=>"DevSec Nginx Baseline, v2.1.0"
                  },
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  },
                  {
                    "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "fake-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"A fake one, v2.0.1"
                  }
                ]
            },
            {
                "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                "name" => "windows(1)-zeta-apache(s)-skipped",
                "platform" => {
                    "name" => "windows",
                    "release" => "7",
                    "full" => "windows 7"
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
                        "failed" => {},
                        "waived" => {}
                    }
                },
                "profiles" => [
                  {
                    "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                    "name" => "apache-baseline",
                    "status" => "skipped",
                    "version" => "2.0.1",
                    "full"=>"DevSec Apache Baseline, v2.0.1"
                  }
                ]
            }
        ],
        "total" => 2,
        "totalPassed" => 1,
        "totalSkipped" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes with environment filter and special characters" do
    # Target missing environment that contains special characters
    actual = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['Dev&Test Trouble+Maker:Env'])
      ]
    )
    assert_equal(Reporting::Nodes.new(), actual)
  end


  it "list_nodes with non-existent profile_id filter" do
    actual = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['non-existent'])
      ]
    )
    assert_equal(Reporting::Nodes.new(), actual)
  end


  it "list_nodes sorted by env, desc order" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      sort: 'environment',
      order: 1,
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes =
        {
            "nodes" => [
                {
                    "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "name" => "windows(1)-zeta-apache(s)-skipped",
                    "platform" => {
                        "name" => "windows",
                        "release" => "7",
                        "full" => "windows 7"
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
                            "failed" => {},
                            "waived" => {}
                        }
                    },
                    "profiles" => [
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "name" => "centos-beta",
                    "platform" => {
                        "name" => "centos",
                        "release" => "5.11",
                        "full" => "centos 5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "passed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      },
                      {
                        "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "fake-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"A fake one, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11",
                        "full" => "redhat 6.11"
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
                            },
                            "waived" => {}
                        }
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11",
                        "full" => "redhat 6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
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
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "failed",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11",
                        "full" => "redhat 6.11"
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
                            },
                            "waived" => {}
                        }
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
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
  end


  it "list_nodes sorted by name, asc order" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      sort: 'name',
      order: 0,
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ]
    )
    expected_nodes =
        {
            "nodes" => [
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
                    "name" => "centos-beta",
                    "platform" => {
                        "name" => "centos",
                        "release" => "5.11",
                        "full" => "centos 5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
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
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "passed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      },
                      {
                        "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "fake-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"A fake one, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e112",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(f)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11",
                        "full" => "redhat 6.11"
                    },
                    "environment" => "DevSec Prod Alpha",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc10",
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
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "failed",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e111",
                    "name" => "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11",
                        "full" => "redhat 6.11"
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
                            },
                            "waived" => {}
                        }
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "9b9f4e51-b049-4b10-9555-10578916e222",
                    "name" => "RedHat(2)-beta-nginx(f)-apache(s)-failed",
                    "platform" => {
                        "name" => "redhat",
                        "release" => "6.11",
                        "full" => "redhat 6.11"
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
                            },
                            "waived" => {}
                        }
                    },
                    "profiles" => [
                      {
                        "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                        "name" => "nginx-baseline",
                        "status" => "failed",
                        "version" => "2.1.0",
                        "full"=>"DevSec Nginx Baseline, v2.1.0"
                      },
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                },
                {
                    "id" => "a0ddd774-cbbb-49be-8730-49c92f3fc2a0",
                    "name" => "windows(1)-zeta-apache(s)-skipped",
                    "platform" => {
                        "name" => "windows",
                        "release" => "7",
                        "full" => "windows 7"
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
                            "failed" => {},
                            "waived" => {}
                        }
                    },
                    "profiles" => [
                      {
                        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                        "name" => "apache-baseline",
                        "status" => "skipped",
                        "version" => "2.0.1",
                        "full"=>"DevSec Apache Baseline, v2.0.1"
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
  end


  it "list_nodes scanned on from 2018-04-01" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T23:59:59Z'])
      ]
    )
    expected_nodes =
        {
          "nodes"=>
            [
              {
                "environment"=>"DevSec Prod Omega",
                "id"=>"34cbbb4c-c502-4971-1111-888888888888",
                "latestReport"=>{
                  "controls"=>{
                    "failed"=>{"critical"=>2, "total"=>2},
                    "passed"=>{},
                    "skipped"=>{},
                    "total"=>6,
                    "waived"=>{"total"=>4}
                  },
                  "endTime"=>"2018-04-01T10:18:41Z",
                  "id"=>"44024b50-2e0d-42fa-cccc-yyyyyyyyyyyy",
                  "status"=>"failed"
                },
                "name"=>"osx(2)-omega-pro1(f)-pro2(w)-failed",
                "platform"=>{"full"=>"mac_os_x 17.7.0", "name"=>"mac_os_x", "release"=>"17.7.0"},
                "profiles"=>[
                  {
                    "full"=>"My Profile 1 title, v1.0.1",
                    "id"=>"447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
                    "name"=>"myprofile1",
                    "status"=>"failed",
                    "version"=>"1.0.1"
                  },
                  {
                    "full"=>"My Profile 2 title, v1.0.5",
                    "id"=>"447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5",
                    "name"=>"myprofile2",
                    "status"=>"waived",
                    "version"=>"1.0.5"
                  }
                ]
              },
              {
                "environment"=>"DevSec Prod Omega",
                "id"=>"34cbbb55-c502-4971-2222-999999999999",
                "latestReport"=>{
                  "controls"=>{
                    "failed"=>{},
                    "passed"=>{},
                    "skipped"=>{},
                    "total"=>1,
                    "waived"=>{"total"=>1}
                  },
                  "endTime"=>"2018-04-01T10:18:41Z",
                  "id"=>"44024b50-2e0d-42fa-dddd-wwwwwwwwwwww",
                  "status"=>"waived"
                },
                "name"=>"osx(1)-omega-pro2(w)-waived",
                "platform"=>{"full"=>"mac_os_x 17.7.0", "name"=>"mac_os_x", "release"=>"17.7.0"},
                "profiles"=>[
                  {
                    "full"=>"My Profile 2 title, v1.0.5",
                    "id"=>"447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5",
                    "name"=>"myprofile2",
                    "status"=>"waived",
                    "version"=>"1.0.5"
                  }
                ]
              }
            ],
            "total"=>2,
            "totalFailed"=>1,
            "totalWaived"=>1,
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes scanned on from 2018-04-02 with two profile filters" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          '5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777',
          '5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
      ]
    )
    expected_nodes =
    {"nodes" =>
      [{"environment" => "DevSec Prod Alpha",
      "id" => "999f4e51-b049-4b10-9555-555789999967",
      "latestReport" =>
       {"controls" =>
         {"failed" => {},
          "passed" => {"total" => 1},
          "skipped" => {},
          "total" => 1,
          "waived" => {}},
        "endTime" => "2018-04-02T03:02:02Z",
        "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
        "status" => "failed"},
      "name" => "ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed",
      "platform" => {"full" => "ubuntu 18.04", "name" => "ubuntu", "release" => "18.04"},
      "profiles" =>
       [{"full" => "DevSec Apache Baseline, v2.0.0",
         "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8",
         "name" => "apache-baseline",
         "status" => "failed",
         "version" => "2.0.0"},
        {"full" => "DevSec Linux Security Baseline, v2.0.1",
         "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
         "name" => "linux-baseline",
         "status" => "passed",
         "version" => "2.0.1"},
        {"full" => "My Faily Profile title, v1.0.0",
         "id" => "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888",
         "name" => "myfaily",
         "status" => "failed",
         "version" => "1.0.0"},
        {"full" => "My Skipped Profile title, v1.0.0",
         "id" => "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777",
         "name" => "myskippy",
         "status" => "skipped",
         "version" => "1.0.0"}]}],
      "total" => 1,
      "totalFailed" => 1}.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes scanned on 2018-04-03 with non matching profile" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          '5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-03T23:59:59Z'])
      ]
    )
    expected_nodes = {}.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes scanned on 2018-04-03" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-03T23:59:59Z'])
      ]
    )
    expected_nodes = {
      "nodes" => [
        {
          "environment" => "DevSec Prod Alpha",
          "id" => "888f4e51-b049-4b10-9555-111222333333",
          "latestReport" => {
            "controls" => { "failed" => {}, "passed" => {}, "skipped" => {}, "waived" => {}},
            "endTime" => "2018-04-03T11:02:02Z",
            "id" => "zz93e1b2-36d6-439e-ac70-cccccccccckk",
            "status" => "failed"
          },
          "name" => "ubuntu(0)-alpha-failed",
          "platform" => { "full" => "unknown unknown", "name" => "unknown", "release" => "unknown"}
        }
      ],
      "total" => 1,
      "totalFailed" => 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_nodes without end_time filter (last 24h search)" do
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new()
    actual_nodes_hash = actual_nodes.to_h
    actual_nodes_hash[:nodes].each { |c| c[:latest_report][:end_time] = 'SOMETIME_IN_THE_LAST_24H' }
    expected_nodes = {
      "nodes": [
        {
          "environment": "DevSec Prod Alpha",
          "id": "888f4e51-b049-4b10-9555-111222333333",
          "latest_report": {
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
            "id": "zz93e1b2-36d6-439e-ac70-cccccccceemm",
            "status": "failed"
          },
          "name": "ubuntu(0)-alpha-failed",
          "platform": {
            "full": "unknown unknown",
            "name": "unknown",
            "release": "unknown"
          },
          "profiles": [],
          "tags": []
        },
        {
          "environment": "DevSec Prod Omega",
          "id": "34cbbb4c-c502-4971-1111-888888888888",
          "latest_report": {
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
            "id": "44024b50-2e0d-42fa-cccc-aaaaaaaaa003",
            "status": "passed"
          },
          "name": "osx(2)-omega-pro1(f)-pro2(w)-failed",
          "platform": {
            "full": "mac_os_x 17.7.0",
            "name": "mac_os_x",
            "release": "17.7.0"
          },
          "profiles": [
            {
              "full": "My Profile 1 title, v1.0.1",
              "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
              "name": "myprofile1",
              "status": "passed",
              "version": "1.0.1"
            }
          ],
          "tags": []
        }
      ],
      "total": 2,
      "total_failed": 1,
      "total_passed": 1,
      "total_skipped": 0,
      "total_waived": 0
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes_hash.to_json)
  end


  it "list_nodes with sorting by platform desc" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'platform',
      order: 1
    )
    assert_equal(Reporting::Nodes, resp.class)
  end


  it "list_nodes with sorting by latest_report.status" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'latest_report.status'
    )
    assert_equal(Reporting::Nodes, resp.class)
  end


  it "list_nodes with sorting by latest_report.end_time" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'latest_report.end_time'
    )
    assert_equal(Reporting::Nodes, resp.class)
  end


  it "list_nodes with sorting by latest_report.controls.failed.total" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'latest_report.controls.failed.total'
    )
    assert_equal(Reporting::Nodes, resp.class)
  end


  it "list_nodes with sorting by latest_report.controls.failed.critical" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'latest_report.controls.failed.critical'
    )
    assert_equal(Reporting::Nodes, resp.class)
  end


  it "read_node by id scanned on 2018-03-05" do
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '9b9f4e51-b049-4b10-9555-10578916e149')
    expected_node = {
        "id" => "9b9f4e51-b049-4b10-9555-10578916e149",
        "name" => "centos-beta",
        "platform" => {
            "name" => "centos",
            "release" => "5.11",
            "full" => "centos 5.11"
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
                },
                "waived" => {}
            }
        },
        "profiles" => [
            {
                "name" => "nginx-baseline",
                "version" => "2.1.0",
                "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                "status": "failed",
                "full"=>"DevSec Nginx Baseline, v2.1.0"
            },
            {
                "name" => "apache-baseline",
                "version" => "2.0.1",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "status": "failed",
                "full"=>"DevSec Apache Baseline, v2.0.1"
            }
        ]
    }
    assert_equal_json_sorted( expected_node.to_json, actual_node.to_json)
  end


  it "read_node by id without end_time, hence 24h stuff" do
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '34cbbb4c-c502-4971-1111-888888888888')
    actual_node_hash = actual_node.to_h
    actual_node_hash[:latest_report][:end_time] = 'SOMETIME_IN_THE_LAST_24H'

    expected_node = {
      "environment": "DevSec Prod Omega",
      "id": "34cbbb4c-c502-4971-1111-888888888888",
      "latest_report": {
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
        "id": "44024b50-2e0d-42fa-cccc-aaaaaaaaa003",
        "status": "passed"
      },
      "name": "osx(2)-omega-pro1(f)-pro2(w)-failed",
      "platform": {
        "full": "mac_os_x 17.7.0",
        "name": "mac_os_x",
        "release": "17.7.0"
      },
      "profiles": [
        {
          "full": "My Profile 1 title, v1.0.1",
          "id": "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
          "name": "myprofile1",
          "status": "passed",
          "version": "1.0.1"
        }
      ],
      "tags": [

      ]
    }
    assert_equal_json_sorted( expected_node.to_json, actual_node_hash.to_json)
  end


  it "read_node by second id without end_time, hence 24h stuff" do
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '888f4e51-b049-4b10-9555-111222333333')
    actual_node_hash = actual_node.to_h
    actual_node_hash[:latest_report][:end_time] = 'SOMETIME_IN_THE_LAST_24H'
    expected_node = {
      "environment": "DevSec Prod Alpha",
      "id": "888f4e51-b049-4b10-9555-111222333333",
      "latest_report": {
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
        "id": "zz93e1b2-36d6-439e-ac70-cccccccceemm",
        "status": "failed"
      },
      "name": "ubuntu(0)-alpha-failed",
      "platform": {
        "full": "unknown unknown",
        "name": "unknown",
        "release": "unknown"
      },
      "profiles": [],
      "tags": []
    }
    assert_equal_json_sorted( expected_node.to_json, actual_node_hash.to_json)
  end


  it "read_node by id with end_time on 2018-04-02" do
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '999f4e51-b049-4b10-9555-555789999967')
    expected_node = {
     "environment" => "DevSec Prod Alpha",
     "id" => "999f4e51-b049-4b10-9555-555789999967",
     "latestReport" =>
      {"controls" =>
        {"failed" => {},
         "passed" => {"total" => 1},
         "skipped" => {},
         "total" => 1,
         "waived" => {}},
       "endTime" => "2018-04-02T03:02:02Z",
       "id" => "bb93e1b2-36d6-439e-ac70-ccccccczzz20",
       "status" => "failed"},
     "name" => "ubuntu(4)-alpha-myskippy(s)-myfaily(f)-apache(f)-linux(p)-failed",
     "platform" => {"full" => "ubuntu 18.04", "name" => "ubuntu", "release" => "18.04"},
     "profiles" =>
      [{"full" => "DevSec Apache Baseline, v2.0.0",
        "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8",
        "name" => "apache-baseline",
        "status" => "failed",
        "version" => "2.0.0"},
       {"full" => "DevSec Linux Security Baseline, v2.0.1",
        "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
        "name" => "linux-baseline",
        "status" => "passed",
        "version" => "2.0.1"},
       {"full" => "My Faily Profile title, v1.0.0",
        "id" => "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888",
        "name" => "myfaily",
        "status" => "failed",
        "version" => "1.0.0"},
       {"full" => "My Skipped Profile title, v1.0.0",
        "id" => "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777",
        "name" => "myskippy",
        "status" => "skipped",
        "version" => "1.0.0"}]
    }
    assert_equal_json_sorted( expected_node.to_json, actual_node.to_json)
  end


  it "list_nodes filtered by profile_ids with end_time on 2018-04-02 and pagination" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(
          type: 'profile_id', values: [
            'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015',
            '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988'
          ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'name',
      order: 0,
      page: 1,
      per_page: 2
    )
    expected = [
        "centos-beta",
        "redhat(2)-alpha-nginx(f)-apache(f)-failed"
    ]
    assert_equal(expected, resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes filtered by profile_ids with end_time on 2018-04-02 and name sort desc" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015',
          '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'name',
      order: 1,
      page: 1,
      per_page: 2
    )
    expected = [
      "RedHat(2)-beta-nginx(f)-apache(s)-failed",
      "redhat(2)-alpha-nginx(f)-apache(s)-failed"
    ]
    assert_equal(expected, resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes filtered by profile_ids with end_time on 2018-03-04 and platform sort desc" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015', '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'platform',
      order: 1,
      page: 2,
      per_page: 2
    )
    assert_equal(["redhat", "centos"], resp['nodes'].map{ |x| x['platform']['name'] })
  end


  it "list_nodes filtered by profile_ids with end_time on 2018-03-04 and environment sort asc" do
    # sort by node environment now
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988', '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'environment',
      order: 0,
      page: 1,
      per_page: 3
    )
    assert_equal(["DevSec Prod Alpha", "DevSec Prod Alpha", "DevSec Prod beta"], resp['nodes'].map {|x| x['environment']})
  end


  it "list_nodes filtered by profile_ids with end_time on 2018-03-04 and environment sort desc" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988', '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'environment',
      order: 1,
      page: 1,
      per_page: 3
    )
    assert_equal(["DevSec Prod Zeta", "DevSec Prod beta", "DevSec Prod beta"], resp['nodes'].map {|x| x['environment']})
  end

    # sort by node failed controls now
    #todo - RDM this needs fixing!!
    # resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
    #     Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015']),
    #     Reporting::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
    # ], sort: 'latest_report.controls.failed.total', order: 0, page: 1, per_page: 2)
    # assert_equal([22], resp['nodes'].map {|x| x['latest_report']['controls']['failed']['total']})


  it "list_nodes when filtering by profile. page is not used when filtering by profile due to aggregations" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: [
          '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9',
          'yyy'
        ]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ],
      sort: 'environment',
      order: 0,
      page: 1,
      per_page: 2
    )
    assert_equal([14, 15], resp['nodes'].map {|x| x['latest_report']['controls']['skipped']['total']})
  end


  it "list_nodes that have a control with a tag key of 'web'" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:web', values: [])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'RedHat(2)-beta-nginx(f)-apache(s)-failed', 'windows(1)-zeta-apache(s)-skipped'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control with a tag key of 'scope'" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: [])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'RedHat(2)-beta-nginx(f)-apache(s)-failed', 'windows(1)-zeta-apache(s)-skipped'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control tagged either scope:Apache or scope:missing, case insensitive" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: ['Apache', 'missing'])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'windows(1)-zeta-apache(s)-skipped'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control tagged either SCOpe:Nginx or SCOpe:missing, case insensitive" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:SCOpe', values: ['Nginx', 'missing'])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'RedHat(2)-beta-nginx(f)-apache(s)-failed'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control tagged 'web' with no values" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:web', values: [''])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'RedHat(2)-beta-nginx(f)-apache(s)-failed'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control tagged 'web' with no values or 'web:IIS'" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:web', values: ['', 'IIS'])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'RedHat(2)-beta-nginx(f)-apache(s)-failed', 'windows(1)-zeta-apache(s)-skipped'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control tagged 'web' with no values or scope wildcard" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:web', values: []),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: ['Ngi*'])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'RedHat(2)-beta-nginx(f)-apache(s)-failed'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes that have a control tagged 'scope' with wildcard value" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: ['ap?che'])
      ],
      sort: 'name'
    )
    assert_equal(['centos-beta', 'windows(1)-zeta-apache(s)-skipped'],
                 resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes returns no data for missing tag values" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:scope', values: ['missing1', 'missing2'])
      ],
      sort: 'name'
    )
    assert_equal([],resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes returns the right node for a end_time and control_tag filter" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:satisfies', values: ['Apache-1', 'zzzz'])
      ],
      sort: 'name'
    )
    assert_equal(['windows(1)-zeta-apache(s)-skipped'],resp['nodes'].map {|x| x['name']})
  end


  it "list_nodes returns no nodes for missing tag values" do
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'control_tag:missing1', values: ['missing2', 'missing3'])
      ],
      sort: 'name'
    )
    assert_equal([],resp['nodes'].map {|x| x['name']})
  end
end
