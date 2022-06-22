##### GRPC SETUP #####
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)
  def reporting;
    Reporting::ReportingService;
  end
  END_OF_DAY = "23:59:59Z"

  it "errors with wrong sort param" do
    message = "Parameter 'sort' only supports one of the following fields: [name title]"
    assert_grpc_error(message, 3) do
      actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(sort: 'something')
    end
  end


  it "list_nodes deep with profile and control filter" do
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
                        "release" => "5.11",
                        "full" => "centos 5.11"
                    },
                    "environment" => "DevSec Prod beta",
                    "latestReport" => {
                        "id" => "bb93e1b2-36d6-439e-ac70-cccccccccc04",
                        "endTime" => "2018-03-04T00:00:10Z",
                        "status" => "passed",
                        "controls" => {
                            "total" => 1,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {},
                            "failed" => {},
                            "waived" => {},
                            "total" => 1
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
                        "full" => "DevSec Apache Baseline, v2.0.1"
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
                            "total" => 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            },
                            "waived" => {},
                            "total" => 1
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
                        "full" => "DevSec Apache Baseline, v2.0.1"
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
                            "total" => 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            },
                            "waived" => {},
                            "total" => 1
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
                        "full" => "DevSec Apache Baseline, v2.0.1"
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
                            "total" => 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            },
                            "waived" => {},
                            "total" => 1
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
                        "full" => "DevSec Apache Baseline, v2.0.1"
                      }
                    ]
                }
            ],
            "total" => 4,
            "totalFailed"=>3,
            "totalPassed"=>1
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end


  it "list_profiles deep with profile filter on 2018-03-04" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"]),
        Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
      ],
      page: 1,
      per_page: 2
    )
    expected_data = {
        "profiles" => [
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            }
        ],
        "counts" => {
            "total" => 1,
            "skipped" => 1
        }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep with profile and node filter" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"]),
      ]
    )
    expected_data = {
      "profiles" => [
        {
          "name" => "nginx-baseline",
          "title" => "DevSec Nginx Baseline",
          "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
          "version" => "2.1.0",
          "status" => "passed"
        }
      ],
      "counts" => {
        "total" => 1,
        "passed" => 1
      }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep with profile and environment filter" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
      ]
    )
    expected_data = {
        "profiles" => [
            {
                "name" => "linux-baseline",
                "title" => "DevSec Linux Security Baseline",
                "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
                "version" => "2.0.1",
                "status" => "failed"
            }
        ],
        "counts" => {
            "total" => 1,
            "failed" => 1
        }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by platform and profile_id where profile ran on nodes on platform" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
    ])
    expected_data = {
        "profiles" => [
            {
                "name" => "nginx-baseline",
                "title" => "DevSec Nginx Baseline",
                "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                "version" => "2.1.0",
                "status" => "passed"
            }
        ],
        "counts" => {
            "total" => 1,
            "passed" => 1
        }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by platform and multiple profile_ids" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values:[
          '09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988',
          '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'
        ]),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ]
    )
    expected_data = {
        "profiles" => [
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
            {
                "name" => "nginx-baseline",
                "title" => "DevSec Nginx Baseline",
                "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                "version" => "2.1.0",
                "status" => "passed"
            }
        ],
        "counts" => {
            "total" => 2,
            "skipped" => 1,
            "passed" => 1
        }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by platform and profile_id and control filters" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "control", values: ["nginx-04"]),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ]
    )
    expected_data = {
        "profiles" =>
            [
                {"name" => "nginx-baseline",
                 "title" => "DevSec Nginx Baseline",
                 "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                 "version" => "2.1.0",
                 "status" => "passed"}
            ],
        "counts" => {
            "total" => 1,
            "passed" => 1
        }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by environment and profile_id" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Omega']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T23:59:59Z'])
      ],
      page: 1,
      per_page: 2
    )
    expected_data = {
      "counts" => { "total" => 1, "waived" => 1 },
      "profiles" => [
          {
            "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5",
            "name" => "myprofile2",
            "status" => "waived",
            "title" => "My Profile 2 title",
            "version" => "1.0.5"
          }
        ]
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by profile_id and waived control id" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4']),
        Reporting::ListFilter.new(type: "control", values: ["pro1-con2"]),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-01T23:59:59Z'])
      ],
      page: 1,
      per_page: 2
    )
    expected_data = {
      "counts" => { "total" => 1, "waived" => 1 },
      "profiles" => [
        {
          "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
          "name" => "myprofile1",
          "status" => "waived",
          "title" => "My Profile 1 title",
          "version" => "1.0.1"
        }
      ]
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by failed profile_id" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
      ],
      page: 1,
      per_page: 2
    )
    expected_data = {
      "counts" => {"failed" => 1, "total" => 1},
      "profiles" =>
        [{"id" => "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888",
          "name" => "myfaily",
          "status" => "failed",
          "title" => "My Faily Profile title",
          "version" => "1.0.0"}]
      }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by profile_id for a date containing a non profiles report" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-03T23:59:59Z'])
      ],
      page: 1,
      per_page: 2
    )
    expected_data = { "counts" => {} }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end


  it "list_profiles deep by profile_id with no end_time filter, last 24h stuff" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4'])
      ]
    )
    expected_data = {
      "counts"=>{
        "passed"=>1,
        "total"=>1
      },
      "profiles"=>[
        {
          "id"=>"447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
          "name"=>"myprofile1",
          "status"=>"passed",
          "title"=>"My Profile 1 title",
          "version"=>"1.0.1"
        }
      ]
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end
end
