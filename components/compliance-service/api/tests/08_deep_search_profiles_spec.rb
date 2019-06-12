##### GRPC SETUP #####
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  def reporting;
    Reporting::ReportingService;
  end

  it "works" do
    ##### Failure tests #####
    message = "Parameter 'sort' only supports one of the following fields: [name title]"
    assert_grpc_error(message, 3) do
      actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(sort: 'something')
    end


    END_OF_DAY = "23:59:59Z"


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
                            "total" => 1,
                            "passed" => {
                                "total" => 1
                            },
                            "skipped" => {},
                            "failed" => {},
                            "total" => 1
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
                            "total" => 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            },
                            "total" => 1
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
                            "total" => 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            },
                            "total" => 1
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
                            "total" => 1,
                            "passed" => {},
                            "skipped" => {},
                            "failed" => {
                                "total" => 1,
                                "major" => 1
                            },
                            "total" => 1
                        }
                    }
                }
            ],
            "total" => 4
        }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)


    #todo - this one is also in 08_search_profiles_spec.. it's in here because it has profile id in filter, making it deep
    # Filter by profile_id
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
        filters: [
            Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"]),
            Reporting::ListFilter.new(type: 'profile_id', values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
        ],
        page: 1, per_page: 2)
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


    # Filter by node_id and profile_id where profile ran on node
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"]),
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


    # Filter by environment and profile_id where profile ran in environment
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
    ])
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


    # Filter by platform and profile_id where profile ran on nodes on platform
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

    # Filter by platform and profile_id where profile ran on nodes on platform
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988',
                                                               '41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
    ])
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


    # Filter by platform and profile_id where profile ran on nodes on platform
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "control", values: ["nginx-04"]),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
    ])
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
end
