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

    ##### Success tests #####
    # Get all profiles
    # actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(),
    # body: Reporting::ProfileMins.new(
    #   profiles: [
    #     Reporting::ProfileMin.new(
    #       name: "apache-baseline",
    #       title: "DevSec Apache Baseline",
    #       id: "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
    #       version: "2.0.1",
    #       status: "skipped"
    #     ),
    #     Reporting::ProfileMin.new(
    #       name: "linux-baseline",
    #       title: "DevSec Linux Security Baseline",
    #       id: "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
    #       version: "2.0.1",
    #       status: "failed"
    #     ),
    #     Reporting::ProfileMin.new(
    #       name: "ssh-baseline",
    #       title: "DevSec SSH Baseline",
    #       id: "3984753145f0db693e2c6fc79f764e9aff78d892a874391fc5f5cc18f4675b68",
    #       version: "2.1.1",
    #       status: "skipped"
    #     )
    #   ],
    #   counts: Reporting::ProfileCounts.new(
    #     total: 3,
    #     skipped: 2,
    #     failed: 1
    #   )
    # )
    #
    # actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
    #   Reporting::ListFilter.new(type: 'status', values: ['failed', 'passed'])
    # ]),
    # body: Reporting::ProfileMins.new(
    #   profiles: [
    #     Reporting::ProfileMin.new(
    #       name: "linux-baseline",
    #       title: "DevSec Linux Security Baseline",
    #       id: "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
    #       version: "2.0.1",
    #       status: "failed"
    #     )
    #   ],
    #   counts: Reporting::ProfileCounts.new(
    #     total: 1,
    #     failed: 1
    #   )
    # )


    # actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
    #   Reporting::ListFilter.new(type: 'status', values: ['skipped'])
    # ]),
    # body: Reporting::ProfileMins.new(
    #   profiles: [
    #     Reporting::ProfileMin.new(
    #       name: "apache-baseline",
    #       title: "DevSec Apache Baseline",
    #       id: "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
    #       version: "2.0.1",
    #       status: "skipped"
    #     ),
    #     Reporting::ProfileMin.new(
    #       name: "ssh-baseline",
    #       title: "DevSec SSH Baseline",
    #       id: "3984753145f0db693e2c6fc79f764e9aff78d892a874391fc5f5cc18f4675b68",
    #       version: "2.1.1",
    #       status: "skipped"
    #     )
    #   ],
    #   counts: Reporting::ProfileCounts.new(
    #     total: 2,
    #     skipped: 2
    #   )
    # )

    # Get first two profiles(on page 1)
    # actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(page: 1, per_page: 2),
    # body: Reporting::ProfileMins.new(
    #   profiles: [
    #     Reporting::ProfileMin.new(
    #       name: "apache-baseline",
    #       title: "DevSec Apache Baseline",
    #       id: "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
    #       version: "2.0.1",
    #       status: "skipped"
    #     ),
    #     Reporting::ProfileMin.new(
    #       name: "linux-baseline",
    #       title: "DevSec Linux Security Baseline",
    #       id: "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
    #       version: "2.0.1",
    #       status: "failed"
    #     )
    #   ],
    #   counts: Reporting::ProfileCounts.new(
    #     total: 3,
    #     skipped: 2,
    #     failed: 1
    #   )
    # )

    END_OF_DAY = "23:59:59Z"

    # Get "two" profiles page 1
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
        filters: [
            Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
        ],
        page: 1, per_page: 2)
    expected_data = {
        "profiles" => [
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            }

        ],
        "counts" => {
            "total" => 3,
            "failed" => 1,
            "skipped" => 2
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

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
    assert_equal(expected_data, actual_data.to_json)

    # Filter by status
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
        filters: [
            Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"]),
            Reporting::ListFilter.new(type: 'status', values: ['skipped'])
        ],
        page: 1, per_page: 2)
    expected_data = {
        "profiles" => [
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            }
        ],
        "counts" => {
            "total" => 2,
            "skipped" => 2
        }
    }
    assert_equal_json_content(expected_data, actual_data)

    # Get profiles used by node with node_id
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"]),
    ])
    expected_data = {
        "profiles" => [
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
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
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)


    # Filter by job_id which exists on this date
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
    ])
    expected_data = {
        "profiles" => [
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8",
                "version" => "2.0.0",
                "status" => "passed"
            },
            {
                "name" => "linux-baseline",
                "title" => "DevSec Linux Security Baseline",
                "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
                "version" => "2.0.1",
                "status" => "failed"
            }
        ],
        "counts" => {
            "total" => 2,
            "failed" => 1,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

    # Filter by a job_id which does not exist on the date specified
    # But we ignore the date, decided by friendly debate
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-10T#{END_OF_DAY}"])
    ])
    expected_data = {
        "profiles" => [
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8",
                "version" => "2.0.0",
                "status" => "passed"
            },
            {
                "name" => "linux-baseline",
                "title" => "DevSec Linux Security Baseline",
                "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
                "version" => "2.0.1",
                "status" => "failed"
            }
        ],
        "counts" => {
            "total" => 2,
            "failed" => 1,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

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
    assert_equal(expected_data, actual_data.to_json)

    # Filter by node_id and profile_id where profile did not run on node
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['some-profile_id'])
    ])
    expected_data = {
        "profiles" => [], "counts" => {}
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

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
    assert_equal(expected_data, actual_data.to_json)

    # Filter by environment and profile_id where profile did not run in environment
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['some-profile_id']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
    ])
    expected_data = {
        "profiles" => [], "counts" => {}
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

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
    assert_equal(expected_data, actual_data.to_json)

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
    assert_equal(expected_data, actual_data.to_json)

    # Filter by platform and all profiles that ran on nodes of filtered platform
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
    ])
    expected_data = {
        "profiles" => [
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
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
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)


    # Show profiles used by nodes filtered by platform. Sorted by name desc
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])], sort: 'name', order: 1)
    expected_data = {
        "profiles" => [
            {
                "name" => "nginx-baseline",
                "title" => "DevSec Nginx Baseline",
                "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                "version" => "2.1.0",
                "status" => "passed"
            },
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            }
        ],
        "counts" => {
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)


    # Show profiles used by nodes filtered by platform. Sorted by name desc, page 1
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])],
                                                                       sort: 'name', order: 1, per_page: 1, page: 1)
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
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)


    # Show profiles used by nodes filtered by platform. Sorted by name desc, page 2
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])],
                                                                       sort: 'name', order: 1, per_page: 1, page: 2)
    expected_data = {
        "profiles" => [
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            }
        ],
        "counts" => {
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)


    # Get profiles used by nodes filtered by platform. Sorted by name desc, page out of bounds
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])],
                                                                       sort: 'name', order: 1, per_page: 1, page: 3)
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
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

    # Get profiles used by nodes filtered by multiple platforms. Sorted by name desc
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos', 'debian', 'missing']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
    ], sort: 'name', order: 1)
    expected_data = {
        "profiles" => [
            {
                "name" => "nginx-baseline",
                "title" => "DevSec Nginx Baseline",
                "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                "version" => "2.1.0",
                "status" => "passed"
            },
            {
                "name" => "fake-baseline",
                "title" => "A fake one",
                "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "version" => "2.0.1",
                "status" => "skipped"
            },
        ],
        "counts" => {
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)

    # Cover the other sort fields:
    resp = actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(sort: 'name', order: 1)
    assert_equal(Reporting::ProfileMins, resp.class)
    resp = actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(sort: 'title')
    assert_equal(Reporting::ProfileMins, resp.class)
  end
end
