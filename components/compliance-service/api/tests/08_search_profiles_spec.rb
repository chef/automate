##### GRPC SETUP #####
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)
  def reporting;
    Reporting::ReportingService;
  end
  END_OF_DAY = "23:59:59Z"

  it "fails on a bad sort value" do
    message = "Parameter 'sort' only supports one of the following fields: [name title]"
    assert_grpc_error(message, 3) do
      actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(sort: 'something')
    end
  end


  it "list_profiles two per page" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      page: 1,
      per_page: 2
    )
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
  end


  it "list_profiles two per page, filtered by profile_id" do
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
  end


  it "list_profiles two per page, filtered by status" do
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
            "total" => 3,
            "failed" => 1,
            "skipped" => 2
        }
    }
    assert_equal_json_content(expected_data, actual_data)
  end


  it "list_profiles used by node_id" do
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
  end


  it "list_profiles for reports where control tags scope:apache or scope:nginx were used" do
    # Get profiles used
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'control_tag:scope', values: ['apache', 'nginx']),
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
                "status" => "failed"
            }
        ],
        "counts" => {
            "total" => 3,
            "failed" => 1,
            "skipped" => 2
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles for job_id which exists on this date" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
      ]
    )
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
  end

  it "list_profiles by a job_id which does exists on the date specified" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
      ]
    )
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
  end

  it "list_profiles by a job_id which does NOT exist on the date specified" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'job_id', values: ['12345678-1234-123e-b12e-222222222222']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-10T#{END_OF_DAY}"])
      ]
    )
    expected_data = { "counts":{} }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles by node_id and profile_id where profile ran on node" do
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
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles for missing profile id" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['some-profile_id'])
      ]
    )
    expected_data = { "counts" => {} }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles by environment and profile_id where profile ran in environment" do
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
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles by environment and profile_id where profile did not run in environment" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod Zeta']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['some-profile_id']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
      ]
    )
    expected_data = { "counts" => {} }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles by platform and profile_id where profile ran on nodes on platform" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values: ['09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
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
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles by platform and profile_id where profile ran on nodes on platform" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: 'profile_id', values: [
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
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles by platform and all profiles that ran on nodes of filtered platform" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ]
    )
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
  end


  it "list_profiles used by nodes filtered by platform. Sorted by name desc" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1
    )
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
  end


  it "list_profiles used by nodes filtered by platform. Sorted by name desc, page 1" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1,
      per_page: 1,
      page: 1
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
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles used by nodes filtered by platform. Sorted by name desc, page 2" do
    # Show profiles
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1,
      per_page: 1,
      page: 2
    )
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
  end


  it "list_profiles used by nodes filtered by platform. Sorted by name desc, page out of bounds" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1,
      per_page: 1,
      page: 3
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
            "total" => 3,
            "skipped" => 2,
            "passed" => 1
        }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles used by nodes filtered by multiple platforms. Sorted by name desc" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: 'platform', values: ['centos', 'debian', 'missing']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1
    )
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
  end


  it "list_profiles used by nodes on 2018-04-01. Sorted by name desc" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2018-04-01T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1
    )
    expected_data = {
      "profiles" => [
        {
          "name" => "myprofile2",
          "title" => "My Profile 2 title",
          "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5",
          "version" => "1.0.5",
          "status" => "waived"
        },
        {
          "name" => "myprofile1",
          "title" => "My Profile 1 title",
          "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
          "version" => "1.0.1",
          "status" => "failed"
        }
      ],
      "counts" => {
        "total" => 2,
        "failed" => 1,
        "waived" => 1
      }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles used by nodes on 2018-04-02. Sorted by name desc" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2018-04-02T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1
    )
    expected_data = {
      "profiles": [
        {
          "name": "myskippy",
          "title": "My Skipped Profile title",
          "id": "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777",
          "version": "1.0.0",
          "status": "skipped"
        },
        {
          "name": "myfaily",
          "title": "My Faily Profile title",
          "id": "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888",
          "version": "1.0.0",
          "status": "failed"
        },
        {
          "name": "linux-baseline",
          "title": "DevSec Linux Security Baseline",
          "id": "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
          "version": "2.0.1",
          "status": "passed"
        },
        {
          "name": "apache-baseline",
          "title": "DevSec Apache Baseline",
          "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8",
          "version": "2.0.0",
          "status": "failed"
        }
      ],
      "counts": {
        "total": 4,
        "failed": 2,
        "skipped": 1,
        "passed": 1
      }
    }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles used by nodes on the day without profiles. Sorted by name desc" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2018-04-03T#{END_OF_DAY}"])
      ],
      sort: 'name',
      order: 1
    )
    expected_data = { "counts": {} }.to_json
    assert_equal(expected_data, actual_data.to_json)
  end


  it "list_profiles with title sorting" do
    resp = actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(sort: 'title')
    assert_equal(Reporting::ProfileMins, resp.class)
  end


  it "list_profiles no end_time filter, last 24h stuff" do
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new()
    expected_data = {
      "counts"=>{"passed"=>1, "total"=>1},
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
