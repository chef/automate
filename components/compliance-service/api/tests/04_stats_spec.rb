require 'api/stats/stats_pb'
require 'api/stats/stats_services_pb'

if !ENV['NO_STATS_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Api::Stats unless defined?(Stats)

    def stats
      Stats::StatsService;
    end

    it "works" do
      ##### Failure tests #####
      assert_grpc_error("A filter of value 'types' must be specified. Valid values are 'platform', 'environment', 'control', and 'profile'", 3) do
        GRPC stats, :read_failures, Stats::Query.new
      end

      assert_grpc_error("Invalid type 'mrbobby'", 3) do
        GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["mrbobby"])
        ])
      end

      ##### Success tests #####
      # Failures by platform
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["platform"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])

      expected_data = {
        "platforms" => [
          {
            "name" => "debian",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Failures by environment
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "environments" => [
          {
            "name" => "DevSec Prod Zeta",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment"]),
        Stats::ListFilter.new(type: "node_name", values: ["debian(2)-zeta-linux(f)-apache(p)-failed"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "environments" => [
          {
            "name" => "DevSec Prod Zeta",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment"]),
        Stats::ListFilter.new(type: "status", values: ["failed"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "environments" => [
          {
            "name" => "DevSec Prod Zeta",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Get failures by platform, top 1
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["platform"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ],
                                                                 size: 1)
      expected_data = {
        "platforms" => [
          {
            "name" => "debian",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Get both failures by environment and platform
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment", "platform"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "platforms" => [
          {
            "name" => "debian",
            "failures" => 1
          }
        ],
        "environments" => [
          {
            "name" => "DevSec Prod Zeta",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)

      # Get both failures by environment and platform with control tag filter
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment", "platform"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Stats::ListFilter.new(type: 'control_tag:web', values: [])
      ])
      expected_data = {
        "platforms" => [
          {
            "name" => "redhat",
            "failures" => 1
          }
        ],
        "environments" => [
          {
            "name"=>"DevSec Prod beta",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)

      # todo - this one is in the deep test: 04_deep_stats_spec.rb::test3::line:57
      # Get failures by profile
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["profile"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ],
                                                                 size: 1)
      expected_data = {
        "profiles" => [
          {
            "name" => "linux-baseline",
            "failures" => 1,
            "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)



      # todo - this one is in the deep test: 04_deep_stats_spec.rb::test1::line78
      # Use control type
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["control"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "controls" => [
          {
            "name" => "os-02",
            "failures" => 1
          },
          {
            "name" => "os-05",
            "failures" => 1
          },
          {
            "name" => "sysctl-01",
            "failures" => 1
          },
          {
            "name" => "sysctl-05",
            "failures" => 1
          },
          {
            "name" => "sysctl-06",
            "failures" => 1
          },
          {
            "name" => "sysctl-07",
            "failures" => 1
          },
          {
            "name" => "sysctl-08",
            "failures" => 1
          },
          {
            "name" => "sysctl-09",
            "failures" => 1
          },
          {
            "name" => "sysctl-10",
            "failures" => 1
          },
          {
            "name" => "sysctl-15",
            "failures" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)

      # Use control type. There are no failures on 03-04
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["control"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "controls" =>
          [
            {"name" => "nginx-04", "failures" => 3},
            {"name" => "nginx-01", "failures" => 2},
            {"name" => "apache-02", "failures" => 1}
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Use environment filter with missing env
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment"]),
        Stats::ListFilter.new(type: "environment", values: ["missing In action"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)

      # todo - this one is in the deep test: 04_deep_stats_spec.rb::test1::line36
      # multiple filters, one match
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["platform"]),
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
          Stats::ListFilter.new(type: "node_id", values: ["34cbbb4c-c502-4971-b193-00e987b4678c"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
          "platforms" => [
              {
                  "name" => "debian",
                  "failures" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Target missing environment that contains special characters
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["platform"]),
          Stats::ListFilter.new(type: "environment", values: ["Dev&Test Trouble+Maker:Env"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)


      # Use start_time in the future and expect no matches  !!!!!! profile
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["platform"]),
          Stats::ListFilter.new(type: "start_time", values: ["3000-03-03T03:03:03Z"])
      ])
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)


      ##read profiles
      actual_data = GRPC stats, :read_profiles, Stats::Query.new()
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_profiles, Stats::Query.new(size: 2)
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)


      # Use environment filter
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profileList" => [
              {
                  "name" => "apache-baseline",
                  "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                  "skipped" => 14
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Use only date filter
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data =
      {
        "profileList" => [
          {
            "name" => "apache-baseline",
            "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
            "failures" => 1,
            "criticals" => 1,
            "skipped" => 69
          },
          {
            "name" => "nginx-baseline",
            "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
            "failures" => 5,
            "majors" => 3,
            "criticals" => 2,
            "passed" => 7,
            "skipped" => 4
          },
          {
            "name" => "fake-baseline",
            "id" => "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9"
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Profile stats for nodes that are in one of the two platform
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "platform", values: ["windows", "debian"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "profileList" => [
          {
            "name" => "apache-baseline",
            "id" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
            "skipped" => 14
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Profile stats for nodes that are using one of the two profiles.
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015", "missing-in-action"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "profileList" => [
          {
            "name" => "linux-baseline",
            "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
            "failures" => 22,
            "criticals" => 22,
            "passed" => 23
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)


      # Use environment filter with missing env for a specific profile_id
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "environment", values: ["Missing In Action"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ],
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "environment", values: ["DevSec Prod beta"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ],
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {
        "controlStats" => [
          {
            "control" => "nginx-01",
            "title" => "Running worker process as non-privileged user",
            "passed" => 1,
            "impact" => 1
          },
          {
            "control" => "nginx-02",
            "title" => "Check NGINX config file owner, group and permissions.",
            "passed" => 1,
            "impact" => 1
          },
          {
            "control" => "nginx-03",
            "title" => "Check NGINX three.",
            "skipped" => 1,
            "impact" => 1
          },
          {
            "control" => "nginx-04",
            "title" => "Check NGINX four",
            "passed" => 1,
            "impact" => 0.5
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)

      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "environment", values: ["DevSec Prod beta"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ],
        type: "summary",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {
        "profileSummary" => {
          "name" => "nginx-baseline",
          "title" => "DevSec Nginx Baseline",
          "version" => "2.1.0",
          "license" => "Apache-2.0",
          "maintainer" => "DevSec Hardening Framework Team",
          "copyright" => "DevSec Hardening Framework Team",
          "copyrightEmail" => "hello@dev-sec.io",
          "summary" => "Test-suite for best-practice nginx hardening",
          "supports" => [{"osFamily" => "unix"}],
          "stats" => {"failed" => 1, "passed" => 5, "skipped" => 2},
          "depends"=>[{"name"=>"myprofile1-new-name", "path"=>"../myprofile1"}]
        }
      }
      assert_equal_json_content(expected_data, actual_data)
      # {"profileList":[],"profileSummary":{"name":"nginx-baseline","title":"DevSec Nginx Baseline","version":"2.1.0","license":"Apache-2.0","maintainer":"DevSec Hardening Framework Team","copyright":"DevSec Hardening Framework Team","copyrightEmail":"hello@dev-sec.io","summary":"Test-suite for best-practice nginx hardening","supports":[{"osFamily":"unix"}],"stats":{"failed":1,"passed":5,"skipped":2}},"controlStats":[]}
      profile_controls = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z']),
          Stats::ListFilter.new(type: 'job_id', values: ['74a54a28-c628-4f82-86df-555555555555']),
          Stats::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149']),
          Stats::ListFilter.new(type: 'platform', values: ['centos', 'win']),
          Stats::ListFilter.new(type: 'status', values: ['passed', 'failed']),
          Stats::ListFilter.new(type: 'node_name', values: ['centos-beta']),
          Stats::ListFilter.new(type: 'roles', values: ['dot.role']),
          Stats::ListFilter.new(type: 'recipes', values: ['java::default']),
        ],
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {
        "controlStats" => [
          {
            "control" => "nginx-01",
            "title" => "Running worker process as non-privileged user",
            "passed" => 2,
            "impact" => 1
          },
          {
            "control" => "nginx-02",
            "title" => "Check NGINX config file owner, group and permissions.",
            "passed" => 2,
            "impact" => 1
          },
          {
            "control" => "nginx-03",
            "title" => "Check NGINX three.",
            "skipped" => 2,
            "impact" => 1
          },
          {
            "control" => "nginx-04",
            "title" => "Check NGINX four",
            "passed" => 1,
            "failed" => 1,
            "impact" => 0.5
          }
        ]
      }
      assert_equal_json_content(expected_data, profile_controls)


      # Compliance Summary without filters
      actual_data = GRPC stats, :read_summary, Stats::Query.new(type: "nodes", filters: [
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "nodeSummary" => {
          "compliant" => 1,
          "skipped" => 1,
          "noncompliant" => 3,
          "highRisk" => 3
        }
      }
      assert_equal_json_content(expected_data, actual_data)

      # Compliance Summary without filters for a node skipped due to unsupported os
      actual_data = GRPC stats, :read_summary, Stats::Query.new(type: "nodes", filters: [
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-07T23:59:59Z'])
      ])
      expected_data = {
        "nodeSummary" => {
          "skipped" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)

      # Compliance Summary Nodes with filters
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "platform", values: ["centos"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
      expected_data = {
        "nodeSummary" => {
          "compliant" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "platform", values: ["windows"]),
          Stats::ListFilter.new(type: "status", values: ["skipped"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
      expected_data = {
        "nodeSummary" => {
          "skipped" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "platform", values: ["debian"]),
          Stats::ListFilter.new(type: 'start_time', values: ['2018-02-04T23:59:59Z']),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ]
      )
      expected_data = {
        "nodeSummary" => {
          "noncompliant" => 1,
          "highRisk" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)


      # Global Compliancy across all controls
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "controls",
        filters: [
          Stats::ListFilter.new(type: 'start_time', values: ['2018-03-02T23:59:59Z']),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
        ]
      )
      expected_data = {
        "controlsSummary" => {
          "passed" => 3,
          "skipped" => 15
        }
      }
      assert_equal_json_content(expected_data, actual_data)

      # Global Compliancy across all controls for nodes that failed the scan
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "controls",
        filters: [
          Stats::ListFilter.new(type: "role", values: ["dot.role", "debian-hardening-prod"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z']),
          Stats::ListFilter.new(type: "status", values: ["failed"])
        ]
      )
      expected_data = {
        "controlsSummary" => {
          "failures" => 22,
          "criticals" => 22,
          "passed" => 24,
          "skipped" => 13
        }
      }
      assert_equal_json_content(expected_data, actual_data)


      # Compliance Summary with role filters
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "role", values: ["apache_deb", "missing"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ]
      )
      expected_data = {
        "reportSummary" => {
          "status" => "failed",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 2
          }
        }
      }
      assert_equal_json_content(expected_data, actual_data)


      # Compliance Summary Nodes with recipe filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "recipe", values: ["apache_extras::windows_harden", "missing.in.action"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
      expected_data = {
        "nodeSummary" => {
          "skipped" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)
    end
  end
end
