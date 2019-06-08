##### GRPC SETUP #####
require 'api/stats/stats_pb'
require 'api/stats/stats_services_pb'

if !ENV['NO_STATS_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Api::Stats

    def stats;
      Stats::StatsService;
    end

    it "works" do
      #todo- test_top-failures-by-env - deep profile level
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["environment", "platform"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "platforms" => [
              {
                  "name" => "redhat",
                  "failures" => 3
              }
          ],
          "environments" => [
              {
                  "name" => "DevSec Prod Alpha",
                  "failures" => 2
              },
              {
                  "name" => "DevSec Prod beta",
                  "failures" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)

      #todo- test_top-failures-by-env - deep profile level
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profiles" => [
              {
                  "name" => "nginx-baseline",
                  "failures" => 3
              }
          ],
          "platforms" => [
              {
                  "name" => "redhat",
                  "failures" => 3
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo- test_top-failures-by-env - deep profile level
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profiles" => [
              {
                  "name" => "nginx-baseline",
                  "failures" => 3
              }
          ],
          "platforms" => [
              {
                  "name" => "redhat",
                  "failures" => 3
              }
          ],
          "environments" => [
              {
                  "name" => "DevSec Prod Alpha",
                  "failures" => 2
              },
              {
                  "name" => "DevSec Prod beta",
                  "failures" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)

      #todo- test all top failure types - deep profile level
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profiles" => [
              {
                  "name" => "nginx-baseline",
                  "failures" => 3
              }
          ],
          "platforms" => [
              {
                  "name" => "redhat",
                  "failures" => 3
              }
          ],
          "controls" => [
              {
                  "name" => "nginx-04",
                  "failures" => 3
              },
              {
                  "name" => "nginx-01",
                  "failures" => 2
              }
          ],
          "environments" => [
              {
                  "name" => "DevSec Prod Alpha",
                  "failures" => 2
              },
              {
                  "name" => "DevSec Prod beta",
                  "failures" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)

      #todo- test all top failure types - deep profile level
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profiles" => [
              {
                  "name" => "nginx-baseline",
                  "failures" => 3
              }
          ],
          "platforms" => [
              {
                  "name" => "redhat",
                  "failures" => 3
              }
          ],
          "controls" => [
              {
                  "name" => "nginx-04",
                  "failures" => 3
              }
          ],
          "environments" => [
              {
                  "name" => "DevSec Prod Alpha",
                  "failures" => 2
              },
              {
                  "name" => "DevSec Prod beta",
                  "failures" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo- test top failures. the profile overview batch - deep profile level
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profiles" => [
              {
                  "name" => "nginx-baseline",
                  "failures" => 3
              }
          ],
          "controls" => [
              {
                  "name" => "nginx-04",
                  "failures" => 3
              },
              {
                  "name" => "nginx-01",
                  "failures" => 2
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo- test_top-failures-by-platform
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

      #todo- test_top-failures-by-profile
      # Get failures by profile
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ],
                                                                 size: 2)
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

      #todo- test_top-failures-by-control
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

      #todo - this one is not in 04_stats_spec.. it tests deep profile filtering for nodes type
      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(type: "nodes", filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "nodeSummary" => {
              "compliant" => 1,
              "noncompliant" => 3,
              "highRisk" => 2,
              "mediumRisk" => 1
          }
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo - this one is not in 04_stats_spec.. it tests control filtering for nodes type
      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(type: "nodes", filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "nodeSummary" => {
              "compliant" => 1,
              "noncompliant" => 3,
              "mediumRisk" => 3
          }
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo - this one is not in 04_stats_spec.. it tests deep profile filtering for control type
      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(type: "controls", filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "controlsSummary" => {
              "failures" => 5,
              "majors" => 3,
              "criticals" => 2,
              "passed" => 7,
              "skipped" => 4
          }
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo - this one is not in 04_stats_spec.. it tests control filtering for control type
      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(type: "controls", filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "controlsSummary" => {
              "failures" => 3,
              "majors" => 3,
              "passed" => 1
          }
      }
      assert_equal_json_content(expected_data, actual_data)


      # Profile stats for nodes that are using one of the two profiles.
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
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


      # profile deep with profile_id specified as a filter
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod beta"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ], type: "controls", id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {
          "controlStats" => [
              {
                  "control" => "nginx-04",
                  "title" => "Check NGINX four",
                  "passed" => 1,
                  "impact" => 0.5
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)

      # profile deep with profile_id specified as a filter
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod beta"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-03"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
      ], type: "controls", id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {
          "controlStats" => [
              {

                  "control" => "nginx-03",
                  "title" => "Check NGINX three.",
                  "skipped" => 1,
                  "impact" => 1

              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)

      #Profile stats for nodes that are using one profile one control.
      #todo - get this one working
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "profileList" => [
              {
                  "name" => "nginx-baseline",
                  "id" => "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
                  "failures" => 3,
                  "majors" => 3,
                  "passed" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)
    end
  end
end
