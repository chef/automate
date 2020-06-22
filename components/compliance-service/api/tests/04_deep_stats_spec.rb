##### GRPC SETUP #####
require 'api/interservice/compliance/stats/stats_pb'
require 'api/interservice/compliance/stats/stats_services_pb'

if !ENV['NO_STATS_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Stats

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

      # Read Failures with root failed profile filter
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
      ])
      expected_data = {
        "profiles" => [{"name" => "myfaily", "failures" => 1}],
        "platforms" => [{"name" => "ubuntu", "failures" => 1}],
        "environments" => [{"name" => "DevSec Prod Alpha", "failures" => 1}]
      }
      assert_equal_json_content(expected_data, actual_data)

      # Read Failures with root skipped profile filter
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
      ])
      expected_data = {}
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



      #description: Profile stats list for nodes that are using one of the two profiles with waivers.
      #calls: stats::ReadProfiles::GetProfileListWithAggregatedComplianceSummaries
      #depth:Profile
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
          "profileList" => [
              {
                  "name" => "linux-baseline",
                  "id" => "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
                  "failures" => 21,
                  "criticals" => 21,
                  "passed" => 22,
                  "waived" => 2
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)


      #todo - get this one working
      #description: Profile stats list for nodes that are using one profile and one control.
      #calls: stats::ReadProfiles::GetProfileListWithAggregatedComplianceSummaries
      #depth:Control
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


      #description: control stats at control depth
      #scenario: control status passed
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
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

      #description: control stats at control depth
      #scenario: control status skipped
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
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

      #description: control stats at control depth
      #scenario: control status waived
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: "control", values: ["os-03"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ], type: "controls", id: "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"
      )
      expected_data = {
          "controlStats" => [
              {

                  "control" => "os-03",
                  "title" => "Check owner and permissions for /etc/passwd",
                  "impact" => 1,
                  "waived" => 1

              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)

      #description: Profile stats list for nodes.
      #filter: missing env for a specific profile_id and end_time
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth: Profile
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "environment", values: ["Missing In Action"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
        ],
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)


      #description: Profile stats list for nodes.
      #filter: env and profile_id and end_time
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth: Profile
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
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

      #description: Profile stats list for nodes.
      #filter: multiple
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth: Profile
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

      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ]
      )
      expected_data = {
        "profileList" => [
          { "name" => "myfaily",
            "id" => "5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888"
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)

      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ])
      expected_data = {
        "nodeSummary" => { "noncompliant" => 1 }
      }
      assert_equal_json_content(expected_data, actual_data)

      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "controls",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ])
      expected_data = {
        "controlsSummary" => {}
      }
      assert_equal_json_content(expected_data, actual_data)


      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ])
      expected_data = {
        "nodeSummary" => { "skipped" => 1 }
      }
      assert_equal_json_content(expected_data, actual_data)

      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "controls",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ])
      expected_data = {
        "controlsSummary" => { "passed" => 1 }
      }
      assert_equal_json_content(expected_data, actual_data)

      #####deep summary stats
      #description: Node stats for profile depth.
      #filter:  one profile
      #calls: stats::ReadSummary::GetStatsSummaryNodes
      #depth: Profile
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
          type: "nodes",
          filters: [
              Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
              Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z']),
          ])
      expected_data = {
          "nodeSummary" => {
              "noncompliant" => 1,
              "highRisk" => 1
          }
      }
      assert_equal_json_content(expected_data, actual_data)

      #description: Node stats for control depth.
      #filter:  one profile one control
      #calls: stats::ReadSummary::GetStatsSummaryNodes
      #depth: Control
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
          type: "nodes",
          filters: [
              Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
              Stats::ListFilter.new(type: "control", values: ["os-02"]),
              Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
          ])
      expected_data = {
          "nodeSummary" => {
              "waived" => 1
          }
      }
      assert_equal_json_content(expected_data, actual_data)


      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
          type: "controls",
          filters: [
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

      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
          type: "controls",
          filters: [
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

      # Get stats with end_time filter for the faily/skippy 04-02 date with deep for skipped profile
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ])
      expected_data = {
        "reportSummary": {
          "status": "skipped",
          "stats": {
            "nodes": "1",
            "platforms": 1,
            "environments": 1,
            "profiles": 1,
            "nodesCnt": 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)
    end
  end
end
