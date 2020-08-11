##### GRPC SETUP #####
require 'interservice/compliance/stats/stats_pb'
require 'interservice/compliance/stats/stats_services_pb'

if !ENV['NO_STATS_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Stats

    def stats;
      Stats::StatsService;
    end

    it "read_failures env-platform with profile_id and end_time filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["environment", "platform"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
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
    end

    it "read_failures profile-platform with profile_id and end_time filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
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
    end

    it "read_failures profile-platform-env with profile_id and end_time filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
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
    end

    it "read_failures profile-platform-env-control profile-deep for the last 24h" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4"])
        ]
      )
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_failures profile-platform-env-control with profile_id and end_time filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
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
    end

    it "read_failures profile-platform-env-control with profile_id, control and end_time filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
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
    end

    it "read_failures profile-platform-env-control with root profile profile failure" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ]
      )
      expected_data = {
        "profiles" => [{"name" => "myfaily", "failures" => 1}],
        "platforms" => [{"name" => "ubuntu", "failures" => 1}],
        "environments" => [{"name" => "DevSec Prod Alpha", "failures" => 1}]
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_failures profile-platform-env-control with root profile profile skipped" do
      # Read Failures with root skipped profile filter
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "platform", "environment", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-04-02T23:59:59Z'])
        ]
      )
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_failures profile-control types with profile_id filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile", "control"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
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
    end

    it "read_failures platform type multiple filters" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["platform"]),
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
          Stats::ListFilter.new(type: "node_id", values: ["34cbbb4c-c502-4971-b193-00e987b4678c"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ]
      )
      expected_data = {
          "platforms" => [
              {
                  "name" => "debian",
                  "failures" => 1
              }
          ]
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_failures just by profile, size 2" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["profile"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ],
        size: 2
      )
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
    end

    it "read_failures control type and end_time filter" do
      actual_data = GRPC stats, :read_failures, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "types", values: ["control"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ]
      )
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
    end

    it "read_profiles stats list for nodes that are using one of the two profiles with waivers" do
      #calls: stats::ReadProfiles::GetProfileListWithAggregatedComplianceSummaries
      #depth:Profile
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ]
      )
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
    end

    it "read_profiles stats list for nodes that are using one profile and one control" do
      #calls: stats::ReadProfiles::GetProfileListWithAggregatedComplianceSummaries
      #depth:Control
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
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

    it "read_profiles stats for a profile scanned in the last 24h" do
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4"])
      ])
      expected_data = {
        "profileList" => [
          {
            "name" => "myprofile1",
            "id" => "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
            "passed" => 4,
            "skipped" => 1
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_profiles control stats at control depth, passed scenario" do
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
        filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod beta"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-04"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
        ],
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
    end

    it "read_profiles control stats at control depth, last 24h" do
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        type: "controls",
        id: "447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4"]),
          Stats::ListFilter.new(type: "control", values: ["pro1-con1"])
        ],
      )
      expected_data = {
        "controlStats" => [
          {
            "control" => "pro1-con1",
            "title" => "Profile 1 - Control 1",
            "passed" => 1,
            "impact" => 0.80000001
          }
        ]
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_profiles control stats at control depth, skipped scenario" do
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
        filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod beta"]),
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: "control", values: ["nginx-03"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
        ]
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
    end

    it "read_profiles control stats at control depth, waived scenario" do
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth:Control
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: "control", values: ["os-03"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ],
        type: "controls",
        id: "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"
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
    end

    it "read_profiles control stats with missing env filter" do
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
    end

    it "read_profiles control stats with profile NOT scanned in the last 24h" do
      actual_data = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [],
        type: "controls",
        id: "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
      )
      expected_data = {}
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_profiles control stats with profile id and env filter" do
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
    end

    it "read_profiles control stats with lots of filters" do
      #calls: stats::ReadProfiles::GetControlListStatsByProfileID
      #depth: Profile
      profile_controls = GRPC stats, :read_profiles, Stats::Query.new(
        filters: [
          Stats::ListFilter.new(type: 'start_time', values: ['2017-01-01T23:59:59Z']),
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
      assert_equal_json_content(expected_data, profile_controls)
    end

    it "read_profiles profile list with profile and end_time filter" do
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
    end

    it "read_summary nodes with a failed profile and end_time filter" do
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
    end

    it "read_summary nodes with a passed profile and NO end_time filter (24h)" do
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4"])
        ])
      expected_data = {
        "nodeSummary" => { "compliant" => 1 }
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_summary controls with profile_id and end_time filter" do
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
    end

    it "read_summary nodes with a skipped profile and end_time filter" do
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
    end

    it "read_summary controls with a passed profile and end_time filter" do
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
    end

    it "read_summary nodes with a profile that ran on two nodes" do
      #calls: stats::ReadSummary::GetStatsSummaryNodes
      #depth: Profile
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z']),
        ]
      )
      expected_data = {
        "nodeSummary" => {
          "noncompliant" => 1,
          "highRisk" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_summary nodes with a profile+control depth filter and a waivey node" do
      #calls: stats::ReadSummary::GetStatsSummaryNodes
      #depth: Control
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "nodes",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
          Stats::ListFilter.new(type: "control", values: ["os-02"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
        ]
      )
      expected_data = {
        "nodeSummary" => {
          "waived" => 1
        }
      }
      assert_equal_json_content(expected_data, actual_data)
    end

    it "read_summary controls with a profile+control depth filter and a waivey node" do
      # Compliance Summary with profile filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "controls",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
        ]
      )
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
    end

    it "read_summary controls with a profile+control depth filter and multiple nodes" do
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
    end

    it "read_summary report with a skipped profile filter" do
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

    it "read_summary controls with a profile scanned multiple times in the last 24h" do
      actual_data = GRPC stats, :read_summary, Stats::Query.new(
        type: "controls",
        filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4"])
        ])
      expected_data = {
        "controlsSummary" => { "passed" => 4, "skipped" => 1 }
      }
      assert_equal_json_content(expected_data, actual_data)
    end
  end
end
