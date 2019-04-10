##### GRPC SETUP #####
require 'api/stats/stats_pb'
require 'api/stats/stats_services_pb'

if !ENV['NO_STATS_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Api::Stats
    def stats ; Stats::StatsService ; end

    it "works" do
      #todo- test_top-failures-by-env
      # Use profile_id, env and end_time filters, whoaaa
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["environment"]),
        Stats::ListFilter.new(type: "profile_id", values: ["41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8"]),
        Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-07T23:59:59Z']),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "profiles" => [],
        "platforms" => [],
        "controls" => [],
        "environments" => [
          {
            "name" => "DevSec Prod Zeta",
            "failures" => 1
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
        "profiles" => [],
        "platforms" => [
          {
            "name" => "debian",
            "failures" => 1
          }
        ],
        "controls" => [],
        "environments" => []
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
        ],
        "platforms" => [],
        "controls" => [],
        "environments" => []
      }
      assert_equal_json_content(expected_data, actual_data)

      #todo- test_top-failures-by-control
      # Use control type
      actual_data = GRPC stats, :read_failures, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "types", values: ["control"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
      expected_data = {
        "profiles": [],
        "platforms": [],
        "controls": [
          {
            "name": "os-02",
            "failures": 1
          },
          {
            "name": "os-05",
            "failures": 1
          },
          {
            "name": "sysctl-01",
            "failures": 1
          },
          {
            "name": "sysctl-05",
            "failures": 1
          },
          {
            "name": "sysctl-06",
            "failures": 1
          },
          {
            "name": "sysctl-07",
            "failures": 1
          },
          {
            "name": "sysctl-08",
            "failures": 1
          },
          {
            "name": "sysctl-09",
            "failures": 1
          },
          {
            "name": "sysctl-10",
            "failures": 1
          },
          {
            "name": "sysctl-15",
            "failures": 1
          }
        ],
        "environments": []
      }
      assert_equal_json_content(expected_data, actual_data)


    end
  end
end
