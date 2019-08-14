
##### GRPC SETUP #####
require 'api/stats/stats_pb'
require 'api/stats/stats_services_pb'

if !ENV['NO_STATS_SUMMARY_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Api::Stats unless defined?(Stats)
    def stats ; Stats::StatsService ; end

    it "works" do
      ##### Failure tests #####
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "profile_id", values: ["123"])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "unknown",
          "stats" => {}
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      ##### Success tests #####
      # Get stats without end_time filter
      # This one is tricky to test because when no end_time is passed in, the backend (correctly) sets it to today.utc.
      # Without test scans being run every day, it is therefore very difficult to do..
      # Just make sure it comes back as listed in expected.
      actual_data = GRPC stats, :read_summary, Stats::Query.new()
      expected_data = {
          "reportSummary" => {
          "status" => "unknown",
          "stats" => {}
      }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Get stats with end_time filter
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "failed",
          "stats" => {
            "nodes" => 5,
            "platforms" => 3,
            "environments" => 3,
            "profiles" => 3
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by profile_id
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "failed",
          "stats" => {
            "nodes" => 4,
            "platforms" => 2,
            "environments" => 2,
            "profiles" => 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # Filter by control tag
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Stats::ListFilter.new(type: 'control_tag:scope', values: ['nginx', 'Apache'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "failed",
          "stats" => {
            "nodes" => 3,
            "platforms" => 3,
            "environments" => 2,
            "profiles" => 3
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # Filter by job_id
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "job_id", values: ["74a54a28-c628-4f82-86df-444444444444"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "skipped",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by multiple profile_ids
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "profile_id", values: [
          "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9", "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"
        ]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "failed",
          "stats" => {
            "nodes" => 5,
            "platforms" => 3,
            "environments" => 3,
            "profiles" => 2
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by node_id
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "node_id", values: ["9b9f4e51-b049-4b10-9555-10578916e149"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "passed",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 3
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by node_id and profile_id where profile ran on node
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "node_id", values: ["9b9f4e51-b049-4b10-9555-10578916e149"]),
        Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "passed",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by node_id and profile_id where profile did not run on node
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "node_id", values: ["9b9f4e51-b049-4b10-9555-10578916e149"]),
        Stats::ListFilter.new(type: "profile_id", values: ["b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "unknown",
          "stats" => {}
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by environment and profile_id where profile ran in environment
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Zeta"]),
        Stats::ListFilter.new(type: "profile_id", values: ["41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "skipped",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by environment and profile_id where profile did not run in environment
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "environment", values: ["DevSec Prod Missing"]),
        Stats::ListFilter.new(type: "profile_id", values: ["41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "unknown",
          "stats" => {}
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by platform and profile_id where profile ran on nodes on platform
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "platform", values: ["windows"]),
        Stats::ListFilter.new(type: "profile_id", values: ["41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "skipped",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by platform and all profiles that ran on nodes of filtered platform
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "platform", values: ["centos"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "passed",
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 3
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      # Filter by platform and all profiles that ran on nodes of filtered platform
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
        Stats::ListFilter.new(type: "platform", values: ["debian"]),
        Stats::ListFilter.new(type: "status", values: ["failed"]),
        Stats::ListFilter.new(type: 'end_time', values: ['2018-02-09T23:59:59Z'])
      ])
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
      }.to_json
      assert_equal(expected_data, actual_data.to_json)


      ##Deep filtering
      # todo -check these numbers well.. in addition to these tests, look closely at our es indices and make sure they are all good

      # Filter by profile_id
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
      ])
      expected_data = {
          "reportSummary" => {
          "status" => "failed",
          "stats" => {
              "nodes" => 4,
          "platforms" => 2,
          "environments" => 2,
          "profiles" => 1
      }
      }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # Filter by profile_id
      # apache-baseline is skipped on centos
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
          Stats::ListFilter.new(type: "platform", values: ["centos"])
      ])
      expected_data = {
          "reportSummary" => {
          "status" => "skipped",
          "stats" => {
              "nodes" => 1,
          "platforms" => 1,
          "environments" => 1,
          "profiles" => 1
      }
      }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # Filter by profile_id
      # nginx-baseline passes on centos
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
          Stats::ListFilter.new(type: "platform", values: ["centos"])
      ])
      expected_data = {
          "reportSummary" => {
          "status" => "passed",
          "stats" => {
              "nodes" => 1,
          "platforms" => 1,
          "environments" => 1,
          "profiles" => 1
      }
      }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # Filter by control_id
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
      Stats::ListFilter.new(type: 'control', values: ['nginx-01'])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "failed",
          "stats" => {
            "nodes" => 4,
            "platforms" => 2,
            "environments" => 2,
            "profiles" => 1
          }
        }
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # Filter by control_id
      actual_data = GRPC stats, :read_summary, Stats::Query.new(filters: [
          Stats::ListFilter.new(type: "profile_id", values: ["09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988"]),
          Stats::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
          Stats::ListFilter.new(type: 'control', values: ['nginx-01']),
          Stats::ListFilter.new(type: "platform", values: ["centos"])
      ])
      expected_data = {
        "reportSummary" => {
          "status" => "passed",   #look at this business! yes! for this control, we have a status of passed! notice the test above, we had 4 nodes with a status of failed
          "stats" => {
            "nodes" => 1,
            "platforms" => 1,
            "environments" => 1,
            "profiles" => 1
          }
        }
      }.to_json

      assert_equal(expected_data, actual_data.to_json)
    end
  end
end
