require 'cgi'

##### GRPC SETUP #####
require 'api/stats/stats_pb'
require 'api/stats/stats_services_pb'

if !ENV['NO_STATS_TREND_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Api::Stats unless defined?(Stats)
    def stats ; Stats::StatsService ; end

    it "works" do
      ##### Failure tests #####
      assert_grpc_error("Required filter 'end_time' must be a date with format: 'YYYY-MM-DDThh:mm:ss+|-hh:mm'", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "nodes")
      end
      assert_grpc_error("Minimum value for 'interval' is 3600(one hour)", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "nodes", interval: 1)
      end

      assert_grpc_error("Required filter 'end_time' must be a date with format: 'YYYY-MM-DDThh:mm:ss+|-hh:mm'", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "controls")
      end
      assert_grpc_error("Minimum value for 'interval' is 3600(one hour)", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "controls", interval: 1)
      end

      END_OF_DAY="23:59:59Z"
      ##### Success tests #####
      # The whole enchilada. For nodes
      # Filter: none
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z", "failed" => 1},
              {"reportTime" => "2018-02-10T23:59:59Z"}, {"reportTime" => "2018-02-11T23:59:59Z"}, {"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"}, {"reportTime" => "2018-02-14T23:59:59Z"}, {"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"}, {"reportTime" => "2018-02-17T23:59:59Z"}, {"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"}, {"reportTime" => "2018-02-20T23:59:59Z"}, {"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"}, {"reportTime" => "2018-02-23T23:59:59Z"}, {"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"}, {"reportTime" => "2018-02-26T23:59:59Z"}, {"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"}, {"reportTime" => "2018-03-01T23:59:59Z"}, {"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z", "passed" => 1, "failed" => 3, "skipped" => 1},
              {"reportTime" => "2018-03-05T23:59:59Z", "passed" => 1},
              {"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole enchilada. for controls
      # Filter: none
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z", "passed" => 24, "failed" => 22, "skipped" => 13},
              {"reportTime" => "2018-02-10T23:59:59Z"}, {"reportTime" => "2018-02-11T23:59:59Z"}, {"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"}, {"reportTime" => "2018-02-14T23:59:59Z"}, {"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"}, {"reportTime" => "2018-02-17T23:59:59Z"}, {"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"}, {"reportTime" => "2018-02-20T23:59:59Z"}, {"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"}, {"reportTime" => "2018-02-23T23:59:59Z"}, {"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"}, {"reportTime" => "2018-02-26T23:59:59Z"}, {"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"}, {"reportTime" => "2018-03-01T23:59:59Z"}, {"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z", "passed" => 7, "failed" => 6, "skipped" => 73},
              {"reportTime" => "2018-03-05T23:59:59Z", "passed" => 3, "skipped" => 15},
              {"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. For nodes
      # Filter: environment="DevSec Prod Zeta"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","failed" => 1},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z","skipped" => 1},
              {"reportTime" => "2018-03-05T23:59:59Z"},
              {"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. for controls
      # Filter: environment="DevSec Prod Zeta"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","passed" => 24,"failed" => 22,"skipped" => 13},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z","skipped" => 14},
              {"reportTime" => "2018-03-05T23:59:59Z"},
              {"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. For nodes
      # Filter: environment="DevSec Prod Zeta" roles="base_windows"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "role", values: ['base_windows'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},{"reportTime" => "2018-02-09T23:59:59Z"},{"reportTime" => "2018-02-10T23:59:59Z"},
              {"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},{"reportTime" => "2018-02-13T23:59:59Z"},
              {"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},{"reportTime" => "2018-02-16T23:59:59Z"},
              {"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},{"reportTime" => "2018-02-19T23:59:59Z"},
              {"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},{"reportTime" => "2018-02-22T23:59:59Z"},
              {"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},{"reportTime" => "2018-02-25T23:59:59Z"},
              {"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},{"reportTime" => "2018-02-28T23:59:59Z"},
              {"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},{"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z","skipped" => 1},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. for controls
      # Filter: environment="DevSec Prod Zeta" roles="base_windows"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "role", values: ['base_windows'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},{"reportTime" => "2018-02-09T23:59:59Z"},{"reportTime" => "2018-02-10T23:59:59Z"},
              {"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},{"reportTime" => "2018-02-13T23:59:59Z"},
              {"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},{"reportTime" => "2018-02-16T23:59:59Z"},
              {"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},{"reportTime" => "2018-02-19T23:59:59Z"},
              {"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},{"reportTime" => "2018-02-22T23:59:59Z"},
              {"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},{"reportTime" => "2018-02-25T23:59:59Z"},
              {"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},{"reportTime" => "2018-02-28T23:59:59Z"},
              {"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},{"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z","skipped" => 14},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. For nodes
      # Filter: environment="DevSec Prod Zeta" roles="base_windows"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "role", values: ['some non-existent role'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},{"reportTime" => "2018-02-09T23:59:59Z"},{"reportTime" => "2018-02-10T23:59:59Z"},
              {"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},{"reportTime" => "2018-02-13T23:59:59Z"},
              {"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},{"reportTime" => "2018-02-16T23:59:59Z"},
              {"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},{"reportTime" => "2018-02-19T23:59:59Z"},
              {"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},{"reportTime" => "2018-02-22T23:59:59Z"},
              {"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},{"reportTime" => "2018-02-25T23:59:59Z"},
              {"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},{"reportTime" => "2018-02-28T23:59:59Z"},
              {"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},{"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z"},{"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. for controls
      # Filter: environment="DevSec Prod Zeta" roles="base_windows"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "role", values: ['some non-existent role'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},{"reportTime" => "2018-02-09T23:59:59Z"},{"reportTime" => "2018-02-10T23:59:59Z"},
              {"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},{"reportTime" => "2018-02-13T23:59:59Z"},
              {"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},{"reportTime" => "2018-02-16T23:59:59Z"},
              {"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},{"reportTime" => "2018-02-19T23:59:59Z"},
              {"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},{"reportTime" => "2018-02-22T23:59:59Z"},
              {"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},{"reportTime" => "2018-02-25T23:59:59Z"},
              {"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},{"reportTime" => "2018-02-28T23:59:59Z"},
              {"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},{"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z"},{"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      ##### Deep filtering tests #####

      # The whole range. For nodes
      # Filter: environment="DevSec Prod Zeta" roles="base_windows"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "profile_id", values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z"},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z","skipped" => 1},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      # The whole range. for controls
      # Filter: environment="DevSec Prod Zeta" roles="base_windows"
      # should have zeroes on the bookends, some numbers on 2/9, 3/4 and 3/5 and zeroes in between.
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "profile_id", values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","passed" => 1, "skipped" => 13},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z"},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "control", values: ["apache-03"]),
              Stats::ListFilter.new(type: "profile_id", values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","passed" => 1},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z"},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "control", values: ["apache-01"]),
              Stats::ListFilter.new(type: "profile_id", values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","skipped" => 1},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z"},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta']),
              Stats::ListFilter.new(type: "control", values: ["apache-14"]),
              Stats::ListFilter.new(type: "profile_id", values: ['41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8'])
          ]
      )
      #Bunching the data up as shown below, helps in it's readability. See how the numbers pop out at you?
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","skipped" => 1},
              {"reportTime" => "2018-02-10T23:59:59Z"},{"reportTime" => "2018-02-11T23:59:59Z"},{"reportTime" => "2018-02-12T23:59:59Z"},
              {"reportTime" => "2018-02-13T23:59:59Z"},{"reportTime" => "2018-02-14T23:59:59Z"},{"reportTime" => "2018-02-15T23:59:59Z"},
              {"reportTime" => "2018-02-16T23:59:59Z"},{"reportTime" => "2018-02-17T23:59:59Z"},{"reportTime" => "2018-02-18T23:59:59Z"},
              {"reportTime" => "2018-02-19T23:59:59Z"},{"reportTime" => "2018-02-20T23:59:59Z"},{"reportTime" => "2018-02-21T23:59:59Z"},
              {"reportTime" => "2018-02-22T23:59:59Z"},{"reportTime" => "2018-02-23T23:59:59Z"},{"reportTime" => "2018-02-24T23:59:59Z"},
              {"reportTime" => "2018-02-25T23:59:59Z"},{"reportTime" => "2018-02-26T23:59:59Z"},{"reportTime" => "2018-02-27T23:59:59Z"},
              {"reportTime" => "2018-02-28T23:59:59Z"},{"reportTime" => "2018-03-01T23:59:59Z"},{"reportTime" => "2018-03-02T23:59:59Z"},
              {"reportTime" => "2018-03-03T23:59:59Z"},
              {"reportTime" => "2018-03-04T23:59:59Z"},
              {"reportTime" => "2018-03-05T23:59:59Z"},{"reportTime" => "2018-03-06T23:59:59Z"}
          ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)

      ##### Failure tests #####
      # StartDate>EndDate. For nodes
      assert_grpc_error("bad date range. end date must be greater than or equal to start date", 2) do
        GRPC stats, :read_trend, Stats::Query.new(
            type: "nodes",
            interval: 86400,
            filters: [
                Stats::ListFilter.new(type: "start_time", values: ['2018-03-08T00:00:00Z']),
                Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"])
            ]
        )
      end
    end
  end
end
