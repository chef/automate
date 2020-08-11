require 'cgi'

##### GRPC SETUP #####
require 'interservice/compliance/stats/stats_pb'
require 'interservice/compliance/stats/stats_services_pb'

if !ENV['NO_STATS_TREND_TESTS']
  describe File.basename(__FILE__) do
    Stats = Chef::Automate::Domain::Compliance::Stats unless defined?(Stats)
    def stats ; Stats::StatsService ; end
    END_OF_DAY="23:59:59Z"

    it "read_trend nodes returns error for missing or invalid end_time filter" do
      assert_grpc_error("Required filter 'end_time' must be a date with format: 'YYYY-MM-DDThh:mm:ss+|-hh:mm'", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "nodes")
      end
    end

    it "read_trend nodes returns error when interval is less then one hour" do
      assert_grpc_error("Minimum value for 'interval' is 3600(one hour)", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "nodes", interval: 1)
      end
    end

    it "read_trend controls returns error for missing or invalid end_time filter" do
      assert_grpc_error("Required filter 'end_time' must be a date with format: 'YYYY-MM-DDThh:mm:ss+|-hh:mm'", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "controls")
      end
    end

    it "read_trend controls returns error when interval is less then one hour" do
      assert_grpc_error("Minimum value for 'interval' is 3600(one hour)", 3) do
        actual_data = GRPC stats, :read_trend, Stats::Query.new(type: "controls", interval: 1)
      end
    end

    it "read_trend nodes returns error when start_time if newer than end_time" do
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

    it "read_trend nodes returns trend data for the whole enchilada" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"])
          ]
      )
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
    end

    it "read_trend controls returns trend data for the whole enchilada" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"])
          ]
      )
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z", "passed" => 23, "failed" => 21, "skipped" => 12, "waived" => 3},
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
    end

    it "read_trend nodes with environment filter" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta'])
          ]
      )
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
    end

    it "read_trend nodes with control tag filter" do
      # Filter: control tag with key 'web'
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "nodes",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-09T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-05T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: 'control_tag:web', values: [])
          ]
      )
      expected_data = {
        "trends":[
          {"reportTime":"2018-02-09T23:59:59Z", "failed":1},
          {"reportTime":"2018-02-10T23:59:59Z"},
          {"reportTime":"2018-02-11T23:59:59Z"},
          {"reportTime":"2018-02-12T23:59:59Z"},
          {"reportTime":"2018-02-13T23:59:59Z"},
          {"reportTime":"2018-02-14T23:59:59Z"},
          {"reportTime":"2018-02-15T23:59:59Z"},
          {"reportTime":"2018-02-16T23:59:59Z"},
          {"reportTime":"2018-02-17T23:59:59Z"},
          {"reportTime":"2018-02-18T23:59:59Z"},
          {"reportTime":"2018-02-19T23:59:59Z"},
          {"reportTime":"2018-02-20T23:59:59Z"},
          {"reportTime":"2018-02-21T23:59:59Z"},
          {"reportTime":"2018-02-22T23:59:59Z"},
          {"reportTime":"2018-02-23T23:59:59Z"},
          {"reportTime":"2018-02-24T23:59:59Z"},
          {"reportTime":"2018-02-25T23:59:59Z"},
          {"reportTime":"2018-02-26T23:59:59Z"},
          {"reportTime":"2018-02-27T23:59:59Z"},
          {"reportTime":"2018-02-28T23:59:59Z"},
          {"reportTime":"2018-03-01T23:59:59Z"},
          {"reportTime":"2018-03-02T23:59:59Z"},
          {"reportTime":"2018-03-03T23:59:59Z"},
          {"reportTime":"2018-03-04T23:59:59Z", "passed":1, "failed":1, "skipped":1},
          {"reportTime":"2018-03-05T23:59:59Z"}
        ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)
    end

    it "read_trend nodes for the special April month" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
        type: "nodes",
        interval: 86400,
        filters: [
          Stats::ListFilter.new(type: "start_time", values: ['2018-04-01T00:00:00Z']),
          Stats::ListFilter.new(type: "end_time", values: ["2018-04-05T#{END_OF_DAY}"])
        ]
      )
      expected_data = {
        "trends": [
          {
            "reportTime": "2018-04-01T23:59:59Z",
            "failed": 1,
            "waived": 1
          },
          {
            "reportTime": "2018-04-02T23:59:59Z",
            "failed": 1
          },
          {
            "reportTime": "2018-04-03T23:59:59Z",
            "failed": 1
          },
          {
            "reportTime": "2018-04-04T23:59:59Z"
          },
          {
            "reportTime": "2018-04-05T23:59:59Z"
          }
        ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)
    end

    it "read_trend nodes for the special April month skipped profile deep" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
        type: "nodes",
        interval: 86400,
        filters: [
          Stats::ListFilter.new(type: "start_time", values: ['2018-04-01T00:00:00Z']),
          Stats::ListFilter.new(type: "end_time", values: ["2018-04-05T#{END_OF_DAY}"]),
          Stats::ListFilter.new(type: "profile_id", values: ['5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777'])
        ]
      )
      expected_data = {
        "trends": [
          {
            "reportTime": "2018-04-01T23:59:59Z"
          },
          {
            "reportTime": "2018-04-02T23:59:59Z",
            "skipped": 1
          },
          {
            "reportTime": "2018-04-03T23:59:59Z"
          },
          {
            "reportTime": "2018-04-04T23:59:59Z"
          },
          {
            "reportTime": "2018-04-05T23:59:59Z"
          }
        ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)
    end

    it "read_trend controls for the special April month" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
        type: "controls",
        interval: 86400,
        filters: [
          Stats::ListFilter.new(type: "start_time", values: ['2018-04-01T00:00:00Z']),
          Stats::ListFilter.new(type: "end_time", values: ["2018-04-05T#{END_OF_DAY}"])
        ]
      )
      expected_data = {
        "trends": [
          {
            "reportTime": "2018-04-01T23:59:59Z",
            "failed": 2,
            "waived": 5
          },
          {
            "reportTime": "2018-04-02T23:59:59Z",
            "passed": 1
          },
          {
            "reportTime": "2018-04-03T23:59:59Z"
          },
          {
            "reportTime": "2018-04-04T23:59:59Z"
          },
          {
            "reportTime": "2018-04-05T23:59:59Z"
          }
        ]
      }.to_json
      assert_equal(expected_data, actual_data.to_json)
    end

    it "read_trend controls with environment filter" do
      actual_data = GRPC stats, :read_trend, Stats::Query.new(
          type: "controls",
          interval: 86400,
          filters: [
              Stats::ListFilter.new(type: "start_time", values: ['2018-02-08T00:00:00Z']),
              Stats::ListFilter.new(type: "end_time", values: ["2018-03-06T#{END_OF_DAY}"]),
              Stats::ListFilter.new(type: "environment", values: ['DevSec Prod Zeta'])
          ]
      )
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z", "passed" => 23, "failed" => 21, "skipped" => 12, "waived" => 3},
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
    end

    it "read_trend nodes with environment and role filters" do
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
    end

    it "read_trend controls with environment and role filters" do
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
    end

    it "read_trend nodes with environment and non-existent role filters" do
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
    end

    it "read_trend controls with environment and non-existent role filters" do
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
    end

    it "read_trend nodes deep filtered by profile_id and environment" do
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
    end

    it "read_trend controls deep filtered by profile_id and environment" do
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
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z", "passed" => 1, "skipped" => 12, "waived" => 1},
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
    end

    it "read_trend controls deep filtered by profile_id and control" do
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
    end

    it "read_trend controls deep filtered by profile_id and another control" do
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
      expected_data = {
          "trends" => [
              {"reportTime" => "2018-02-08T23:59:59Z"},
              {"reportTime" => "2018-02-09T23:59:59Z","waived" => 1},
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
    end

    it "read_trend controls deep filtered by profile_id and yet another control" do
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
    end
  end
end
