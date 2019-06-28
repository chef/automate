##### GRPC SETUP #####
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  it "works" do
    reporting = Reporting::ReportingService

    ##### Success tests #####
    # sort by node name now
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-02T00:00:01Z']), # start_time is ignored for this call. Only end_time is used
      Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'name')
    assert_equal([ "centos(2)-beta-nginx(p)-apache(s)-passed","windows(1)-zeta-apache(s)-skipped" ],
                 resp['nodes'].map{ |x| x['name'] }
    )


    # Get all nodes, sorted by latest_report(default), asc(default) order
    # Hits only daily index 2018-03-05
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
    ])
    expected_nodes = {
      "nodes": [
        {
          "id": "74a54a28-c628-4f82-86df-centosssssss",
          "name": "centos(2)-beta-nginx(p)-apache(s)-passed",
          "platform": {
            "name": "centos",
            "release": "5.11"
          },
          "environment": "DevSec Prod Beta",
          "latestReport": {
            "id": "bb93e1b2-36d6-439e-ac70-cccccccccc05",
            "endTime": "2018-03-05T02:02:02Z",
            "status": "passed",
            "controls": {
              "total": 16,
              "passed": {
                "total": 2
              },
              "skipped": {
                "total": 14
              },
              "failed": {}
            }
          }
        }
      ],
      "total": 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Node details API
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '74a54a28-c628-4f82-86df-centosssssss')
    expected_node = {
      "id": "74a54a28-c628-4f82-86df-centosssssss",
      "name": "centos(2)-beta-nginx(p)-apache(s)-passed",
      "platform": {
        "name": "centos",
        "release": "5.11"
      },
      "environment": "DevSec Prod Beta",
      "latestReport": {
        "id": "bb93e1b2-36d6-439e-ac70-cccccccccc05",
        "endTime": "2018-03-05T02:02:02Z",
        "status": "passed",
        "controls": {
          "total": 16,
          "passed": {
            "total": 2
          },
          "skipped": {
            "total": 14
          },
          "failed": {}
        }
      },
      "profiles": [
        {
          "name": "nginx-baseline",
          "version": "2.1.0",
          "id": "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
          "status": "passed"
        },
        {
          "name": "apache-baseline",
          "version": "2.0.1",
          "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
          "status": "skipped"
        }
      ]
    }.to_json
    assert_equal_json_sorted(expected_node, actual_node.to_json)

    END_OF_DAY = "23:59:59Z"
    # Get "two" profiles page 1
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2018-03-04T#{END_OF_DAY}"])
      ],
      page: 1, per_page: 2)
    expected_data = {
      "profiles": [
        {
          "name": "apache-baseline",
          "title": "DevSec Apache Baseline",
          "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
          "version": "2.0.1",
          "status": "skipped"
        },
        {
          "name": "nginx-baseline",
          "title": "DevSec Nginx Baseline",
          "id": "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
          "version": "2.1.0",
          "status": "passed"
        }
      ],
      "counts": {
        "total": 2,
        "skipped": 1,
        "passed": 1
      }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)

    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control', text: 'icMP'
    )
    expected = [
      "ICMP ratemask--sysctl-06--",
      "ICMP ratelimit--sysctl-05--",
      "ICMP echo ignore broadcasts--sysctl-04--",
      "ICMP ignore bogus error responses--sysctl-03--" ]
    assert_suggestions_text_id_version(expected, actual_data)


    # Get a specific report
    res = GRPC reporting, :read_report, Reporting::Query.new(id: 'bb93e1b2-36d6-439e-ac70-cccccccccc05')
    assert_equal(Reporting::Report, res.class)

    # in case you need to update the expected_json, print it out with the line below
    #puts actual_data.to_json

    assert_equal('2.1.10', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc05', res['id'])
    assert_equal('74a54a28-c628-4f82-86df-centosssssss', res['node_id'])
    assert_equal('centos(2)-beta-nginx(p)-apache(s)-passed', res['node_name'])
    assert_equal('DevSec Prod Beta', res['environment'])
    assert_equal('passed', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520215322, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11"), res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 4.109065055847168), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    if res['profiles'].is_a?(Google::Protobuf::RepeatedField)
      assert_equal(2, res['profiles'].length)
      assert_equal('nginx-baseline', res['profiles'][0]['name'])
      assert_equal(2, res['profiles'][0]['controls'].length)

      control_ids = res['profiles'][0]['controls'].map {|c| c.id}
      assert_equal(["nginx-01", "nginx-02"], control_ids)

      assert_equal(Reporting::Report, res.class)

      assert_equal('nginx-01', res['profiles'][0]['controls'][0]['id'])
      assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
      assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
      assert_equal('Running worker process as non-privileged user', res['profiles'][0]['controls'][0]['title'])
      assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)

      passed_control = res['profiles'][0]['controls'][0]['results'].first
      assert_equal('passed', passed_control['status'])

      # assert_equal('skipped', res['profiles'][0]['depends'][0]['status'])
      # assert_equal("Skipping profile: 'myprofile1' on unsupported platform: 'mac_os_x/17.7.0'.", res['profiles'][0]['depends'][0]['skip_message'])

      assert_equal('skipped', res['profiles'][1]['status'])
      assert_equal('', res['profiles'][1]['skip_message'])
    end

  end
end
