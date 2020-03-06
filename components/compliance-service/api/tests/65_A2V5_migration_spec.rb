##### GRPC SETUP #####
require 'api/interservice/compliance/reporting/reporting_pb'
require 'api/interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)

  reporting = Reporting::ReportingService

  it "ListNodes: sort by node name" do

    # sort by node name now
    resp = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-02T00:00:01Z']), # start_time is ignored for this call. Only end_time is used
      Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z'])
    ], sort: 'name')
    assert_equal(["centos(3)-beta-nginx(p)-apache(s)-fake(s)-passed",
                  "redhat(2)-alpha-nginx(f)-apache(s)-failed",
                  "redhat(2)-beta-nginx(f)-apache(s)-failed",
                  "windows(1)-zeta-apache(s)-skipped"],
                 resp['nodes'].map{ |x| x['name'] }
    )
  end

  it "ListNodes: defaults" do
    # Get all nodes, sorted by latest_report(default), asc(default) order
    # Hits only daily index 2018-03-05
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-05T23:59:59Z'])
    ])
    expected_nodes = {
      "nodes": [
        {
          "id": "9b9f4e51-b049-4b10-9555-10578916e149",
          "name": "centos(2)-beta-nginx-apache",
          "platform": {
            "name": "centos",
            "release": "5.11",
            "full": "centos 5.11"
          },
          "environment": "DevSec Prod Beta",
          "latestReport": {
            "id": "bb93e1b2-36d6-439e-ac70-cccccccccc05",
            "endTime": "2018-03-05T02:02:02Z",
            "status": "passed",
            "controls": {
              "total": 18,
              "passed": {
                "total": 3
              },
              "skipped": {
                "total": 15
              },
              "failed": {},
              "waived": {}
            }
          },
          "profiles": [
            {
              "name": "nginx-baseline",
              "version": "2.1.0",
              "id": "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
              "status": "passed",
              "full": "DevSec Nginx Baseline, v2.1.0"
            },
            {
              "name": "apache-baseline",
              "version": "2.0.1",
              "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
              "status": "skipped",
              "full": "DevSec Apache Baseline, v2.0.1"
            }
          ]
        }
      ],
      "total": 1,
      "totalPassed"=>1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)
  end

  it "ReadNode" do
    # Node details API
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '9b9f4e51-b049-4b10-9555-10578916e149')
    expected_node = {
      "id": "9b9f4e51-b049-4b10-9555-10578916e149",
      "name": "centos(2)-beta-nginx-apache",
      "platform": {
        "name": "centos",
        "release": "5.11",
        "full": "centos 5.11"
      },
      "environment": "DevSec Prod Beta",
      "latestReport": {
        "id": "bb93e1b2-36d6-439e-ac70-cccccccccc07",
        "endTime": "2018-03-05T07:02:02Z",
        "status": "failed",
        "controls": {
          "total": 18,
          "passed": {
            "total": 3
          },
          "skipped": {
            "total": 13
          },
          "failed": {
            "total": 2,
            "major": 2
          },
          "waived": {}
        }
      },
      "profiles": [
        {
          "name": "nginx-baseline",
          "version": "2.1.0",
          "id": "09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988",
          "status": "failed",
          "full": "DevSec Nginx Baseline, v2.1.0"
        },
        {
          "name": "apache-baseline",
          "version": "2.0.1",
          "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
          "status": "failed",
          "full": "DevSec Apache Baseline, v2.0.1"
        }
      ]
    }.to_json
    assert_equal_json_sorted(expected_node, actual_node.to_json)
  end

  it "ListProfiles: Get two profiles" do
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
          "name": "fake-baseline",
          "title": "A fake one",
          "id": "41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
          "version": "2.0.1",
          "status": "skipped"
        },
        {
          "name": "apache-baseline",
          "title": "DevSec Apache Baseline",
          "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
          "version": "2.0.1",
          "status": "skipped"
        }
      ],
      "counts": {
        "total": 3,
        "failed": 1,
        "skipped": 2
      }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end

  it "ListProfiles: filter by control" do
    # Search profiles and filter by control. Should not show profiles that ran on a node without containing the control
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'control', values: ['sysctl-06']),
        Reporting::ListFilter.new(type: "end_time", values: ["2018-02-09T#{END_OF_DAY}"])
    ])
    #TODO - the profile (apache-baseline) below should not be here. it is because we get info based on report..but this
    # should only include profiles that contain the given control
    expected_data = {
      "profiles": [
        {
          "name": "apache-baseline",
          "title": "DevSec Apache Baseline",
          "id": "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8",
          "version": "2.0.0",
          "status": "passed"
        },
        {
          "name": "linux-baseline",
          "title": "DevSec Linux Security Baseline",
          "id": "b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015",
          "version": "2.0.1",
          "status": "failed"
        }
      ],
      "counts": {
        "total": 2,
        "failed": 1,
        "passed": 1
      }
    }.to_json
    assert_equal_json_sorted(expected_data, actual_data.to_json)
  end

  it "ListSuggestions: controls 'icMP' with filter by profile ID" do
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control',
      text: 'icMP',
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015'])
      ]
    )
    expected = [
      "ICMP ratemask--sysctl-06--",
      "ICMP ratelimit--sysctl-05--",
      "ICMP echo ignore broadcasts--sysctl-04--",
      "ICMP ignore bogus error responses--sysctl-03--" ]
    assert_suggestions_text_id_version(expected, actual_data)
  end


  it "ListSuggestions: controls 'sys' with filter by profile ID" do
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control',
      text: 'sys',
      filters: [
        Reporting::ListFilter.new(type: 'profile_id', values: ['b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015'])
      ]
    )
    expected = [
      "Magic SysRq--sysctl-30--",
      "Disable the system`s acceptance of router advertisement--sysctl-25--",
      "Protection against SYN flood attacks--sysctl-11--",
      "Disable Apache’s follows Symbolic Links for directories in alias.conf--apache-11--"]
    assert_suggestions_text_id_version(expected, actual_data)
  end

  it "ListSuggestions: profile with filter by controls" do
    # suggest profiles with control filters. Should not show profiles that ran on a node without containing the control
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'profile',
      filters: [
        Reporting::ListFilter.new(type: 'control', values: ['sysctl-06','missing-one'])
      ]
    )
    expected = ["DevSec Apache Baseline--41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8--2.0.0",
                "DevSec Linux Security Baseline--b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015--2.0.1"]
    assert_suggestions_text_id_version(expected, actual_data)
  end

  it "ReadReport" do
    res = GRPC reporting, :read_report, Reporting::Query.new(id: 'bb93e1b2-36d6-439e-ac70-cccccccccc07')
    assert_equal(Reporting::Report, res.class)

    assert_equal('3.1.3', res['version'])
    assert_equal('bb93e1b2-36d6-439e-ac70-cccccccccc07', res['id'])
    assert_equal('9b9f4e51-b049-4b10-9555-10578916e149', res['node_id'])
    assert_equal('centos(2)-beta-nginx-apache', res['node_name'])
    assert_equal('DevSec Prod Beta', res['environment'])
    assert_equal('failed', res['status'])
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1520233322, nanos: 0), res['end_time'])
    assert_equal(Reporting::Platform.new(name: "centos", release: "5.11", full: "centos 5.11"),
      res['platform'])
    assert_equal(Reporting::Statistics.new(duration: 4.109065055847168), res['statistics'])
    assert_equal(Google::Protobuf::RepeatedField, res['profiles'].class)
    assert_equal(2, res['profiles'].length)
    assert_equal('nginx-baseline', res['profiles'][0]['name'])
    assert_equal('DevSec Nginx Baseline, v2.1.0', res['profiles'][0]['full'])
    assert_equal(4, res['profiles'][0]['controls'].length)

    control_ids = res['profiles'][0]['controls'].map {|c| c.id}
    assert_equal(["nginx-01", "nginx-02", "nginx-03", "nginx-04"], control_ids)

    assert_equal(Reporting::Report, res.class)

    assert_equal('nginx-01', res['profiles'][0]['controls'][0]['id'])
    assert_equal(1, res['profiles'][0]['controls'][0]['impact'])
    assert_equal(Google::Protobuf::Map, res['profiles'][0]['controls'][0]['tags'].class)
    assert_equal('Running worker process as non-privileged user', res['profiles'][0]['controls'][0]['title'])
    assert_equal(1, res['profiles'][0]['controls'][0]['results'].length)
    assert_equal('no', res['profiles'][0]['controls'][0]['waived_str'])

    passed_control = res['profiles'][0]['controls'][0]['results'].first
    assert_equal('passed', passed_control['status'])

    assert_equal('failed', res['profiles'][1]['status'])
    assert_equal('', res['profiles'][1]['skip_message'])
  end
end
