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
      Reporting::ListFilter.new(type: 'start_time', values: ['2018-04-16T00:00:01Z']), # start_time is ignored for this call. Only end_time is used
      Reporting::ListFilter.new(type: 'end_time', values: ['2018-04-16T23:59:59Z'])
    ], sort: 'name')
    assert_equal([ "127.0.0.1", "localhost" ],
                 resp['nodes'].map{ |x| x['name'] }
    )


    # Get all nodes, sorted by latest_report(default), asc(default) order
    # Hits only daily index 2018-03-05
    actual_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'end_time', values: ['2017-06-29T23:59:59Z'])
    ])
    expected_nodes = {
      "nodes": [
        {
          "id": "6dd7c19c-c1bc-4cf5-b431-524e3e42f551",
          "name": "centos6-node1",
          "platform": {
            "name": "mac_os_x",
            "release": "10.12.6"
          },
          "environment": "_default",
          "latestReport": {
            "id": "491285be-b513-4f8a-ad05-295829243b51",
            "endTime": "2017-06-29T10:39:25Z",
            "status": "passed",
            "controls": {
              "total": 2,
              "passed": {
                "total": 2
              },
              "skipped": {},
              "failed": {}
            }
          }
        }
      ],
      "total": 1
    }.to_json
    assert_equal_json_sorted(expected_nodes, actual_nodes.to_json)

    # Node details API
    actual_node = GRPC reporting, :read_node, Reporting::Id.new(id: '6dd7c19c-c1bc-4cf5-b431-524e3e42f551')
    expected_node = {
      "id": "6dd7c19c-c1bc-4cf5-b431-524e3e42f551",
      "name": "centos6-node1",
      "platform": {
        "name": "mac_os_x",
        "release": "10.12.6"
      },
      "environment": "_default",
      "latestReport": {
        "id": "491285be-b513-4f8a-ad05-295829243b52",
        "endTime": "2017-07-29T10:39:25Z",
        "status": "passed",
        "controls": {
          "total": 2,
          "passed": {
            "total": 2
          },
          "skipped": {},
          "failed": {}
        }
      },
      "profiles": [
        {
          "name": "mylinux-success",
          "version": "1.0.0",
          "id": "d94022291cae7c87bb13d42bbd0dbc86b027c1e32fdc6790d2555d62deb8b6a9"
        }
      ]
    }.to_json
    assert_equal_json_sorted(expected_node, actual_node.to_json)

    END_OF_DAY = "23:59:59Z"
    # Get "two" profiles page 1
    actual_data = GRPC reporting, :list_profiles, Reporting::Query.new(
      filters: [
        Reporting::ListFilter.new(type: "end_time", values: ["2017-06-29T#{END_OF_DAY}"])
      ],
      page: 1, per_page: 2)
    expected_data = {
      "profiles": [
        {
          "name": "mylinux-success",
          "title": "My Demo Linux successful profile",
          "id": "d94022291cae7c87bb13d42bbd0dbc86b027c1e32fdc6790d2555d62deb8b6a9",
          "version": "1.0.0",
          "status": "passed"
        }
      ],
      "counts": {
        "total": 1,
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
    actual_data = GRPC reporting, :read_report, Reporting::Query.new(id: '491285be-b513-4f8a-ad05-295829243b52')
    assert_equal(Reporting::Report, actual_data.class)

    # in case you need to update the expected_json, print it out with the line below
    # puts actual_data.to_json

    expected_json = {
      "id": "491285be-b513-4f8a-ad05-295829243b52",
      "nodeId": "6dd7c19c-c1bc-4cf5-b431-524e3e42f551",
      "nodeName": "centos6-node1",
      "endTime": "2017-07-29T10:39:25Z",
      "status": "passed",
      "environment": "_default",
      "version": "1.39.1",
      "platform": {
        "name": "mac_os_x",
        "release": "10.12.6"
      },
      "statistics": {
        "duration": 0.002533
      },
      "profiles": [
        {
          "name": "mylinux-success",
          "title": "My Demo Linux successful profile",
          "copyright": "Chef Software, Inc.",
          "copyrightEmail": "support@chef.io",
          "summary": "Demonstrates the use of InSpec Compliance Profile",
          "version": "1.0.0",
          "sha256": "d94022291cae7c87bb13d42bbd0dbc86b027c1e32fdc6790d2555d62deb8b6a9",
          "controls": [
            {
              "id": "/etc/group must exist",
              "code": "control '/etc/group must exist' do\n  impact 0.3\n  describe file('/etc/group') do\n    it { should be_file }\n  end\nend\n",
              "impact": 0.30000001,
              "sourceLocation": {
                "ref": "controls/success.rb",
                "line": 9
              },
              "results": [
                {
                  "status": "passed",
                  "codeDesc": "File /etc/group should be file",
                  "runTime": 0.000103
                }
              ],
              "tags": {}
            },
            {
              "id": "/etc/passwd must exist",
              "code": "control '/etc/passwd must exist' do\n  impact 0.7\n  describe file('/etc/passwd') do\n    it { should be_file }\n  end\nend\n",
              "impact": 0.69999999,
              "sourceLocation": {
                "ref": "controls/success.rb",
                "line": 2
              },
              "results": [
                {
                  "status": "passed",
                  "codeDesc": "File /etc/passwd should be file",
                  "runTime": 0.001871
                }
              ],
              "tags": {}
            }
          ],
          "status": "passed"
        }
      ]
    }.to_json
    assert_equal_json_sorted(expected_json, actual_data.to_json)

  end
end
