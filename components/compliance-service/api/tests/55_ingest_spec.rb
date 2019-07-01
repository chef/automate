require_relative 'test_support'
##### GRPC SETUP #####
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/jobs/jobs_pb'
require 'api/jobs/jobs_services_pb'
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Jobs = Chef::Automate::Domain::Compliance::Api::Jobs unless defined?(Jobs)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

  it "works" do
    reporting = Reporting::ReportingService
    nodes = Nodes::NodesService
    jobs = Jobs::JobsService

    rand1000 = rand(1000)

    # These ifs are used only when developing/troubleshooting the tests to avoid
    # having to create a node and wait for a job to complete
    job_id = ENV['JOB_ID'].to_s
    if job_id == ''
      if ENV['NODE_ID'].to_s == ''
        # Add a docker node to be able to reference it in the job
        node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
          name:"My cc_pg Docker Node #{rand1000}",
          manager:"automate",
          target_config: Nodes::TargetConfig.new(
            backend:"docker",
            host:"cc_pg",
            sudo: false
          ),
          tags: [
            Common::Kv.new( key:"environment", value:"trouble" ),
            Common::Kv.new( key:"group", value:"makers" ),
            Common::Kv.new( key:"_no_auto_detect", value:"true" )
          ]
        )
        docker_node_id1 = node['id']
      else
        docker_node_id1 = ENV['NODE_ID']
      end

      # Add an exec job that should succeed
      job = GRPC jobs, :create, Jobs::Job.new(
        name: "My Exec Job For Existing node",
        tags: [],
        type: "exec",
        nodes: [docker_node_id1],
        profiles: ["https://github.com/dev-sec/apache-baseline/archive/2.0.2.tar.gz"],
        retries: 1,
        node_selectors: []
      )
      job_id = job['id']
    end

    i = 0
    job_status = 'new'
    report_id = 'unknown'
    while job_status != "completed" do
      job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id)
      job_status = job1['status']
      if i > 30
        assert_equal('completed', job_status)
      end
      sleep 5
      i += 1
    end

    report_id = job1['results'][0]['report_id']
    node_id = job1['results'][0]['node_id']

    # Get job details
    job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id)

    # Wait for a bit for the report to be pushed to ElasticSearched
    sleep 3

    # Filtering by job_id as this is leading to a search in the timeseries indices
    act_rep_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'job_id', values: [job_id])
    ])

    sleep_count = 0
    while act_rep_nodes.nodes.size == 0 do
      puts "waiting for node to land in elasticsearch"
      sleep 1
      sleep_count += 1
      act_rep_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: [job_id])])
      break if sleep_count >= 30
    end
    assert_equal(1, act_rep_nodes.nodes.size)

    act_rep_nodes['nodes'][0]['latest_report']['end_time']['seconds'] = 1
    exp_rep_nodes = {
      "nodes": [
        {
          "id": node_id,
          "name": "My cc_pg Docker Node #{rand1000}",
          "platform": {
            "name": "debian",
            "release": "9.7"
          },
          "environment": "trouble",
          "latestReport": {
            "id": report_id,
            "endTime": "1970-01-01T00:00:01Z",
            "status": "skipped",
            "controls": {
              "total": 14,
              "passed": {},
              "skipped": {
                "total": 14
              },
              "failed": {}
            }
          },
          "profiles": [
            {
              "id": "3e1310b071dc4d706263e9d07083e10a92b4b69e4a36cffa1eda7eaecc09969a",
              "name": "apache-baseline",
              "status": "skipped",
              "version": "2.0.2"
            }
          ]
        }
      ],
      "total": 1
    }.to_json
    assert_equal_json_sorted(exp_rep_nodes, act_rep_nodes.to_json)

    # Filtering by node_id as this is leading to a search in the latest index
    act_rep_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'node_id', values: [node_id])
    ])
    act_rep_nodes['nodes'][0]['latest_report']['end_time']['seconds'] = 1
    assert_equal(1, act_rep_nodes.nodes.size)
    assert_equal_json_sorted(exp_rep_nodes, act_rep_nodes.to_json)


    # Get profiles used by the node with node_id. This will hit the profiles index
    act_rep_profiles = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'node_id', values: [node_id])
    ])
    exp_rep_profiles = {
      "profiles": [
        {
          "name": "apache-baseline",
          "title": "DevSec Apache Baseline",
          "id": "3e1310b071dc4d706263e9d07083e10a92b4b69e4a36cffa1eda7eaecc09969a",
          "version": "2.0.2",
          "status": "skipped"
        }
      ],
      "counts": {
        "total": 1,
        "skipped": 1
      }
    }.to_json
    assert_equal_json_sorted(exp_rep_profiles, act_rep_profiles.to_json)

    # that node should have recent last contact time, so we expect more than 0 nodes here
    formatted_one_hour_ago_utc = (Time.now.utc - 3600).iso8601
    formatted_now_time_utc = (Time.now.utc + 1).iso8601
    nodes_with_recent_last_contact = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "last_contact", values: [formatted_one_hour_ago_utc, formatted_now_time_utc])])
    for node in nodes_with_recent_last_contact.nodes
      epoch_micros = node.last_contact.nanos / 10 ** 6
      node_contact_time = Time.at(node.last_contact.seconds, epoch_micros).utc.iso8601
      assert_equal(true, node_contact_time > formatted_one_hour_ago_utc)
      assert_equal(true, node_contact_time < formatted_now_time_utc)
    end
    assert_equal(true, nodes_with_recent_last_contact.total > 0)
  end
end
