require_relative 'test_support'
##### GRPC SETUP #####
require 'interservice/nodemanager/nodes/nodes_pb'
require 'interservice/nodemanager/nodes/nodes_services_pb'
require 'interservice/compliance/jobs/jobs_pb'
require 'interservice/compliance/jobs/jobs_services_pb'
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)
  Nodes = Chef::Automate::Domain::Nodemanager::Nodes unless defined?(Nodes)
  Jobs = Chef::Automate::Domain::Compliance::Jobs unless defined?(Jobs)
  Common = Chef::Automate::Domain::Compliance::Common unless defined?(Common)

  reporting = Reporting::ReportingService
  nodes = Nodes::NodesService
  jobs = Jobs::JobsService

  @rand1000 = rand(1000)
  @job_id = ''


  before(:all) do
    # These ifs are used only when developing/troubleshooting the tests to avoid
    # having to create a node and wait for a job to complete
    job_id = ENV['JOB_ID'].to_s
    if job_id == ''
      if ENV['NODE_ID'].to_s == ''
        # Add a docker node to be able to reference it in the job
        node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
          name:"My cc_pg Docker Node #{@rand1000}",
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
        profiles: ["https://github.com/dev-sec/ssl-baseline/archive/1.4.0.tar.gz"],
        retries: 1,
        node_selectors: []
      )
      @job_id = job['id']
    end

    i = 0
    job_status = 'new'
    report_id = 'unknown'
    while job_status != "completed" do
      job1 = GRPC jobs, :read, Jobs::Id.new(id: @job_id)
      job_status = job1['status']
      if i > 40
        assert_equal('completed', job_status)
      end
      puts "sleeping 5 sec, counter is: ", i
      sleep 5
      i += 1
    end
    @report_id = job1['results'][0]['report_id']
    @node_id = job1['results'][0]['node_id']

    @exp_rep_nodes = {
      "nodes": [
        {
          "id": @node_id,
          "name": "My cc_pg Docker Node #{@rand1000}",
          "platform": {
            "name": "debian",
            "release": "9.7",
            "full": "debian 9.7"
          },
          "environment": "trouble",
          "latestReport": {
            "id": @report_id,
            "endTime": "1970-01-01T00:00:01Z",
            "status": "passed",
            "controls": {
              "total": 33,
              "passed": {
                "total": 1
              },
              "skipped": {
                "total": 32
              },
              "failed": {},
              "waived": {}
            }
          },
          "profiles": [
            {
              "id": "a9e6a3c330193aa1396939ae1ec277024f4e7df5337852616fc5bc0bdc746a84",
              "name": "ssl-baseline",
              "status": "passed",
              "version": "1.4.0",
              "full": "DevSec SSL/TLS Baseline, v1.4.0"
            }
          ]
        }
      ],
      "total": 1,
      "totalPassed"=>1
      }.to_json
  end


it "listing nodes with a job id filter returns the expected node" do

    # Filtering by job_id as this is leading to a search in the timeseries indices
    act_rep_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'job_id', values: [@job_id])
    ])

    sleep_count = 0
    while act_rep_nodes.nodes.size == 0 do
      puts "sleeping 5, waiting for node to land in elasticsearch. count: ", sleep_count
      sleep 5
      sleep_count += 1
      act_rep_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
        Reporting::ListFilter.new(type: 'job_id', values: [@job_id])])
      break if sleep_count >= 30
    end
    assert_equal(1, act_rep_nodes.nodes.size)

    act_rep_nodes['nodes'][0]['latest_report']['end_time']['seconds'] = 1
    assert_equal_json_sorted(@exp_rep_nodes, act_rep_nodes.to_json)
  end

  it "filtering the reporting nodes by node id works too" do
    # Filtering by node_id as this is leading to a search in the latest index
    act_rep_nodes = GRPC reporting, :list_nodes, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'node_id', values: [@node_id])
    ])
    act_rep_nodes['nodes'][0]['latest_report']['end_time']['seconds'] = 1
    assert_equal(1, act_rep_nodes.nodes.size)
    assert_equal_json_sorted(@exp_rep_nodes, act_rep_nodes.to_json)
  end

  it "calling reporting profiles returns expected profile from scan job" do
      # Get profiles used by the node with node_id. This will hit the profiles index
    act_rep_profiles = GRPC reporting, :list_profiles, Reporting::Query.new(filters: [
      Reporting::ListFilter.new(type: 'node_id', values: [@node_id])
    ])
    exp_rep_profiles = {
      "profiles": [
        {
          "name": "ssl-baseline",
          "title": "DevSec SSL/TLS Baseline",
          "id": "a9e6a3c330193aa1396939ae1ec277024f4e7df5337852616fc5bc0bdc746a84",
          "version": "1.4.0",
          "status": "passed"
        }
      ],
      "counts": {
        "total": 1,
        "passed": 1
      }
    }.to_json
    assert_equal_json_sorted(exp_rep_profiles, act_rep_profiles.to_json)
  end

  it "the nodemanager knows about the node too" do
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
