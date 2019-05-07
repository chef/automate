require_relative 'test_support'
##### GRPC SETUP #####
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/external/secrets/secrets_services_pb'
require 'api/jobs/jobs_pb'
require 'api/jobs/jobs_services_pb'
require 'api/manager/manager_pb'
require 'api/manager/manager_services_pb'

describe File.basename(__FILE__) do
  Manager = Chef::Automate::Domain::Nodemanager::Api::Manager unless defined?(Manager)
  Jobs = Chef::Automate::Domain::Compliance::Api::Jobs unless defined?(Jobs)
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

  def manager ; Manager::NodeManagerService ; end
  def jobs ; Jobs::JobsService ; end
  def nodes ; Nodes::NodesService ; end
  def secrets ; Secrets::SecretsService ; end

  def cleanup
    return if ENV['NO_DELETE']
    GRPC(jobs, :list, Jobs::Query.new())['jobs'].each do |j|
      GRPC jobs, :delete, Jobs::Id.new(id: j.id)
    end

    MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes'].each do |s|
      MANAGER_GRPC nodes, :delete, Nodes::Id.new(id: s.id)
    end

    SS_GRPC(secrets, :list, Secrets::Query.new())['secrets'].each do |s|
      SS_GRPC secrets, :delete, Secrets::Id.new(id: s.id)
    end
  end

  before(:all) { cleanup }
  after(:all) { cleanup }

  before(:all) {
    secret = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new( key: "username", value: "bobby" ),
        Secrets::Kv.new( key: "password", value: "b055y-th3_w1nn3r" )
      ],
      name: "betasec",
      tags: [],
      type: ""
    )

    # Add a docker node to be able to reference it in the job
    # include a _no_auto_detect tag on the node to ensure the scan job
    # updates the platform information for the node
    node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "My Existing Docker Node",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "docker",
        host: "cc_pg",
        sudo: false,
        secrets: [secret['id']]
      ),
      tags: [
        Common::Kv.new( key: "environment", value: "trouble" ),
        Common::Kv.new( key: "group", value: "makers" ),
        Common::Kv.new( key: "group", value: "doers" ),
        Common::Kv.new( key: "_no_auto_detect", value: "true" )
      ]
    )
    @docker_node_id1 = node['id']

    # Add a docker node to be able to reference it in the job
    node2 = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "My Missing Docker Node",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "docker",
        host: "cc_pggggggggg",
        secrets: [secret['id']]
      ),
      tags: [
        Common::Kv.new( key: "environment", value: "trouble" )
      ]
    )
    @docker_node_id2 = node2['id']
  }

  it "returns an error message when fetching a job with a nonexistant id" do
    assert_grpc_error("Not found for id: missing", 5) do
      GRPC jobs, :read, Jobs::Id.new(id: 'missing')
    end
  end

  it "returns an error message listing jobs with an invalid sort key" do
    assert_grpc_error("Invalid sort field, valid ones are: [end_time name start_time status type]", 3) do
      GRPC jobs, :list, Jobs::Query.new(sort: "wrong")
    end
  end

  it "returns an error message when creating a job with no nodes or node selectors" do
    assert_grpc_error("Invalid job: nodes or node selectors required.", 3) do
      GRPC jobs, :create, Jobs::Job.new(
        name: "job job",
        tags: [],
        type: "detect",
        nodes: [],
        retries: 1,
        node_selectors: []
      )
    end
  end

  it "returns an error message when creating a job with no name" do
    assert_grpc_error("Invalid job, \'name\' is a required parameter", 3) do
      GRPC jobs, :create, Jobs::Job.new(
        name: "",
        tags: [],
        type: "detect",
        nodes: [@docker_node_id2],
        retries: 1,
        node_selectors: []
      )
    end
  end

  it "works" do
    test_start_time = Time.now.utc - 30
    puts "30_jobs_docker_spec start time #{test_start_time}"
    puts "30_jobs_docker_spec start time (actual time now - 30s)  #{test_start_time}"

    # Add a job with tags and name(lower case 'my' to test case insensitive sorting by name)
    job1 = GRPC jobs, :create, Jobs::Job.new(
      name: "my Detect Job For two nodes",
      tags: [
        Common::Kv.new( key: "reason", value: "checking the connectivity" ),
        Common::Kv.new( key: "team", value: "Spain" )
      ],
      type: "detect",
      nodes: [@docker_node_id1, @docker_node_id2],
      retries: 1,
      node_selectors: [],
      parent_id: "123"
    )
    job_id1 = job1['id']
    assert_uuid(job_id1)

    # Test job update
    assert_grpc_error("Invalid job. Child jobs may not be updated. If you wish to update the parent job, please find job: 123", 3) do
      GRPC jobs, :update, Jobs::Job.new(
        id: job_id1,
        name: "my Detect Job For two nodes",
        tags: [
          Common::Kv.new( key: "reason", value: "checking the connectivity" ),
          Common::Kv.new( key: "team", value: "Spain" )
        ],
        type: "detect",
        nodes: [@docker_node_id1, @docker_node_id2],
        retries: 1,
        node_selectors: [],
        parent_id: "123"
      )
    end

    # Add an exec job that should succeed
    job2 = GRPC jobs, :create, Jobs::Job.new(
      name: "My Exec Job For Existing node",
      tags: [],
      type: "exec",
      nodes: [@docker_node_id1],
      profiles: ["https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"],
      retries: 1,
      node_selectors: []
    )
    job_id2 = job2['id']
    assert_uuid(job_id2)

    sleep 1 # time for the status to turn running

    # Get all jobs
    all_jobs = GRPC jobs, :list, Jobs::Query.new()

    all_jobs['jobs'].each {|j|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, j, 'start_time'))
      j['status'] = 'good' if j['status'] == 'running' || j['status'] == 'scheduled'
    }
    expected_jobs = [
      Jobs::Job.new(
        end_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
        id: job_id1,
        name: "my Detect Job For two nodes",
        node_count: 2,
        parent_id: "123",
        start_time: nil,
        status: "good",
        tags: [
          Common::Kv.new( key: "reason", value: "checking the connectivity" ),
          Common::Kv.new( key: "team", value: "Spain" )
        ],
        type: "detect",
        scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
        recurrence: ""
      ),
      Jobs::Job.new(
        end_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
        id: job_id2,
        name: "My Exec Job For Existing node",
        node_count: 1,
        profile_count: 1,
        start_time: nil,
        status: "good",
        type: "exec",
        scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0)
      )
    ]
    assert_equal(expected_jobs, all_jobs['jobs'])

    # Get job by id with all details
    job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job1, 'start_time'))
    job1['status'] = 'good' if job1['status'] == 'running' || job1['status'] == 'scheduled'
    expected_job1 = Jobs::Job.new(
      end_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
      id: job_id1,
      name: "my Detect Job For two nodes",
      node_count: 2,
      nodes: [@docker_node_id1, @docker_node_id2],
      parent_id: "123",
      retries: 1,
      retries_left: 1,
      start_time: nil,
      status: "good",
      tags: [
        Common::Kv.new( key: "reason", value: "checking the connectivity" ),
        Common::Kv.new( key: "team", value: "Spain" )
      ],
      scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
      timeout: 600,
      type: "detect"
    )
    assert_equal(expected_job1, job1)

    # Get job by id with all details
    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job2, 'start_time'))
    job2['status'] = 'good' if job2['status'] == 'running' || job2['status'] == 'scheduled'
    expected_job2 = Jobs::Job.new(
      end_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
      id: job_id2,
      name: "My Exec Job For Existing node",
      node_count: 1,
      nodes: [@docker_node_id1],
      profiles: ["https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"],
      retries: 1,
      retries_left: 1,
      start_time: nil,
      status: "good",
      timeout: 7200,
      type: "exec",
      scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0)
    )
    assert_equal(expected_job2, job2)

    # give the jobs some time to reach a conclusion
    job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)

    sleep_count = 1
    while job1.status == "scheduled" || job1.status == "running" || job2.status == "scheduled" || job2.status == "running" do
      puts "> #{sleep_count} < waiting for jobs to conclude, current status job1 (#{job1.status}), job2 (#{job2.status})"
      sleep 3
      sleep_count += 1
      job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
      job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
      break if sleep_count >= 100
    end
    puts "> #{sleep_count} < while exit with statuses: job1 (#{job1.status}), job2 (#{job2.status})"
    assert_equal("completed", job2.status)

    # Get job by id with all details
    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job2, 'start_time'))
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job2, 'end_time'))
    job2['results'].each {|result|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'start_time'))
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'end_time'))
      assert_uuid(result['report_id'])
      result['report_id'] = 'some-report-id'
    }
    expected_job2 = Jobs::Job.new(
      end_time: nil,
      id: job_id2,
      name: "My Exec Job For Existing node",
      node_count: 1,
      nodes: [@docker_node_id1],
      profiles: ["https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"],
      results: [ Jobs::ResultsRow.new(
        end_time: nil,
        node_id: @docker_node_id1,
        report_id: "some-report-id",
        start_time: nil,
        status: "completed"
      )],
      retries: 1,
      retries_left: 1,
      start_time: nil,
      status: "completed",
      timeout: 7200,
      type: "exec",
      scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0)
    )
    assert_equal(expected_job2, job2)

    # Get all jobs after they had time to finish
    all_jobs = GRPC jobs, :list, Jobs::Query.new()
    all_jobs['jobs'].each {|j|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, j, 'start_time'))
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, j, 'end_time'))
    }
    actual_jobs = all_jobs['jobs']
    expected_jobs = [
      Jobs::Job.new(
        end_time: nil,
        id: job_id1,
        name: "my Detect Job For two nodes",
        node_count: 2,
        parent_id: "123",
        start_time: nil,
        status: "failed",
        tags: [
          Common::Kv.new( key: "reason", value: "checking the connectivity" ),
          Common::Kv.new( key: "team", value: "Spain" )
        ],
        type: "detect",
        scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0)
      ),
      Jobs::Job.new(
        end_time: nil,
        id: job_id2,
        name: "My Exec Job For Existing node",
        node_count: 1,
        profile_count: 1,
        start_time: nil,
        status: "completed",
        type: "exec",
        scheduled_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0)
      )
    ]
    assert_equal(expected_jobs, actual_jobs)

    # Getting job2 details to grab the end_time before rerunning the job
    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
    job2_orig_end_time = job2.end_time.seconds\
    print "\n * Rerunning job: #{job2.id}\n"
    # Test rerun for job2
    GRPC jobs, :rerun, Jobs::Id.new(id: job_id2)
    rerun_job = GRPC jobs, :read, Jobs::Id.new(id: job_id2)

    sleep_count = 1
    # Status will be completed initially from the previous run. Then it turns running and then completed again with a new end_time
    while rerun_job.status != "completed" || job2_orig_end_time == rerun_job.end_time.seconds do
      puts "> #{sleep_count} < waiting for rerun job status 'completed', current status '#{rerun_job.status}'"
      sleep 3
      sleep_count += 1
      rerun_job = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
      break if sleep_count >= 50
    end
    puts "> #{sleep_count} < while exit with rerun job status '#{rerun_job.status}'"
    assert_equal("completed", rerun_job.status)
    assert_equal(false, job2_orig_end_time == rerun_job.end_time.seconds)
    assert_equal(2, rerun_job.results.length)

    # Get all nodes
    actual_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    actual_nodes['nodes'].each {|n|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, n, 'last_contact'))
      n.scan_data.end_time = ""
      n.scan_data.id = "some-id"
    }
    expected_nodes = {
      "nodes": [
        {
          "id": @docker_node_id1,
          "name": "My Existing Docker Node",
          "platform": "debian",
          "platformVersion": "9.7",
          "manager": "automate",
          "tags": [
            {
              "key": "environment",
              "value": "trouble"
            },
            {
              "key": "group",
              "value": "makers"
            },
            {
              "key": "group",
              "value": "doers"
            },
            {
              "key": "_no_auto_detect",
              "value": "true"
            }
          ],
          "status": "reachable",
          "lastJob": {
            "jobId": job_id2,
            "nodeId": @docker_node_id1,
            "status": "completed",
          },
          "managerIds": [
            "e69dc612-7e67-43f2-9b19-256afd385820"
          ],
          "state": "RUNNING",
          "projects": [],
          "runData": {},
          "scanData": {
            "id": "some-id",
            "status": "passed",
            "penultimateStatus": "passed"
          }
        },
        {
          "id": @docker_node_id2,
          "name": "My Missing Docker Node",
          "manager": "automate",
          "tags": [
            {
              "key": "environment",
              "value": "trouble"
            }
          ],
          "status": "unreachable",
          "lastJob": {
            "jobId": job_id1,
            "nodeId": @docker_node_id2,
            "status": "failed",
          },
          "managerIds": [
            "e69dc612-7e67-43f2-9b19-256afd385820"
          ],
          "connectionError": "unknown error",
          "projects": [],
          "runData": {},
          "scanData": {
            "id": "some-id"
          }
        }
      ],
      "total": 2,
      "totalUnreachable": 1,
      "totalReachable": 1
    }
    assert_equal_json_content(expected_nodes, actual_nodes)

    default_jobs_json = GRPC jobs, :list, Jobs::Query.new()
    # Testing the default sorting(name ASC)
    assert_equal( ["my Detect Job For two nodes", "My Exec Job For Existing node"],
                  extract_grpc_field(default_jobs_json['jobs'], 'name') )

    # Testing jobs list sorted DESC by type
    type_jobs_json = GRPC jobs, :list, Jobs::Query.new(sort: "type", order: 1)
    assert_equal( ["My Exec Job For Existing node", "my Detect Job For two nodes"],
                  extract_grpc_field(type_jobs_json['jobs'], 'name') )

    # Testing jobs list sorted DESC by type, page 1(default), size 1
    type_jobs_json = GRPC jobs, :list, Jobs::Query.new(sort: "name", order: 1, per_page: 1)
    assert_equal(["My Exec Job For Existing node"], extract_grpc_field(type_jobs_json['jobs'], 'name'))

    # Testing jobs list sorted DESC by type, page 1, size 1
    type_jobs_json = GRPC jobs, :list, Jobs::Query.new(sort: "name", order: 1, per_page: 1, page: 2)
    assert_equal(["my Detect Job For two nodes"], extract_grpc_field(type_jobs_json['jobs'], 'name'))

    # Test update (PUT)
    GRPC jobs, :update, Jobs::Job.new(
      id: job_id2,
      name: "My Exec Job For two nodes",
      tags: [ Common::Kv.new( key: "team", value: "Canada" )],
      type: "exec",
      nodes: [@docker_node_id1, @docker_node_id2],
      profiles: ["https://github.com/dev-sec/linux-baseline/archive/master.tar.gz"],
      retries: 3,
      node_selectors: [],
      recurrence: "FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z"
    )

    sleep 1
    # Get job by id with all details
    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job2, 'start_time'))
    job2['results'].each { |result|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'start_time'))
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'end_time'))
      result['report_id'] = 'some-report-id'
    }
    job2['status'] = 'good-stuff' if job2['status'] == 'new' || job2['status'] == 'scheduled'
    assert_equal(true, job2['nodes'].include?(@docker_node_id1))
    assert_equal(true, job2['nodes'].include?(@docker_node_id2))
    job2['nodes'] = Google::Protobuf::RepeatedField.new(:string)
    updated_expected_job2 = Jobs::Job.new(
      id: job_id2,
      end_time: Google::Protobuf::Timestamp.new(seconds: -62135596800, nanos: 0),
      name: "My Exec Job For two nodes",
      nodes: [],
      profiles: ["https://github.com/dev-sec/linux-baseline/archive/master.tar.gz"],
      recurrence: "FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z",
      results: [ Jobs::ResultsRow.new(
        end_time: nil,
        node_id: @docker_node_id1,
        report_id: "some-report-id",
        start_time: nil,
        status: "completed"
      ),
      Jobs::ResultsRow.new(
        end_time: nil,
        node_id: @docker_node_id1,
        report_id: "some-report-id",
        start_time: nil,
        status: "completed"
      )],
      retries: 3,
      retries_left: 3,
      start_time:nil,
      status: "good-stuff",
      tags: [Common::Kv.new( key: "team", value: "Canada" )],
      timeout: 7200,
      type: "exec",
      scheduled_time: Google::Protobuf::Timestamp.new(seconds: 1861920000, nanos: 0)
    )
    assert_equal(updated_expected_job2, job2)

    GRPC jobs, :update, Jobs::Job.new(
      id: job_id2,
      name: "My Detect Job For one node",
      tags: [],
      type: "detect",
      nodes: [@docker_node_id1],
      profiles: [],
      retries: 3,
      node_selectors: [],
      recurrence: "FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z"
    )

    sleep 1

    # Testing jobs list filtered by job_type = 'exec' expect empty array (non found)
    actual = GRPC jobs, :list, Jobs::Query.new(
      filters: [
        Common::Filter.new(key: "job_type", values: ["exec"])
      ]
    )
    assert_equal actual, Jobs::Jobs.new(jobs: [], total: 0)

    # Testing jobs list filtered by job_type = 'bogus_job_type' expect 400 bad request
    assert_grpc_error("Invalid job_type filter: bogus_job_type. job_type must be one of the following: \'detect\' or \'exec\'", 3) do
      GRPC jobs, :list, Jobs::Query.new(
        filters: [
          Common::Filter.new(key: "job_type", values: ["bogus_job_type"])
        ]
      )
    end

    # Get node1 by id with all details
    node1 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: @docker_node_id1)
    assert_equal('reachable', node1['status'])
    last_job = node1['last_job']

    assert_equal(Nodes::ResultsRow, last_job.class)

    assert_equal('completed', last_job['status'])
    assert_equal(job2['id'], last_job['job_id'])
    assert_equal('', last_job['result'])

    # Get node2 by id with all details
    node2 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: @docker_node_id2)
    assert_equal('unreachable', node2['status'])
    last_job = node2['last_job']
    if last_job != nil
      assert_equal('failed', last_job['status'])
      assert_equal(job1['id'], last_job['job_id'])
    end

    # Get filtered nodes
    unreachable = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "status", values: ["unreachable"])])
    assert_equal(1, unreachable.total)

    reachable = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "status", values: ["reachable"])])
    assert_equal(1, reachable.total)

    debian8_10 = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [
      Common::Filter.new(key: "platform_name", values: ["debian"]),
      Common::Filter.new(key: "platform_release", values: ["9.7"])
    ])
    assert_equal(1, debian8_10.total)


    # Test manual node state change using node already created here, to take advantage of the fact
    # that the node already ran a detect job, so it has status reachable
    assert_equal("reachable", node1.status)

    # change node state to stopped, status gets automatically updated to unreachable
    MANAGER_GRPC manager, :change_node_state, Manager::NodeState.new(id: node1.id, state: 1)
    node_after_state_change = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node1.id)
    assert_equal("unreachable", node_after_state_change.status)

    # this requires another detect job to run, so not running in ci
    if ENV["NO_TIME_LIMITS"]
      MANAGER_GRPC manager, :change_node_state, Manager::NodeState.new(id: node1.id, state: 0)
      node_after_state_change = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node1.id)).status
      while node_after_state_change == "unreachable" do
        sleep 5
        node_after_state_change = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node1.id)).status
      end
      assert_equal("reachable", node_after_state_change)
    end

    # change state to terminated
    MANAGER_GRPC manager, :change_node_state, Manager::NodeState.new(id: node1.id, state: 2)

    # should not be able to update state of terminated node
    assert_grpc_error("unable to update terminated node #{node1.id}", 3) do
      MANAGER_GRPC manager, :change_node_state, Manager::NodeState.new(id: node1.id, state: 0)
    end

    terminated_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "state", values: ["TERMINATED"])])
    assert_equal(1, terminated_nodes.total)

    not_terminated_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "state", values: ["RUNNING", "STOPPED", ""])])
    assert_equal(1, not_terminated_nodes.total)

    # since that node's state was just updated, it should show up when we filter for state of terminated within this time range
    formatted_now_time_utc = (Time.now.utc + 1).iso8601
    terminated_nodes_by_time = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [
      Common::Filter.new(key: "state", values: ["TERMINATED"]),
      Common::Filter.new(key: "statechange_timerange", values: ["2018-03-05T00:00:00Z", formatted_now_time_utc])
    ])
    assert_equal(1, terminated_nodes_by_time.total)

    # and there should be nothing for 30 sec ago
    formatted_sec_ago_utc = (Time.now.utc - 30).iso8601
    terminated_nodes_by_time = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [
      Common::Filter.new(key: "state", values: ["TERMINATED"]),
      Common::Filter.new(key: "statechange_timerange", values: ["2018-03-05T00:00:00Z", formatted_sec_ago_utc])
    ])
    assert_equal(0, terminated_nodes_by_time.total)

    # test tag filtering
    filtered_nodes_by_tags = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [
      Common::Filter.new(key: "environment", values: ["trou"])
    ])
    assert_equal(2, filtered_nodes_by_tags.total)

    filtered_nodes_by_tags = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [
      Common::Filter.new(key: "state", values: ["TERMINATED"]),
      Common::Filter.new(key: "group", values: ["mak", "do"])
    ])
    assert_equal(1, filtered_nodes_by_tags.total)

    filtered_nodes_by_tags = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [
      Common::Filter.new(key: "state", values: ["TERMINATED"]),
      Common::Filter.new(key: "environment", values: ["trou"]),
      Common::Filter.new(key: "group", values: ["mak", "do"])
    ])
    assert_equal(1, filtered_nodes_by_tags.total)
  end

  it "does not include deleted jobs in job list" do
    job_id = GRPC(jobs, :create, Jobs::Job.new(
      name: "deletable job",
      tags: [],
      type: "detect",
      nodes: [@docker_node_id1],
      retries: 1,
      node_selectors: [],
      parent_id: "123"
    ))['id']

    assert_uuid(job_id)

    job_ids = GRPC(jobs, :list, Jobs::Query.new())['jobs'].map {|j| j['id'] }
    # Assert that deleted is false

    assert_includes job_ids, job_id

    GRPC jobs, :delete, Jobs::Id.new(id: job_id)

    job_ids = GRPC(jobs, :list, Jobs::Query.new())['jobs'].map {|j| j['id'] }

    refute_includes job_ids, job_id
  end

  it "indicates jobs are deleted after deleting them" do
    job_id = GRPC(jobs, :create, Jobs::Job.new(
      name: "deletable job",
      tags: [],
      type: "detect",
      nodes: [@docker_node_id1],
      retries: 1,
      node_selectors: [],
      parent_id: "123"
    ))['id']

    assert_uuid(job_id)

    job = GRPC jobs, :read, Jobs::Id.new(id: job_id)
    refute job['deleted']

    GRPC jobs, :delete, Jobs::Id.new(id: job_id)

    job = GRPC jobs, :read, Jobs::Id.new(id: job_id)
    assert job['deleted']
  end
end
