require_relative 'test_support'
##### GRPC SETUP #####
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/external/secrets/secrets_services_pb'
require 'api/jobs/jobs_pb'
require 'api/jobs/jobs_services_pb'
require 'api/profiles/profiles_pb'
require 'api/profiles/profiles_services_pb'

describe File.basename(__FILE__) do
  Profiles = Chef::Automate::Domain::Compliance::Api::Profiles unless defined?(Profiles)
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Jobs = Chef::Automate::Domain::Compliance::Api::Jobs unless defined?(Jobs)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

  def jobs ; Jobs::JobsService ; end
  def nodes ; Nodes::NodesService ; end
  def profiles ; Profiles::ProfilesService ; end
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

  it "works" do
    test_start_time = Time.now.utc - 30

    # upload profile for testing against
    profile_content = File.read(File.join(MARKET_PATH, 'linux-patch-baseline-0.3.0.tar.gz'), mode: 'rb')
    req = Profiles::ProfilePostRequest.new(
      owner: 'mario',
      meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
      chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]

    exec_profile = "compliance://mario/linux-patch-baseline#0.3.0"

    ##### Success tests #####
    actual = GRPC jobs, :list, Jobs::Query.new()
    assert_equal Jobs::Jobs.new(), actual

    secret_id1 = (SS_GRPC secrets, :create, Secrets::Secret.new(
      name: "My SSH Login secrets",
      type: "ssh",
      data: [
        Secrets::Kv.new( key: "username", value: "wrong" ),
        Secrets::Kv.new( key: "password", value: "password" )
      ],
      tags: []
    )).id

    SS_GRPC secrets, :update, Secrets::Secret.new(
      id: secret_id1,
      data: [
        Secrets::Kv.new( key: "username", value: "pwsudo" )
      ],
      tags: []
    )

    secret_id2 = (SS_GRPC secrets, :create, Secrets::Secret.new(
      name: "My SSH sudo secrets",
      type: "sudo",
      data: [
        Secrets::Kv.new( key: "password", value: "password" )
      ],
      tags: []
    )).id

    f = File.new("containers/key.pem")
    SSH_PRIVATE_KEY = f.read.chomp
    secret_id3 = (SS_GRPC secrets, :create, Secrets::Secret.new(
      name: "My SSH sudo secrets",
      type: "ssh",
      data: [
        Secrets::Kv.new( key: "username", value: "nosudo" ),
        Secrets::Kv.new( key: "key", value: SSH_PRIVATE_KEY )
      ],
      tags: []
    )).id

    # Add an ssh node to be able to reference it in the job
    node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "My Working SSH Node",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "ssh",
        sudo:true,
        host: "localhost",
        port: 11030,
        secrets: [secret_id1, secret_id2]
      ),
      tags: [
        Common::Kv.new( key: "deployment", value: "ZDV" ),
        Common::Kv.new( key: "farm", value: "STELLAR" ),
        Common::Kv.new( key: "_no_auto_detect", value: "true" )
      ]
    )

    assert_equal(Nodes::Id, node.class)
    ssh_node_id1 = node['id']

    # Add an ssh node to be able to reference it in the job
    node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "My Bad Creds SSH Node",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "ssh",
        sudo: false,
        host: "localhost",
        port: 11030,
        secrets: [secret_id2]
      ),
      tags: [
        Common::Kv.new( key: "failed", value: "unreachable" ),
        Common::Kv.new( key: "reason", value: "bad_creds" ),
        Common::Kv.new( key: "_no_auto_detect", value: "true" )
      ]
    )

    assert_equal(Nodes::Id, node.class)
    ssh_node_id2_bad_creds = node['id']

    # Add an ssh node to be able to reference it in the job
    node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "My Working SSH Key Node",
      manager: "automate",
      target_config:Nodes::TargetConfig.new(
        backend: "ssh",
        sudo:false,
        host: "localhost",
        port: 11030,
        secrets: [secret_id3]
      ),
      tags: [
        Common::Kv.new( key: "_no_auto_detect", value: "true" )
      ]
    )

    assert_equal(Nodes::Id, node.class)
    ssh_node_id3 = node['id']

    # Add a job with tags and name
    job = GRPC jobs, :create, Jobs::Job.new(
      name: "My Detect Job For two nodes",
      tags: [
        Common::Kv.new( key: "reason", value: "checking the connectivity" ),
        Common::Kv.new( key: "team", value: "sshers" )
      ],
      type: "detect",
      nodes: ["#{ssh_node_id1}", "#{ssh_node_id2_bad_creds}"],
      retries: 1,
      node_selectors: []
    )

    job_id1 = job['id']
    assert_uuid(job_id1)

    # Add an exec job that should succeed
    job = GRPC jobs, :create, Jobs::Job.new(
      name: "My Exec Job For Existing node",
      tags: [],
      type: "exec",
      nodes: ["#{ssh_node_id1}"],
      profiles: [exec_profile],
      retries: 1,
      node_selectors: []
    )

    job_id2 = job['id']
    assert_equal(36, job_id2.size)


    job = GRPC jobs, :create, Jobs::Job.new(
      name: "My Exec Job For Bad Creds node",
      tags: [
        Common::Kv.new( key: "trigger", value: "Jenkins 008" )
      ],
      type: "exec",
      nodes: ["#{ssh_node_id2_bad_creds}"],
      profiles: ["https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"],
      retries: 1,
      node_selectors: []
    )
    job_id3 = job['id']
    assert_equal(36, job_id3.size)


    # Add a detect jobs via ssh+key
    job = GRPC jobs, :create, Jobs::Job.new(
      name: "My Exec Job For Existing node w/ Key",
      tags: [],
      type: "detect",
      nodes: ["#{ssh_node_id3}"],
      retries: 1,
      node_selectors: []
    )
    job_id4 = job['id']
    assert_equal(36, job_id4.size)

    all_jobs = {}
    sleep_count = 1
    failed_or_completed = 0
    while failed_or_completed < 4 &&  sleep_count < 100 do
      # Get all jobs
      all_jobs = GRPC jobs, :list, Jobs::Query.new()
      job_statues = all_jobs['jobs'].map{|j| j['status'] }
      puts "> #{sleep_count} < waiting for jobs to conclude, current statuses: #{job_statues}"
      sleep 3
      sleep_count += 1
      failed_or_completed = job_statues.select{ |s| ['failed', 'completed'].include?(s) }.length
    end
    puts "> #{sleep_count} < while exit with statuses: #{job_statues}"

    job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
    assert_equal("failed", job1['status'])


    # extra tests as these are likely to break on inspec bugs
    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
    if (job2['status'] != 'completed' && job2['results'].length > 0)
      job2['results'].each { |r| assert_equal('', r.to_hash[:result].gsub(/\n/,'\n')) }
    end
    assert_equal("completed", job2['status'])

    job3 = GRPC jobs, :read, Jobs::Id.new(id: job_id3)
    assert_equal("failed", job3['status'])

    # extra tests as these are likely to break on inspec bugs
    job4 = GRPC jobs, :read, Jobs::Id.new(id: job_id4)
    if (job4['status'] != 'completed' && job4['results'].length > 0)
      job4['results'].each { |r| assert_equal('', r.to_hash[:result].gsub(/\n/,'\n')) }
    end
    assert_equal("completed", job4['status'])

    jobs_list = all_jobs['jobs']
    jobs_list.each {|j|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, j, 'start_time'))
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, j, 'end_time'))
    }

    # Test the total count
    assert_equal(4, all_jobs['total'])
    assert_equal( [job_id1, job_id2, job_id3, job_id4].sort!,
                  extract_grpc_field(jobs_list, 'id').sort! )
    assert_equal(["failed", "completed", "failed", "completed"].sort!,
                 extract_grpc_field(jobs_list, 'status').sort! )
    assert_equal( [2, 1, 1, 1].sort!,
                  extract_grpc_field(jobs_list, 'node_count').sort! )
    assert_equal( ["detect", "exec", "exec", "detect"].sort!,
                  extract_grpc_field(jobs_list, 'type').sort! )

    # Get job by id with all details
    result_should_contain = "Failed to verify connectivity to localhost : exit status 1"
    job1 = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job1, 'start_time'))
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job1, 'end_time'))
    job1['results'].each {|result|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'start_time'))
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'end_time'))
      ResultStuff.checkResultAndAdjustIfNeeded(result, 'result', result_should_contain)
    }
    assert_equal(job_id1, job1['id'])
    assert_equal("My Detect Job For two nodes", job1['name'])
    assert_equal( [ssh_node_id1, ssh_node_id2_bad_creds].sort!, job1['nodes'].sort! )
    assert_equal("failed", job1['status'])
    assert_equal(2, job1['results'].length)
    assert_equal( ["completed", "failed"].sort!,
                  extract_grpc_field(job1['results'], 'status').sort! )

    # Get job by id with all details
    result_should_contain = "Failed to verify connectivity to localhost : exit status 1"
    job3 = GRPC jobs, :read, Jobs::Id.new(id: job_id3)
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job3, 'start_time'))
    assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, job3, 'end_time'))
    job3['results'].each {|result|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'start_time'))
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, result, 'end_time'))
      ResultStuff.checkResultAndAdjustIfNeeded(result, 'result', result_should_contain)
    }
    assert_equal(job_id3, job3['id'])
    assert_equal("My Exec Job For Bad Creds node", job3['name'])
    assert_equal([ssh_node_id2_bad_creds], job3['nodes'])
    assert_equal("failed", job3['status'])
    assert_equal(1, job3['results'].length)
    assert_equal(["failed"], extract_grpc_field(job3['results'], 'status'))

    # Get all nodes
    actual_nodes = MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes']
    actual_nodes.each {|n|
      assert_equal(true, TimeStuff.checkTimestampAndAdjustIfNeeded(test_start_time, n, 'last_contact'))
    }
    assert_equal( [ssh_node_id1, ssh_node_id2_bad_creds, ssh_node_id3].sort!,
                  (extract_grpc_field(actual_nodes, 'id')).sort!)
    assert_equal( ["reachable", "unreachable", "reachable"].sort!,
                  (extract_grpc_field(actual_nodes, 'status')).sort! )
    assert_equal( ["ubuntu", "", "ubuntu"].sort!,
                  (extract_grpc_field(actual_nodes, 'platform')).sort! )
    assert_equal( ["My Working SSH Node", "My Bad Creds SSH Node", "My Working SSH Key Node"].sort!,
                  (extract_grpc_field(actual_nodes, 'name')).sort! )

    default_jobs_json = GRPC jobs, :list, Jobs::Query.new()
    # Testing the default sorting(name ASC)
    assert_equal( [
      "My Detect Job For two nodes",
      "My Exec Job For Bad Creds node",
      "My Exec Job For Existing node",
      "My Exec Job For Existing node w/ Key"
    ], extract_grpc_field(default_jobs_json['jobs'], 'name') )

    # Sort by name(DESC) before clearing up the jobs
    name_jobs_json = GRPC jobs, :list, Jobs::Query.new(sort: "name", order: 1)
    assert_equal( [
      "My Exec Job For Existing node w/ Key",
      "My Exec Job For Existing node",
      "My Exec Job For Bad Creds node",
      "My Detect Job For two nodes"
    ], extract_grpc_field(name_jobs_json['jobs'], 'name') )

    # Sort by status(ASC), page 2, size 2
    name_jobs_json = GRPC jobs, :list, Jobs::Query.new(sort: "name", order: 1, per_page: 2, page: 2)
    assert_equal( [
      "My Exec Job For Bad Creds node",
      "My Detect Job For two nodes"
    ], extract_grpc_field(name_jobs_json['jobs'], 'name') )
  end

  it "filters jobs by type" do
    job_names = GRPC(jobs, :list, Jobs::Query.new(filters: [
      Common::Filter.new(key: "job_type", values: ["exec"])
    ]))['jobs'].map { |j| j['name'] }

    expected = ["My Exec Job For Bad Creds node", "My Exec Job For Existing node"]

    assert_equal expected, job_names
  end

  it "filters jobs by status" do
    job_names = GRPC(jobs, :list, Jobs::Query.new(filters: [
      Common::Filter.new(key: "status", values: ["completed"])
    ]))['jobs'].map { |j| j['name'] }

    expected = ["My Exec Job For Existing node", "My Exec Job For Existing node w/ Key"]

    assert_equal expected, job_names
  end

  it "filters jobs by name" do
    job_names = GRPC(jobs, :list, Jobs::Query.new(filters: [
      Common::Filter.new(key: "name", values: ["My Exec"])
    ]))['jobs'].map { |j| j['name'] }

    expected = [
      "My Exec Job For Existing node w/ Key",
      "My Exec Job For Existing node",
      "My Exec Job For Bad Creds node",
    ]

    assert_equal expected.sort, job_names.sort
  end

  it "filters jobs by profile name" do
    job_names = GRPC(jobs, :list, Jobs::Query.new(filters: [
      Common::Filter.new(key: "profile", values: ["linux"])
    ]))['jobs'].map { |j| j['name'] }

    expected = ["My Exec Job For Existing node"]

    assert_equal expected, job_names
  end

  it "filters jobs by profile namespace" do
    job_names = GRPC(jobs, :list, Jobs::Query.new(filters: [
      Common::Filter.new(key: "profile", values: ["mar"])
    ]))['jobs'].map { |j| j['name'] }

    expected = ["My Exec Job For Existing node"]

    assert_equal expected, job_names
  end

  it "filters jobs by profile namespace and name" do
    job_names = GRPC(jobs, :list, Jobs::Query.new(filters: [
      Common::Filter.new(key: "profile", values: ["mario/linux"])
    ]))['jobs'].map { |j| j['name'] }

    expected = ["My Exec Job For Existing node"]

    assert_equal expected, job_names
  end
end
