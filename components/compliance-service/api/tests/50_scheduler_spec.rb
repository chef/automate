require_relative 'test_support'
##### GRPC SETUP #####
require 'interservice/nodemanager/nodes/nodes_pb'
require 'interservice/nodemanager/nodes/nodes_services_pb'
require 'external/secrets/secrets_services_pb'
require 'interservice/compliance/jobs/jobs_pb'
require 'interservice/compliance/jobs/jobs_services_pb'
require 'external/common/query/parameters_pb'

describe File.basename(__FILE__) do
  Jobs = Chef::Automate::Domain::Compliance::Jobs unless defined?(Jobs)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Nodes = Chef::Automate::Domain::Nodemanager::Nodes unless defined?(Nodes)
  Common = Chef::Automate::Domain::Compliance::Common unless defined?(Common)
  Query = Chef::Automate::Api::Common::Query unless defined?(Query)

  def jobs ; Jobs::JobsService ; end
  def secrets ; Secrets::SecretsService ; end
  def nodes ; Nodes::NodesService ; end

  def cleanup
    return if ENV['NO_DELETE']

    MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes'].each do |s|
      MANAGER_GRPC nodes, :delete, Nodes::Id.new(id: s.id)
    end

    GRPC(jobs, :list, Nodes::Query.new())['jobs'].each do |s|
      GRPC jobs, :delete, Jobs::Id.new(id: s.id)
    end

    SS_GRPC(secrets, :list, Secrets::Query.new())['secrets'].each do |s|
      SS_GRPC secrets, :delete, Secrets::Id.new(id: s.id)
    end
  end

  PROFILE_URL = 'https://github.com/dev-sec/apache-baseline/archive/master.tar.gz'
  NOW_UTC = Time.now().utc().strftime('%Y%m%dT%H%M%SZ')

  before(:all) { cleanup }
  after(:all) { cleanup }

  before(:all) do
    secret_id = (SS_GRPC secrets, :create, Secrets::Secret.new(
      name: "My SSH Login secrets",
      type: "ssh",
      data: [
        Query::Kv.new(key: "username", value: "pwsudo"),
        Query::Kv.new(key: "password", value: "password")
      ],
      tags: []
    )).id

    # Add an ssh node to be able to reference it in the job
    @node_id = (MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "My Working SSH Node",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "ssh",
        sudo: true,
        host: "localhost",
        port: 11030,
        secrets: [secret_id]
      ),
      tags: [
        Common::Kv.new(key: "_no_auto_detect", value: "true")
      ]
    )).id

    # Run every minute, only five times, no explicit start date
    @job_id1 = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec job1",
      tags: [],
      type: "exec",
      nodes: [@node_id],
      retries: 1,
      node_selectors: [],
      profiles: [PROFILE_URL],
      recurrence: "FREQ=MINUTELY;INTERVAL=1;COUNT=5"
    )).id

    # Run every minute, only ONE time, with explicit start date of now
    @test_count_max_job_id = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec job for count test",
      tags: [],
      type: "exec",
      nodes: [@node_id],
      retries: 1,
      node_selectors: [],
      profiles: [PROFILE_URL],
      recurrence: "FREQ=MINUTELY;INTERVAL=1;COUNT=1;DTSTART=#{NOW_UTC}"
    )).id

    # Run with only date of now
    @date_only_job_id = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec for date_only job",
      tags: [],
      type: "exec",
      nodes: [@node_id],
      retries: 1,
      node_selectors: [],
      profiles: [PROFILE_URL],
      recurrence: "DTSTART=#{NOW_UTC}"
    )).id
  end

  it "fails with invalid recurrence rule" do
    assert_grpc_error("Invalid job recurrence rule strconv.Atoi: parsing \"0.25\": invalid syntax", 3) do
      GRPC jobs, :create, Jobs::Job.new(
        name: "Invalid recurrence rule",
        tags: [],
        type: "exec",
        nodes: [@node_id],
        retries: 1,
        node_selectors: [],
        profiles: [PROFILE_URL],
        recurrence: "DTSTART=#{NOW_UTC};FREQ=HOURLY;INTERVAL=0.25"
      )
    end
  end

  it "created 3 jobs during setup" do
    assert_equal(3, GRPC(jobs, :list, Jobs::Query.new()).total)
  end

  it "gives the parent job scheduled status" do
    first_job = GRPC jobs, :read, Jobs::Id.new(id: @job_id1)

    assert_equal("scheduled", first_job.status)
    assert_equal("", first_job.parent_id)
    assert_equal(0, first_job.job_count)
  end

  it "gives new jobs scheduled in the future proper attributes" do
    future_job_id = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec job2",
      tags: [],
      type: "exec",
      nodes: [@node_id],
      retries: 1,
      node_selectors: [],
      profiles: [PROFILE_URL],
      recurrence: "FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z"
    )).id

    sleep 1

    # we should have four jobs now, the three from above and our new scheduled-for-the-future one
    all_jobs = GRPC jobs, :list, Jobs::Query.new()
    assert(all_jobs.total >= 4)

    future_job = GRPC jobs, :read, Jobs::Id.new(id: future_job_id)
    assert_equal("scheduled", future_job.status)
    assert_equal("FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z", future_job.recurrence)
    assert_equal("", future_job.parent_id)
    # scheduled datetime should be same as dtstart value
    assert_equal(Google::Protobuf::Timestamp.new(seconds: 1861920000, nanos: 0), future_job.scheduled_time)
  end

  it "creates new jobs as children of existing ones" do
    all_jobs = GRPC jobs, :list, Jobs::Query.new()    
    10.times {|i| 
      puts "sleeping 10sec, iteration: ", i
      sleep 10
      all_jobs = GRPC jobs, :list, Jobs::Query.new()
      if all_jobs.total >= 7 
        break
      end
    }
    # we should have seven jobs now, the three from above, our scheduled-for-the-future one,
    # our child job from the first one, our child job from the test_count_max_job job
    # and our child job from the date_only job
    assert(all_jobs.total >= 7)
  end

  it "properly updates parent jobs" do
    first_job = GRPC jobs, :read, Jobs::Id.new(id: @job_id1)
    assert(first_job.job_count >= 1)
  end

  it "properly updates jobs that occur once" do
    test_count_max_job = GRPC jobs, :read, Jobs::Id.new(id: @test_count_max_job_id)
    assert_equal(1, test_count_max_job.job_count)
    assert_equal("completed", test_count_max_job.status)
  end

  it "properly updates non-recurring jobs" do
    date_only_job = GRPC jobs, :read, Jobs::Id.new(id: @date_only_job_id)
    assert_equal(1, date_only_job.job_count)
    assert_equal("completed", date_only_job.status)
  end

  it "properly sets the attributes of child jobs" do
    parent_ids = [@job_id1, @test_count_max_job_id, @date_only_job_id]

    all_jobs = GRPC jobs, :list, Jobs::Query.new()

    all_jobs.jobs.each do |job|
      next if job.parent_id == ""

      assert_equal(0, job.job_count)
      assert_includes(parent_ids, job.parent_id)
    end
  end

  it "fails when passed an invalid parent_job filter" do
    assert_grpc_error("Invalid parent_job uuid filter: bad-uuid", 3) do
      GRPC jobs, :list, Jobs::Query.new(
        filters: [
          Common::Filter.new( key: "job_type", values: ["exec"]),
          Common::Filter.new( key: "parent_job", values: ["bad-uuid"])
        ]
      )
    end
  end

  it "returns an empty jobs collection when passed an unused parent_job filter" do
    job_list = GRPC jobs, :list, Jobs::Query.new(
      filters: [
        Common::Filter.new( key: "job_type", values: ["exec"]),
        Common::Filter.new( key: "parent_job", values: ["00000000-34fb-4022-72ae-8847b54d4ea0"])
      ]
    )
    assert_equal(Jobs::Jobs.new(), job_list)
  end

  it "can filter on parent_job" do
    job_list = GRPC jobs, :list, Jobs::Query.new(
      filters: [
        Common::Filter.new( key: "job_type", values: ["exec"]),
        Common::Filter.new( key: "parent_job", values: [@job_id1])
      ]
    )
    # if we take a while to get here, we may have two jobs instead of just one
    if job_list.total == 1 
      puts "one child job found"
      job = job_list.jobs.first

      assert_equal(@job_id1, job.parent_id)
      assert_equal('My exec job1 - run 1', job.name)
    else 
      # should never really be more than two...
      assert_equal(2, job_list.total)
      puts "two child jobs found"

      job_list.jobs.each do |job|
        assert_equal(@job_id1, job.parent_id)
        assert_equal(true, job.name.start_with?('My exec job1 - run'))
      end
    end
  end

  it "can filter out child jobs" do
    job_list = GRPC jobs, :list, Jobs::Query.new(
      filters:[
        Common::Filter.new( key: "parent_job", values: [""])
      ]
    )
    assert_equal(4, job_list.total)
  end

  it "cannot rerun a scheduled job" do
    assert_grpc_error("Unable to rerun a scheduled job", 3) do
      GRPC jobs, :rerun, Jobs::Id.new(id: @date_only_job_id)
    end
  end
end
