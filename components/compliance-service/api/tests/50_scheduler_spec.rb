require_relative 'test_support'
##### GRPC SETUP #####
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/external/secrets/secrets_services_pb'
require 'api/jobs/jobs_pb'
require 'api/jobs/jobs_services_pb'

describe File.basename(__FILE__) do
  Jobs = Chef::Automate::Domain::Compliance::Api::Jobs unless defined?(Jobs)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

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

  before(:all) { cleanup }
  after(:all) { cleanup }

  it "works" do
    profile_url = 'https://github.com/dev-sec/apache-baseline/archive/master.tar.gz'

    now_time_utc = Time.now().utc()
    formatted_now_time_utc = now_time_utc.strftime('%Y%m%dT%H%M%SZ')

    secret_id1 = (SS_GRPC secrets, :create, Secrets::Secret.new(
      name:"My SSH Login secrets",
      type:"ssh",
      data: [
        Secrets::Kv.new( key:"username", value:"pwsudo" ),
        Secrets::Kv.new( key:"password", value:"password" )
      ],
      tags:[]
    )).id

    # Add an ssh node to be able to reference it in the job
    node_id1 = (MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name:"My Working SSH Node",
      manager:"automate",
      target_config: Nodes::TargetConfig.new(
        backend:"ssh",
        sudo:true,
        host:"localhost",
        port:11030,
        secrets:[secret_id1]
      ),
      tags: [
        Common::Kv.new( key:"_no_auto_detect", value:"true" )
      ]
    )).id

    # Run every minute, only five times, no explicit start date
    job_id1 = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec job1",
      tags: [],
      type: "exec",
      nodes: ["#{node_id1}"],
      retries: 1,
      node_selectors: [],
      profiles: [profile_url],
      recurrence: "FREQ=MINUTELY;INTERVAL=1;COUNT=5"
    )).id

    # Run every minute, only ONE time, with explicit start date of now
    test_count_max_job_id = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec job for count test",
      tags: [],
      type: "exec",
      nodes: ["#{node_id1}"],
      retries: 1,
      node_selectors: [],
      profiles: [profile_url],
      recurrence: "FREQ=MINUTELY;INTERVAL=1;COUNT=1;DTSTART=#{formatted_now_time_utc}"
    )).id

    # Invalid recurrence rule
    assert_grpc_error("Invalid job recurrence rule strconv.Atoi: parsing \"0.25\": invalid syntax", 3) do
      GRPC jobs, :create, Jobs::Job.new(
        name: "Invalid recurrence rule",
        tags: [],
        type: "exec",
        nodes: ["#{node_id1}"],
        retries: 1,
        node_selectors: [],
        profiles: [profile_url],
        recurrence: "DTSTART=#{formatted_now_time_utc};FREQ=HOURLY;INTERVAL=0.25"
      )
    end

    # Run with only date of now
    date_only_job_id = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec for date_only job",
      tags: [],
      type: "exec",
      nodes: ["#{node_id1}"],
      retries: 1,
      node_selectors: [],
      profiles: [profile_url],
      recurrence: "DTSTART=#{formatted_now_time_utc}"
    )).id

    all_jobs = GRPC jobs, :list, Jobs::Query.new()
    # we should have three jobs now
    assert_equal(all_jobs.total, 3)

    first_job = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
    # the parent job should have status scheduled
    assert_equal(first_job.status, "scheduled")
    assert_equal(first_job.parent_id, "")
    assert_equal(first_job.job_count, 0)

    job_id2 = (GRPC jobs, :create, Jobs::Job.new(
      name: "My exec job2",
      tags: [],
      type: "exec",
      nodes: ["#{node_id1}"],
      retries: 1,
      node_selectors: [],
      profiles: [profile_url],
      recurrence: "FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z"
    )).id

    sleep 1
    all_jobs = GRPC jobs, :list, Jobs::Query.new()

    # we should have four jobs now, the three from above and our new scheduled-for-the-future one
    assert_equal(all_jobs.total >= 4, true)

    job2 = GRPC jobs, :read, Jobs::Id.new(id: job_id2)
    # since job is scheduled for future, it should have status scheduled
    assert_equal(job2.status, "scheduled")
    assert_equal(job2.recurrence, "FREQ=HOURLY;INTERVAL=1;COUNT=5;DTSTART=20290101T000000Z")

    # and no parent_id
    assert_equal(job2.parent_id, "")
    # scheduled datetime should be same as dtstart value
    assert_equal(job2.scheduled_time, Google::Protobuf::Timestamp.new(seconds: 1861920000, nanos: 0))

    sleep 60
    all_jobs = GRPC jobs, :list, Jobs::Query.new()
    # we should have seven jobs now, the three from above, our scheduled-for-the-future one,
    # our child job from the first one, our child job from the test_count_max_job job
    # and our child job from the date_only job
    assert_equal(all_jobs.total >= 7, true)

    # ensure parent job 1 has a job_count of 1
    first_job = GRPC jobs, :read, Jobs::Id.new(id: job_id1)
    assert_equal(first_job.job_count >= 1, true)

    # ensure test_count_max_job has a job_count of 1, status of completed
    test_count_max_job = GRPC jobs, :read, Jobs::Id.new(id: test_count_max_job_id)
    assert_equal(test_count_max_job.job_count, 1)
    assert_equal(test_count_max_job.status, "completed")

    # ensure date_only_job has a job_count of 1, status of completed
    date_only_job = GRPC jobs, :read, Jobs::Id.new(id: date_only_job_id)
    assert_equal(date_only_job.job_count, 1)
    assert_equal(date_only_job.status, "completed")

    # ensure child job has job_count 0 and parent_id of job1
    all_jobs.jobs.each { |job|
      if job.parent_id != ""
        assert_equal(job.job_count, 0)
        if job.parent_id == job_id1 || job.parent_id == test_count_max_job_id || job['parent_id'] == date_only_job_id
          job.parent_id = "good"
        end
        assert_equal(job.parent_id, "good")
      end
    }

    # Testing jobs list with exec job_type and parent_job filter..
    assert_grpc_error("Invalid parent_job uuid filter: bad-uuid", 3) do
      GRPC jobs, :list, Jobs::Query.new(
        filters: [
          Common::Filter.new( key:  "job_type", values: ["exec"]),
          Common::Filter.new( key:  "parent_job", values: ["bad-uuid"])
        ]
      )
    end

    type_jobs_json = GRPC jobs, :list, Jobs::Query.new(
      filters: [
        Common::Filter.new( key:  "job_type", values: ["exec"]),
        Common::Filter.new( key:  "parent_job", values: ["00000000-34fb-4022-72ae-8847b54d4ea0"])
      ]
    )
    assert_equal(type_jobs_json, Jobs::Jobs.new())

    type_jobs_json = GRPC jobs, :list, Jobs::Query.new(
      filters: [
        Common::Filter.new( key:  "job_type", values: ["exec"]),
        Common::Filter.new( key:  "parent_job", values: [job_id1])
      ]
    )
    assert_equal(type_jobs_json.jobs.class, Google::Protobuf::RepeatedField)
    if type_jobs_json.jobs.class == Google::Protobuf::RepeatedField && type_jobs_json.jobs.length > 0
      assert_equal(type_jobs_json.jobs[0]['parent_id'], job_id1)
      assert_equal(type_jobs_json.jobs[0]['name'], 'My exec job1 - run 1')
    end

    # Testing jobs list with parent_job filter == "" (when you want no child jobs in the list)
    no_child_jobs = GRPC jobs, :list, Jobs::Query.new(
      filters:[
        Common::Filter.new( key: "parent_job", values: [""])
      ]
    )
    assert_equal(no_child_jobs['total'], 4)

    # Test we cannot rerun a scheduled job
    assert_grpc_error("Unable to rerun a scheduled job", 3) do
      GRPC jobs, :rerun, Jobs::Id.new(id: date_only_job_id)
    end
  end
end
