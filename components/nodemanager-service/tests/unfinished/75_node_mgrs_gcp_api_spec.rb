##### GRPC SETUP #####
require 'api/manager/manager_pb'
require 'api/manager/manager_services_pb'
require 'api/external/secrets/secrets_services_pb'
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/jobs/jobs_pb'
require 'api/jobs/jobs_services_pb'
=begin
describe File.basename(__FILE__) do
  GOOGLE_CREDENTIALS_JSON = ENV['GOOGLE_CREDENTIALS_JSON'] || raise('GOOGLE_CREDENTIALS_JSON ENV variable missing, needs to contain the contents of the google service account json file')

  Manager = Chef::Automate::Domain::Nodemanager::Api::Manager unless defined?(Manager)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Jobs = Chef::Automate::Domain::Compliance::Api::Jobs unless defined?(Jobs)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

  def manager ; Manager::NodeManagerService ; end
  def secrets ; Secrets::SecretsService ; end
  def nodes ; Nodes::NodesService ; end
  def jobs ; Jobs::JobsService ; end

  def cleanup
    return if ENV['NO_DELETE']

    MANAGER_GRPC(manager, :list, Manager::Query.new())['managers'].each do |m|
      MANAGER_GRPC manager, :delete, Manager::Id.new(id: m.id) if m.id != "e69dc612-7e67-43f2-9b19-256afd385820"
    end

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

  it "returns an error message when gcp secret type has wrong data" do
    assert_grpc_error("Invalid data content for secret type 'gcp'. GOOGLE_CREDENTIALS_JSON not provided", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        name:"My Bad GCP Cred",
        tags: [],
        type: "gcp",
        data: [
          Secrets::Kv.new(
            key: "GOOGLE_CREDENTIALS",
            value: "bad one"
          )
        ]
      )
    end

    assert_grpc_error("Unable to unmarshal Google Credentials JSON: invalid character 'b' looking for beginning of value", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        name:"My Bad GCP Cred2",
        tags: [],
        type: "gcp",
        data: [
          Secrets::Kv.new(
            key: "GOOGLE_CREDENTIALS_JSON",
            value: "bad two"
          )
        ]
      )
    end

    assert_grpc_error("Only 'service_account' type is supported for GOOGLE_CREDENTIALS_JSON", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        name:"My Bad GCP Cred3",
        tags: [],
        type: "gcp",
        data: [
          Secrets::Kv.new(
            key: "GOOGLE_CREDENTIALS_JSON",
            value: '{"client_id":"764086051850...","client_secret":"d-FL95Q19q7MQm...","type":"authorized_user"}'
          )
        ]
      )
    end

    assert_grpc_error("handleCredentialData: unable to validate secret: Unable to unmarshal Google Credentials JSON: invalid character 'b' looking for beginning of value", 3) do
      # This is how the UI creates the manager, with bundled credentials in the request
      MANAGER_GRPC manager, :create, Manager::NodeManager.new(
        credential_data: [
          Common::Kv.new(
            key: "GOOGLE_CREDENTIALS_JSON",
            value: "bad two two"
          )
        ],
        name: "gcp manager w/ bad bundled creds2",
        type: "gcp"
      )
    end

    assert_grpc_error("handleCredentialData: unable to validate secret: Only 'service_account' type is supported for GOOGLE_CREDENTIALS_JSON", 3) do
      # This is how the UI creates the manager, with bundled credentials in the request
      MANAGER_GRPC manager, :create, Manager::NodeManager.new(
        credential_data: [
          Common::Kv.new(
            key: "GOOGLE_CREDENTIALS_JSON",
            value: '{"client_id":"764086051850...","client_secret":"d-FL95Q19q7MQm...","type":"authorized_user"}'
          )
        ],
        name: "gcp manager w/ bad bundled creds2",
        type: "gcp"
      )
    end

    # Getting an API error from GCP based on connectivity checking with a missing project_id
    parsed_creds = JSON.parse(GOOGLE_CREDENTIALS_JSON)
    parsed_creds['project_id'] = 'get-an-api-error-from-google-to-test-connectivity-check-23'
    assert_grpc_error("rpc error: code = Unknown desc = googleapi: Error 404: Failed to find project get-an-api-error-from-google-to-test-connectivity-check-23, notFound", 2) do
      # This is how the UI creates the manager, with bundled credentials in the request
      MANAGER_GRPC manager, :create, Manager::NodeManager.new(
        credential_data: [
          Common::Kv.new(
            key: "GOOGLE_CREDENTIALS_JSON",
            value: parsed_creds.to_json
          )
        ],
        name: "gcp manager w/ bad bundled creds3",
        type: "gcp"
      )
    end
  end

  it "works" do
    # Add a good Google cloud secret to be referenced by node manager
    good_secret = SS_GRPC secrets, :create, Secrets::Secret.new(
      name:"My GCP Cred",
      type:"gcp-api",
      data: [
        Secrets::Kv.new(
          key: "GOOGLE_CREDENTIALS_JSON",
          value: GOOGLE_CREDENTIALS_JSON
        )
      ]
    )
    good_creds_secret_id = good_secret['id']

    # Add a good Google cloud node manager, referencing the secret created above
    good_secret_node_manager = MANAGER_GRPC manager, :create, Manager::NodeManager.new(
      credential_id: "#{good_creds_secret_id}",
      name: "good gcp manager",
      type: "gcp-api"
    )
    good_creds_manager_id = good_secret_node_manager.ids[0].id

    # Get the gcp node manager by id
    read_manager = MANAGER_GRPC manager, :read, Manager::Id.new(id: good_creds_manager_id)
    assert_equal(true, read_manager.account_id != "")

    # Overwrite the data_added and test the rest of the fields
    read_manager.date_added = Google::Protobuf::Timestamp.new()
    assert_equal(read_manager, Manager::NodeManager.new(
        id: "#{good_creds_manager_id}",
        name: "good gcp manager",
        type: "gcp-api",
        credential_id: "#{good_creds_secret_id}",
        account_id: "alex-pop-project",
        date_added: Google::Protobuf::Timestamp.new(),
        status: "reachable"
    ))

    # Find the api node that's automatically added for the GCP integration
    nodes_list = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters:[Common::Filter.new(key: "manager_id", values: [good_creds_manager_id])])
    assert_equal(1, nodes_list.total)

    # Add a detect job with tags
    detect_job = GRPC jobs, :create, Jobs::Job.new(
      name: "my Detect Job For gcp-api node",
      tags: [
        Common::Kv.new( key:"reason", value:"checking the connectivity" ),
        Common::Kv.new( key:"team", value:"awesome" )
      ],
      type: "detect",
      nodes: [nodes_list.nodes.first.id],
      retries: 1,
      node_selectors: [],
      parent_id: "123"
    )
    detect_job_id = detect_job['id']
    assert_uuid(detect_job_id)

    # Add an exec job that should succeed
    exec_job = GRPC jobs, :create, Jobs::Job.new(
      name: "My Exec Job For gcp-api node",
      tags: [],
      type: "exec",
      nodes: [nodes_list.nodes.first.id],
      profiles: ["https://s3.eu-west-2.amazonaws.com/apop-bucket/mygcp-0.3.0.tar.gz"],
      retries: 1,
      node_selectors: []
    )
    print "\n * Adding job: #{exec_job.to_json}\n"
    exec_job1_id = exec_job['id']
    assert_uuid(exec_job1_id)

    # This is how the UI creates the manager, with bundled credentials in the request
    another_node_manager_id = (MANAGER_GRPC manager, :create, Manager::NodeManager.new(
      credential_data: [
        Common::Kv.new(
          key: "GOOGLE_CREDENTIALS_JSON",
          value: GOOGLE_CREDENTIALS_JSON
        )
      ],
      name: "good gcp manager w/ bundled creds",
      type: "gcp"
    )).ids[0].id

    # Get the gcp node manager by id
    read_manager = MANAGER_GRPC manager, :read, Manager::Id.new(id: another_node_manager_id)
    assert_equal(true, read_manager.account_id != "")
    # Start a job with a node manager reference
    exec_job2_id = (GRPC jobs, :create, Jobs::Job.new(
      name: "My Node Manager Job - GCP",
      tags: [],
      type: "exec",
      profiles: ["https://s3.eu-west-2.amazonaws.com/apop-bucket/mygcp-0.3.0.tar.gz"],
      retries: 1,
      node_selectors: [
        Jobs::ManagerFilter.new(
          manager_id: "#{good_creds_manager_id}"
        )
      ]
    )).id

    # jobs are time-consuming for tests. That's why we parallelize them ^ and wait to complete
    detect_job_status = 'unknown'
    exec1_job_status = 'unknown'
    exec2_job_status = 'unknown'
    retries = 30
    while retries > 0 do
      detect_job_status = GRPC(jobs, :read, Jobs::Id.new(id: detect_job_id)).status
      detect_job_done = ['completed','failed'].include?(detect_job_status)
      exec_job1_status = GRPC(jobs, :read, Jobs::Id.new(id: exec_job1_id)).status
      exec_job1_done = ['completed','failed'].include?(exec_job1_status)
      exec_job2_status = GRPC(jobs, :read, Jobs::Id.new(id: exec_job2_id)).status
      exec_job2_done = ['completed','failed'].include?(exec_job2_status)
      if (detect_job_done && exec_job1_done && exec_job2_done)
        break
      else
        sleep 3
        retries -= 1
      end
    end
    print "\n * while loop exited with remaining retries=#{retries}\n"
    assert_equal('completed', detect_job_status)
    assert_equal('completed', exec_job1_status)
    assert_equal('completed', exec_job2_status)


    nodes_filtered = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "#{another_node_manager_id}",
      query: Manager::Query.new(
        filter_map: []
      ),
    )
    assert_equal(1, nodes_filtered['nodes'].length)
    assert_equal(["alex-pop-project"], nodes_filtered['nodes'])


    nodes_filtered = MANAGER_GRPC manager, :search_node_fields, Manager::FieldQuery.new(
      node_manager_id: "#{good_creds_manager_id}",
      query: Manager::Query.new(
        filter_map: []
      ),
      field: "name"
    )
    assert_equal(true, nodes_filtered['fields'].length > 0)
    assert_equal(["alex-pop-project"], nodes_filtered['fields'])
  end
end
=end
