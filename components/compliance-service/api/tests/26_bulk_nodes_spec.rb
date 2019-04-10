##### GRPC SETUP #####
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/external/secrets/secrets_services_pb'
require 'api/jobs/jobs_pb'
require 'api/jobs/jobs_services_pb'
require 'api/manager/manager_pb'
require 'api/manager/manager_services_pb'

describe File.basename(__FILE__) do
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

  def secrets ; Secrets::SecretsService ; end
  def nodes ; Nodes::NodesService ; end

  def cleanup
    return if ENV['NO_DELETE']
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
    # add a credential
    secret = SS_GRPC secrets, :create, Secrets::Secret.new(
      name:"My WinRM Cred",
      type:"ssh",
      data: [
        Secrets::Kv.new(key:"username", value:"administrator"),
        Secrets::Kv.new(key:"password", value:"123456" )
      ],
      tags:[]
    )
    secret_id = secret.id

    # Add nodes via bulk create; this should create three nodes
    bulk_nodes_added = MANAGER_GRPC nodes, :bulk_create, Nodes::Nodes.new(
      nodes: [
        Nodes::Node.new(
            name_prefix: "my-ssh-node",
            manager:"automate",
            target_config: Nodes::TargetConfig.new(
              backend:"ssh",
              hosts:["localhost","127.0.0.1"],
              secrets:[secret_id],
              port: 22
            ),
            tags: [
              Common::Kv.new( key:"test-node", value:"is-amazing" ),
              Common::Kv.new( key:"compliance-service", value:"rockin-like-whoa" )
            ]
        ),
        Nodes::Node.new(
            name: "my-other-node",
            manager:"automate",
            target_config: Nodes::TargetConfig.new(
              backend:"winrm",
              hosts:["localhost"],
              secrets:[secret_id],
              port: 5986
            ),
            tags: [
              Common::Kv.new( key:"test-node", value:"is-more-amazing" )
            ]
        )
      ]
    )
    assert_equal(3, bulk_nodes_added['ids'].length)

    actual_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    assert_equal(3, actual_nodes['nodes'].length)
    assert_equal(["my-other-node", "my-ssh-node-127.0.0.1", "my-ssh-node-localhost"], extract_grpc_field(actual_nodes.nodes, 'name'))

    first_node = actual_nodes['nodes'].find {|n| n.name == "my-other-node" }
    assert_equal([Common::Kv.new( key:"test-node", value:"is-more-amazing" )], first_node.tags)
    # read node to evaluate target config info
    node_read = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: first_node.id)
    assert_equal(5986, node_read.target_config.port)
    assert_equal("localhost", node_read.target_config.host)
    assert_equal("winrm", node_read.target_config.backend)

    second_node = actual_nodes['nodes'].find {|n| n.name == "my-ssh-node-127.0.0.1" }
    assert_equal([Common::Kv.new( key:"test-node", value:"is-amazing" ), Common::Kv.new( key:"compliance-service", value:"rockin-like-whoa" )], second_node.tags)
    # read node to evaluate target config info
    node_read = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: second_node.id)
    assert_equal(22, node_read.target_config.port)
    assert_equal("127.0.0.1", node_read.target_config.host)
    assert_equal("ssh", node_read.target_config.backend)

    third_node = actual_nodes['nodes'].find {|n| n.name == "my-ssh-node-localhost" }
    assert_equal([Common::Kv.new( key:"test-node", value:"is-amazing" ), Common::Kv.new( key:"compliance-service", value:"rockin-like-whoa" )], third_node.tags)
    # read node to evaluate target config info
    node_read = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: third_node.id)
    assert_equal(22, node_read.target_config.port)
    assert_equal("localhost", node_read.target_config.host)
    assert_equal("ssh", node_read.target_config.backend)
  end
end
