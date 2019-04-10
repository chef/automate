##### GRPC SETUP #####
require 'api/manager/manager_pb'
require 'api/manager/manager_services_pb'
require 'api/secrets/secrets_pb'
require 'api/secrets/secrets_services_pb'
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'

Manager = Chef::Automate::Domain::Nodemanager::Api::Manager
Secrets = Chef::Automate::Domain::Compliance::Api::Secrets
Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes
Common = Chef::Automate::Domain::Compliance::Api::Common

def self.manager ; Manager::NodeManagerService ; end
def self.secrets ; Secrets::SecretsService ; end
def self.nodes ; Nodes::NodesService ; end

# add ssh node, should create auto-detect job and report back as reachable
f = File.new("containers/key.pem")
ssh_key = f.read.chomp
secret_id3 = (GRPC secrets, :create, Secrets::Secret.new(
  name:"My SSH sudo secrets",
  type:"ssh",
  data: [
    Common::Kv.new( key:"username", value:"nosudo" ),
    Common::Kv.new( key:"key", value: ssh_key)
  ],
  tags:[]
)).id

# Add a ssh node
node = MANAGER_GRPC nodes, :create, Nodes::Node.new(
  name:"My Working SSH Key Node",
  manager:"automate",
  target_config:Nodes::TargetConfig.new(
    backend:"ssh",
    sudo:false,
    host:"localhost",
    port:11030,
    secrets:[secret_id3]
  ),
  tags: []
)

puts "##################"
puts "Added node....."
puts "##################"

node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
while node_status != "reachable" do
  sleep 5
  node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
end
assert_equal(node_status, "reachable")

puts "\n##################"
puts "status of node is reachable"
puts "Now go stop the node with docker stop cc_ssh_node"
puts "##################"

node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
while node_status != "unreachable" do
  sleep 5
  node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
end
assert_equal(node_status, "unreachable")

puts "\n##################"
puts "status of node is unreachable"
puts "Now start the node back up with make start-ssh-node from compliance service dir"
puts "##################"

node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
while node_status != "reachable" do
  sleep 5
  node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
end
assert_equal(node_status, "reachable")

puts "\n##################"
puts "status of node is reachable"
puts "Now we are going to change the node state to terminated."
puts "##################"

# change state to terminated
MANAGER_GRPC manager, :change_node_state, Manager::NodeState.new(id: node.id, state: 2)

node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
while node_status != "unreachable" do
  sleep 5
  node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
end
assert_equal(node_status, "unreachable")

puts "\n##################"
puts "status of node is unreachable"
puts "now we wait for 120 seconds to ensure the node does not get updated to reachable again, because we set state to terminated."
puts "when we add more tests to this file, we can use those 120 seconds to test other things."

sleep 120
node_status = (MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)).status
assert_equal(node_status, "unreachable")
