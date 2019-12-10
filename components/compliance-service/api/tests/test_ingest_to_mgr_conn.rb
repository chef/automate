##### GRPC SETUP #####
require 'api/nodes/nodes_pb'
require 'api/nodes/nodes_services_pb'
require 'api/manager/manager_pb'
require 'api/manager/manager_services_pb'

describe File.basename(__FILE__) do
  it "works" do
    Manager = Chef::Automate::Domain::Nodemanager::Api::Manager
    Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes
    Common = Chef::Automate::Domain::Compliance::Api::Common
    nodes = Nodes::NodesService

    original_manually_managed_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "manager_id", values: ["e69dc612-7e67-43f2-9b19-256afd385820"])])

    # since we are running this as part of the `make test-reporting` command, we expect to have
    # seven nodes to start with, because there are seven reports sent in as part of the ingest-reports-into-es
    # command. as part of the makefile task that runs this test, we send in three reports, two of which are for the
    # same node. so we expect the total here to be 9 nodes.
    nodes_list = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    counter = 0
    while nodes_list.total < 9 do
      puts "sleeping 5, counter: ", counter
      counter +=1
      nodes_list = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    end
    assert_equal(11, nodes_list.total)

    state_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters:[Common::Filter.new(key: "state", values: ["RUNNING", "STOPPED", ""])])
    assert_equal(11, state_nodes.total)

    # those nodes should not have been added to the manual node manager, as they were ingested nodes, not manually added nodes
    manually_managed_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "manager_id", values: ["e69dc612-7e67-43f2-9b19-256afd385820"])])
    assert_equal(original_manually_managed_nodes.total, manually_managed_nodes.total)

    # ensure we can read an ingested node
    nodes_list['nodes'].each { |node|
      node_read = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node.id)
    }
  end

  it "nodes have scan data" do
    nodes = Nodes::NodesService
    nodes_list = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    assert nodes_list["nodes"].all? { |node|
      node["scan_data"].id.length > 0 &&
        node["scan_data"].status.length > 0 &&
        node["scan_data"].end_time != nil
    }, "Nodes did not have necessary scan_data: #{nodes_list["nodes"]}"
  end

  it "not allowed to update name or tags of ingested node" do
    nodes = Nodes::NodesService
    nodes_list = MANAGER_GRPC nodes, :list, Nodes::Query.new()

    assert_grpc_error("invalid option. unable to update name of ingested node", 3) do
      MANAGER_GRPC nodes, :update, Nodes::Node.new(
        id: nodes_list['nodes'][0].id,
        name: "new name"
      )
    end

    assert_grpc_error("invalid option. unable to update tags of ingested node", 3) do
      node = nodes_list['nodes'][0]
      MANAGER_GRPC nodes, :update, Nodes::Node.new(
        id: node.id,
        name: node.name,
        tags: [
          Common::Kv.new(key: "blablabla", value: "hahaha")
        ]
      )
    end
  end
end
