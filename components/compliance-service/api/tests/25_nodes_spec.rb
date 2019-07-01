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
  Nodes = Chef::Automate::Domain::Nodemanager::Api::Nodes unless defined?(Nodes)
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)
  Jobs = Chef::Automate::Domain::Compliance::Api::Jobs unless defined?(Jobs)
  Common = Chef::Automate::Domain::Compliance::Api::Common unless defined?(Common)

  def manager ; Manager::NodeManagerService ; end
  def secrets ; Secrets::SecretsService ; end
  def nodes ; Nodes::NodesService ; end
  def jobs; Jobs::JobsService ; end

  def cleanup
    return if ENV['NO_DELETE']
    MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes'].each do |s|
      MANAGER_GRPC nodes, :delete, Nodes::Id.new(id: s.id)
    end

    GRPC(jobs, :list, Jobs::Query.new())['jobs'].each do |s|
      GRPC jobs, :delete, Jobs::Id.new(id: s.id)
    end

    SS_GRPC(secrets, :list, Secrets::Query.new())['secrets'].each do |s|
      SS_GRPC secrets, :delete, Secrets::Id.new(id: s.id)
    end
  end

  before(:all) { cleanup }
  after(:all) { cleanup }

  it "works" do
    ##### GRPC Failure tests #####
    assert_grpc_error("Not found for id: missing", 5) do
      MANAGER_GRPC nodes, :read, Nodes::Id.new(id: 'missing')
    end

    assert_grpc_error("Invalid sort field, valid ones are: [last_contact manager name platform platform_version status]", 3) do
      MANAGER_GRPC nodes, :list, Nodes::Query.new(sort: "wrong")
    end

    ##### GRPC Success tests #####
    actual = MANAGER_GRPC nodes, :list, Nodes::Query.new(page: 1)
    assert_equal Nodes::Nodes.new(nodes: [], total: 0), actual

    secret = SS_GRPC secrets, :create, Secrets::Secret.new(
      name:"My WinRM Cred",
      type:"winrm",
      data: [
        Secrets::Kv.new(key:"username", value:"administrator"),
        Secrets::Kv.new(key:"password", value:"123456" )
      ],
      tags:[]
    )
    secret_id = secret.id

    assert_grpc_error("Invalid node. Port for node must be within range: 1-65535", 3) do
      MANAGER_GRPC nodes, :create, Nodes::Node.new(
        name: "M$",
        manager: "automate",
        target_config: Nodes::TargetConfig.new(
          backend: "winrm",
          host: "5.6.7.8",
          secrets: [secret_id]
        ),
        tags: []
      )
    end

    assert_grpc_error("Invalid node. Port for node must be within range: 1-65535", 3) do
      MANAGER_GRPC nodes, :create, Nodes::Node.new(
        name: "Alphamale",
        manager: "automate",
        target_config: Nodes::TargetConfig.new(
          backend: "ssh",
          host: "1.2.3.4",
          sudo: true,
          secrets: [secret_id]
        ),
        tags: [
          Common::Kv.new(key: "environment", value: "Pro duc tion")
        ]
      )
    end

    # Add a node without a name
    assert_grpc_error("Invalid node, \'name\' is a required parameter", 3) do
      MANAGER_GRPC nodes, :create, Nodes::Node.new(
        name: "",
        manager: "automate",
        target_config: Nodes::TargetConfig.new(),
        tags: []
      )
    end

    # Add a node without tags
    node1 = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "M$",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "winrm",
        host: "5.6.7.8",
        secrets: [secret_id],
        port: 5985
      ),
      tags: []
    )
    node_id1 = node1['id']
    # poor man's uuid validation
    assert_uuid(node_id1)

    # Add a node with tags
    node2 = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "betamaniac",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "aws",
        secrets: [secret_id]
      ),
      tags: [
        Common::Kv.new(key: "department", value: "marketing"),
        Common::Kv.new(key: "boss", value: "John")
      ]
    )
    node_id2 = node2['id']
    assert_equal(36, node_id2.size)

    # Add a node with a name
    node3 = MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name: "Alphamale",
      manager: "automate",
      target_config: Nodes::TargetConfig.new(
        backend: "ssh",
        secrets: [secret_id],
        host: "1.2.3.4",
        sudo: true,
        port: 22
      ),
      tags: [
        Common::Kv.new(key: "environment", value: "Pro duc tion")
      ]
    )
    node_id3 = node3['id']

    # Get all nodes
    actual_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    actual_nodes['nodes'].each { |n|
      n['last_contact'] = nil
    }
    expected_nodes = {
      "nodes": [
        {
          "id": node_id3,
          "name": "Alphamale",
          "manager": "automate",
          "tags": [
            {
              "key": "environment",
              "value": "Pro duc tion"
            }
          ],
          "status": "unknown",
          "managerIds": [
            "e69dc612-7e67-43f2-9b19-256afd385820"
          ],
          "runData": {},
          "scanData": {}
        },
        {
          "id": node_id2,
          "name": "betamaniac",
          "manager": "automate",
          "tags": [
            {
              "key": "department",
              "value": "marketing"
            },
            {
              "key": "boss",
              "value": "John"
            }
          ],
          "status": "unknown",
          "managerIds": [
            "e69dc612-7e67-43f2-9b19-256afd385820"
          ],
          "runData": {},
          "scanData": {}
        },
        {
          "id": node_id1,
          "name": "M$",
          "manager": "automate",
          "status": "unknown",
          "managerIds": [
            "e69dc612-7e67-43f2-9b19-256afd385820"
          ],
          "runData": {},
          "scanData": {}
        }
      ],
      "total": 3,
      "totalUnknown": 3
    }
    assert_equal_json_sorted(expected_nodes.to_json, actual_nodes.to_json)

    # Testing the default sorting(name ASC)
    assert_equal(["Alphamale", "betamaniac", "M$"], extract_grpc_field(actual_nodes['nodes'], 'name'))

    # Get all nodes sorted DESC by name, get first two on page 1
    res = MANAGER_GRPC nodes, :list, Nodes::Query.new(
      sort: "name",
      order: 1,
      page: 1,
      per_page: 2
    )
    assert_equal(["M$", "betamaniac"], extract_grpc_field(res['nodes'], 'name'))
    # Test the total count
    assert_equal(3, res['total'])

    # Get all nodes sorted DESC by name, get first two(or how many are left) on page 2
    res = MANAGER_GRPC nodes, :list, Nodes::Query.new(
      sort: "name",
      order: 1,
      page: 2,
      per_page: 2
    )
    assert_equal(["Alphamale"], extract_grpc_field(res['nodes'], 'name'))
    # Test the total count
    assert_equal(3, res['total'])

    # Get node by id with all details
    node1 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node_id1)
    node1['last_contact'] = nil
    expected_node1 = {
      "id": node_id1,
      "name": "M$",
      "manager": "automate",
      "status": "unknown",
      "managerIds": [
        "e69dc612-7e67-43f2-9b19-256afd385820"
      ],
      "runData": {},
      "scanData": {},
      "targetConfig": {
        "secrets": [secret_id],
        "backend": "winrm",
        "host": "5.6.7.8",
        "port": 5985,
        "user": "administrator",
        "password": "123456"
      }
    }
    assert_equal_json_sorted(expected_node1.to_json, node1.to_json)

    # Get node by id with all details
    node2 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node_id2)
    node2['last_contact'] = nil
    expected_node2 ={
      "id": node_id2,
      "name": "betamaniac",
      "manager": "automate",
      "tags": [
        {
          "key": "department",
          "value": "marketing"
        },
        {
          "key": "boss",
          "value": "John"
        }
      ],
      "status": "unknown",
      "managerIds": [
        "e69dc612-7e67-43f2-9b19-256afd385820"
      ],
      "runData": {},
      "scanData": {},
      "targetConfig": {
        "secrets": [secret_id],
        "backend": "aws"
      }
    }
    assert_equal_json_sorted(expected_node2.to_json, node2.to_json)

    # Get node by id with all details
    node3 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node_id3)
    node3['last_contact'] = nil
    expected_node3 = {
      "id": node_id3,
      "name": "Alphamale",
      "manager": "automate",
      "tags": [
        {
          "key": "environment",
          "value": "Pro duc tion"
        }
      ],
      "status": "unknown",
      "managerIds": [
        "e69dc612-7e67-43f2-9b19-256afd385820"
      ],
      "runData": {},
      "scanData": {},
      "targetConfig": {
        "secrets": [secret_id],
        "backend": "ssh",
        "host": "1.2.3.4",
        "port": 22,
        "sudo": true,
        "user": "administrator",
        "password": "123456"
      }
    }
    assert_equal_json_sorted(expected_node3.to_json, node3.to_json)

    # Update a node
    MANAGER_GRPC nodes, :update, Nodes::Node.new(
      id: node_id3,
      manager: "automate",
      name: "Donkey Kong",
      target_config: Nodes::TargetConfig.new(
        backend: "ssh",
        host: "5.6.7.8",
        secrets: [secret_id],
        port: 22
      ),
      tags: [
        Common::Kv.new(key: "environment", value: "Yo yo duckies"),
        Common::Kv.new(key: "something", value: "as cute as red pandas"),
      ]
    )

    # Get node by id with all details to compare
    updated_node3 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node_id3)
    updated_node3['last_contact'] = nil
    expected_node3 = {
      "id": node_id3,
      "name": "Donkey Kong",
      "manager": "automate",
      "tags": [
        {
          "key": "environment",
          "value": "Yo yo duckies"
        },
        {
          "key": "something",
          "value": "as cute as red pandas"
        }
      ],
      "status": "unknown",
      "managerIds": [
        "e69dc612-7e67-43f2-9b19-256afd385820"
      ],
      "runData": {},
      "scanData": {},
      "targetConfig": {
        "secrets": [secret_id],
        "backend": "ssh",
        "host": "5.6.7.8",
        "port": 22,
        "user": "administrator",
        "password": "123456"
      }
    }
    assert_equal_json_sorted(expected_node3.to_json, updated_node3.to_json)

    # Add a second secret to reference
    new_ssh_secret = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "username", value: "administratore"),
        Secrets::Kv.new(key: "password", value: "qwerty")
      ],
      name: "sshy $tuff",
      tags: [],
      type: "ssh"
    )

    ssh_secret_id = new_ssh_secret['id']

    # Update node again
    super_updated_node3 = MANAGER_GRPC nodes, :update, Nodes::Node.new(
      id: node_id3,
      manager: "automate",
      name: "Donkey Kong",
      target_config: Nodes::TargetConfig.new(
        backend: "ssh",
        host: "0.4.9.8",
        sudo: true,
        secrets: [ssh_secret_id],
        port: 22
      ),
      tags: []
    )

    # Get node by id with all details to compare
    super_updated_node3 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node_id3)
    expected_node3 = Nodes::Node.new(
        id: node_id3,
        name: "Donkey Kong",
        last_contact: nil,
        manager: "automate",
        manager_ids: ["e69dc612-7e67-43f2-9b19-256afd385820"],
        status: "unknown",
        target_config: Nodes::TargetConfig.new(
          backend: "ssh",
          host: "0.4.9.8",
          user: "administratore",
          sudo: true,
          secrets: [ssh_secret_id],
          port: 22
        ),
        tags: [],
        run_data: Nodes::LastContactData.new(),
        scan_data: Nodes::LastContactData.new()
    )

    # Add a second secret to reference
    new_ssh_super_duper_secret = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new( key:"username", value:"administratore_blah" ),
        Secrets::Kv.new( key:"password", value:"blah" )
      ],
      name:"sshy $tuff_blah",
      tags:[],
      type:"ssh"
    )
    ssh_super_duper_secret_id = new_ssh_super_duper_secret['id']

    # Update node again
    MANAGER_GRPC nodes, :update, Nodes::Node.new(
      id: node_id3,
      manager: "automate",
      name: "Donkey Kong",
      target_config: Nodes::TargetConfig.new(
          backend: "ssh",
          host: "0.4.9.8",
          sudo: true,
          secrets: [secret_id, ssh_super_duper_secret_id],
          port: 22
      ),
      tags: [
        Common::Kv.new(key: "department", value: "engineering")
      ]
    )

    # Get node by id with all details to compare
    super_duper_updated_node_3 = MANAGER_GRPC nodes, :read, Nodes::Id.new(id: node_id3)
    super_duper_updated_node_3['last_contact'] = nil
    expected_node3 = {
      "id": node_id3,
      "name": "Donkey Kong",
      "manager": "automate",
      "tags": [
        {
          "key": "department",
          "value": "engineering"
        }
      ],
      "status": "unknown",
      "managerIds": [
        "e69dc612-7e67-43f2-9b19-256afd385820"
      ],
      "runData": {},
      "scanData": {},
      "targetConfig": {
        "secrets": [secret_id, ssh_super_duper_secret_id],
        "backend": "ssh",
        "host": "0.4.9.8",
        "port": 22,
        "sudo": true,
        "secretsArr": [
          {
            "user": "administrator",
            "password": "123456"
          },
          {
            "user": "administratore_blah",
            "password": "blah"
          }
        ]
      }
    }
    assert_equal_json_sorted(expected_node3.to_json, super_duper_updated_node_3.to_json)

    # add a node to test automatic job creation
    auto_job_creation_test_node_id = (MANAGER_GRPC nodes, :create, Nodes::Node.new(
      name:"test-auto-job-creation",
      manager:"automate",
      target_config: Nodes::TargetConfig.new(
        backend: "winrm",
        host: "5.6.7.8",
        secrets: [secret_id],
        port: 5985
      ),
      tags: []
    )).id

    # fail to upload node if no port not within range
    assert_grpc_error("Invalid node. Port for node must be within range: 1-65535", 3) do
      MANAGER_GRPC nodes, :create, Nodes::Node.new(
        name:"test-not-valid-without-secret",
        manager:"automate",
        target_config: Nodes::TargetConfig.new(
          backend: "winrm",
          host: "5.6.7.8"
        ),
        tags: []
      )
    end

    # Add a node with a good secret and a missing one
    assert_grpc_error("AddNode unable to confirm secrets existence", 3) do
      MANAGER_GRPC nodes, :create, Nodes::Node.new(
        name:"M$",
        manager:"automate",
        target_config: Nodes::TargetConfig.new(
          backend: "winrm",
          host: "5.6.7.8",
          port: 5985,
          secrets: [secret_id, "12345678-a4d8-4af5-45d5-missinginaction"],
        ),
        tags: []
      )
    end


    unfiltered_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new()
    assert_equal(4, unfiltered_nodes.total)
    # `total` should count items returned by query and the other totals should be constant
    unreachable_total = unfiltered_nodes.total_unreachable
    reachable_total = unfiltered_nodes.total_reachable
    unknown_total = unfiltered_nodes.total_unknown

    name_filtered_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters:[Common::Filter.new(key: "name", values: ["Donkey K"])])
    assert_equal(1, name_filtered_nodes.total)
    assert_equal(unreachable_total, name_filtered_nodes.total_unreachable)
    assert_equal(reachable_total, name_filtered_nodes.total_reachable)
    assert_equal(unknown_total, name_filtered_nodes.total_unknown)

    excluded_name_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "name", values: ["Donkey"], exclude: true)])
    assert_equal(3, excluded_name_nodes.total)
    assert_equal(unreachable_total, excluded_name_nodes.total_unreachable)
    assert_equal(reachable_total, excluded_name_nodes.total_reachable)
    assert_equal(unknown_total, excluded_name_nodes.total_unknown)

    manually_managed_nodes = MANAGER_GRPC nodes, :list, Nodes::Query.new(filters: [Common::Filter.new(key: "manager_id", values: ["e69dc612-7e67-43f2-9b19-256afd385820"])])
    assert_equal(4, manually_managed_nodes.total)
    assert_equal(unreachable_total, manually_managed_nodes.total_unreachable)
    assert_equal(reachable_total, manually_managed_nodes.total_reachable)
    assert_equal(unknown_total, manually_managed_nodes.total_unknown)

    tag_keys = MANAGER_GRPC manager, :search_node_fields, Manager::FieldQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: []
      ),
      field: "tags"
    )
    assert_equal(["department", "boss"], tag_keys["fields"])

    names = MANAGER_GRPC manager, :search_node_fields, Manager::FieldQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: []
      ),
      field: "name"
    )
    assert_equal(["betamaniac", "Donkey Kong", "M$", "test-auto-job-creation"], names["fields"])

    tag_vals = MANAGER_GRPC manager, :search_node_fields, Manager::FieldQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: []
      ),
      field: "tags:department"
    )
    assert_equal(["marketing", "engineering"], tag_vals["fields"])

    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(
            key: "name", values: ["bet"], exclude: true
          )
        ]
      )
    )
    assert_equal(["Donkey Kong", "M$", "test-auto-job-creation"], nodes_list["nodes"])

    # Get the nodes with a department tag without passing the values array:
    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(key: "department"),
        ]
      )
    )
    assert_equal(["betamaniac", "Donkey Kong"], nodes_list["nodes"])

    # Get the nodes with a department tag:
    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(key: "department", values: [""]),
        ]
      )
    )
    assert_equal(["betamaniac", "Donkey Kong"], nodes_list["nodes"])

    # Get the nodes without a department tag:
    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(key: "department", values: [""], exclude: true),
        ]
      )
    )
    assert_equal(["M$", "test-auto-job-creation"], nodes_list["nodes"])

    # Get the nodes tagged department:market% or department:missing-in-action%:
    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(key: "department", values: ["market", "missing-in-action"]),
        ]
      )
    )
    assert_equal(["betamaniac"], nodes_list["nodes"])

    # Get the nodes NOT tagged department:market% or department:missing-in-action%:
    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(key: "department", values: ["market", "missing-in-action"], exclude: true),
        ]
      )
    )
    assert_equal(["Donkey Kong", "M$", "test-auto-job-creation"], nodes_list["nodes"])

    # Get the nodes NOT tagged with something:else but tagged boss:Joh%
    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(key: "something", values: ["else"], exclude: true),
          Common::Filter.new(key: "boss", values: ["Joh%"]),
        ]
      )
    )
    assert_equal(["betamaniac"], nodes_list["nodes"])

    nodes_list = MANAGER_GRPC manager, :search_nodes, Manager::NodeQuery.new(
      node_manager_id: "e69dc612-7e67-43f2-9b19-256afd385820",
      query: Manager::Query.new(
        filter_map: [
          Common::Filter.new(
            key: "name", values: ["Donk*"]
          )
        ]
      )
    )
    assert_equal(["Donkey Kong"], nodes_list["nodes"])

    # try to delete manual node mgr, get denied
    assert_grpc_error("Invalid request. Unable to delete manual node manager", 3) do
      MANAGER_GRPC manager, :delete, Manager::Id.new(id: "e69dc612-7e67-43f2-9b19-256afd385820")
    end
  end

  it "can delete multiple nodes with a query" do
    names_before = MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes'].map(&:name)
    assert_includes names_before, "Donkey Kong"

    delete_response = MANAGER_GRPC nodes, :bulk_delete, Nodes::Query.new(filters:[Common::Filter.new(key: "name", values: ["Donkey K"])])
    assert_equal ["Donkey Kong"], delete_response.names

    names_after = MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes'].map(&:name)

    refute_includes names_after, "Donkey Kong"
  end

  it "can delete multiple nodes by ids" do
    nodes_arr = Array.new
    MANAGER_GRPC(nodes, :list, Nodes::Query.new())['nodes'].each do |s|
      nodes_arr.push(s.id)
    end
    MANAGER_GRPC nodes, :bulk_delete_by_id, Nodes::Ids.new(ids: nodes_arr)
  end
end
