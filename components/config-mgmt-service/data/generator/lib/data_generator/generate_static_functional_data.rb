require_relative '../data_generator'
require_relative 'elasticsearch_insert_client'
require 'json'
require 'securerandom'
require 'time'

module DataGenerator
  class GenerateStaticFunctionalData
    def initialize
      end_time = Time.new(2017, 04, 13, 0, 0, 0, '+00:00').utc
      client = DataGenerator::ElasticsearchInsertClient.new
      (0..99).each do |i|
        node_name = "node#{i}"
        puts "Generating node #{node_name}..."

        timestamp = end_time - (60 * 60 * i)
        chef_server_org_name = ["org1", "org2"][i % 2]

        node_converges = (i % 3)+1
        (1..node_converges).each do |n|
          converge_start_time = timestamp + (60*10*i)
          converge_end_time = converge_start_time + (60*10*(i+1))
          puts "Generating converge for #{node_name} with end time #{converge_end_time.iso8601}..."

          status = ['success', 'failure', 'missing'][n % 3]
          # The run_id that we will use to inject to the
          # node-state document as the latest_run_id
          run_id = SecureRandom.uuid

          node_converge = generate_node_converge(
            run_id,
            node_state,
            status,
            converge_start_time,
            converge_end_time
          )

          save_node_converge_document!(client, converge_end_time, node_converge)
        end

        node_state = generate_node_state(
          i,  # index
          node_name,
          chef_server_org_name,
          timestamp,
          run_id # latest_run_id
        )

        save_node_state_document!(client, node_state)

        client.write # Write saved node and converges via bulk insert API
      end
    end

    private

    def generate_node_state(
      index,
      node_name,
      chef_server_org_name,
      node_time,
      latest_run_id
    )
      role = ["role1", "role2", "role3", "role4", nil][index % 5]
      status = ['success', 'failure', 'missing'][index % 3]
      environment = ["rehearsal", "union", "delivered"][index % 3]

      node_state_hash = {
        entity_uuid: SecureRandom.uuid,
        exists: true,
        name: node_name,
        fqdn: "#{node_name}.cd",
        ipaddress: "192.168.0.0",
        checkin: node_time.iso8601,
        uptime_seconds: 60 * 60,
        status: status,
        source_fqdn: "chef-server.cd",
        organization_name: chef_server_org_name,
        platform: "ubuntu",
        platform_family: 'robinson',
        platform_version: '0.0.42',
        latest_run_id: latest_run_id
      }

      if role
        node_state_hash[:roles] = role
        node_state_hash[:environment] = environment
      else
        policy_name = ['policy1', 'policy2'][index % 2]
        policy_group = ['dev', 'prod'][index % 2]
        policy_revision = Faker::Crypto.sha256

        node_state_hash[:policy_name] = policy_name
        node_state_hash[:policy_group] = policy_group
        node_state_hash[:policy_revision] = policy_revision
      end

      node_state_hash
    end

    def save_node_state_document!(client, node_state)
      client.create index: 'node-state',
                    type: 'node-state',
                    id: node_state[:entity_uuid],
                    body: node_state
    end

    def generate_node_converge(run_id, node, status, converge_start_time, converge_end_time)
      doc = {
        event_type: 'converge',
        event_action: 'finished',
        node_name: node[:name],
        run_id: run_id,
        status: status,
        start_time: converge_start_time.iso8601,
        end_time: converge_end_time.iso8601
      }

      [:entity_uuid, :environment, :platform, :uptime_seconds, :ipaddress, :fqdn].each do |key|
        doc[key] = node[key]
      end

      doc
    end

    def save_node_converge_document!(client, end_time, node_converge)
      client.create index: "converge-history-#{end_time.strftime("%Y.%m.%d")}",
                    type: 'converge',
                    body: node_converge,
                    id: node_converge[:run_id]
    end
  end
end
