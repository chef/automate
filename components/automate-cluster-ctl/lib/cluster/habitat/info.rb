require_relative '../helpers'
require_relative '../terraform/helpers'
require_relative '../ssh'
require 'tomlrb'
require 'net/http'

module AutomateCluster
  module Habitat
    class Info
      include AutomateCluster::Helpers
      include AutomateCluster::Terraform::Helpers

      attr_reader :config
      def initialize
        @logger = AutomateCluster.logger
        config_file = File.join(terraform_path, 'a2ha_habitat.auto.tfvars')
        @config = Tomlrb.load_file(config_file, symbolize_keys: true)
      end

      def auth_token
        config[:hab_sup_http_gateway_auth_token]
      end

      def ring_key
        config[:hab_sup_ring_key]
      end

      def census_data(connection)
        @census_data ||= connection.curl_hab("/census")
      end

      def service_census_data(service, member_id, connection)
        census_data(connection).dig("census_groups", "#{service}.default", "population", member_id)
      end

      def service_data(service)
        headers = { 'Authorization' => "Bearer #{auth_token}" }
        census = nil
        ssh = AutomateCluster::SSH.new
        ssh.connections(service: service) do |type, conn|
          service_name = "automate-ha-#{service}"
          data = conn.curl_hab("/services/#{service_name}/default")
          health = conn.curl_hab("/services/#{service_name}/default/health")
          member_id = data.dig("sys", "member_id")
          census = service_census_data(service_name, member_id, conn)
          info = {
            "member_id" => member_id,
            "health_check" => health,
            "process" => data["process"],
            "role" => role(census),
            "census" => census
          }

          yield conn.ip, info
        end
      end

      def role(data)
        if data['leader']
          'Leader'
        elsif data['follower']
          'Follower'
        else
          'Unknown'
        end
      end

      def leader(service = 'postgresql')
        service_data(service) do |ip, data|
          return [ip, data] if data["census"]["leader"]
        end
      end
    end
  end
end
