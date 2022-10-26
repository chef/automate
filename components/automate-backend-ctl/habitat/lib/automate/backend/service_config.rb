require 'httparty'
require 'erb'
require 'json'
require 'toml-rb'
require_relative '../../core_extensions/net/http'

module Automate
  module Backend
    # ServiceConfig class
    class ServiceConfig
      include HTTParty

      SLEEP_TIMEOUT = 900

      read_timeout 5
      open_timeout 5

      debug_output $stdout if ENV['BACKEND_CTL_LOG_LEVEL'] && ENV['BACKEND_CTL_LOG_LEVEL'] == 'debug'

      attr_accessor :auth, :headers, :service, :group

      def initialize(host, port, service, group = "default")
        self.class.base_uri "https://#{host}:#{port}"
        @service = service
        @group = group
        @auth = {
          token: ENV["HAB_SUP_GATEWAY_AUTH_TOKEN"],
        }
        @headers = {
          "Content-Type" => "application/json",
          "Authorization" => "Bearer #{auth[:token]}",
        }
      end

      def config
        JSON.parse(get("/services/#{service}/#{group}/config").body)
      end

      def toml_config
        TomlRB.dump(config)
      end

      def hab_config_applied
        svc_group = "#{service}.#{group}"
        conf = JSON.parse(get('/census').body)['census_groups'][svc_group]['service_config']
        TomlRB.dump(conf['value']) if conf
      end

      def default_options
        {
          headers: headers,
          digest_auth: auth,
          verify: false
        }
      end

      def get(uri)
        sleep_length = 5
        Log.debug "GET #{self.class.base_uri}#{uri}"
        while true
          begin
            resp = self.class.get(
              uri,
              default_options
            )

            Log.debug "resp code: #{resp.code}"
            case resp.code
            when 200
              break
            when 401
              raise 'HTTP 401 Unauthorized (The HAB_SUP_GATEWAY_AUTH_TOKEN environment variable does not match what the Supervisor HTTP Gateway is expecting.)'
            when 402..599
              Log.warn "HTTP #{resp.code}"
            end
            sleep sleep_length
          rescue Errno::ECONNREFUSED
            raise Errno::ECONNREFUSED if sleep_length > SLEEP_TIMEOUT
            Log.info "Waiting for #{service}.#{group} to come up. Sleeping for #{sleep_length} seconds"
            sleep(sleep_length)
            sleep_length = if sleep_length > 80
              sleep_length + 100
            else
              sleep_length * 2
            end
          end
        end
        resp
      end
    end
  end
end
