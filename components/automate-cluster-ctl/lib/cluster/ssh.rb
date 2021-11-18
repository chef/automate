#
# Author:: Faizan Fulara (<ffulara@progress.com>)
# Copyright:: Copyright (c) Chef Software Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
require 'train'
require 'train/transports/ssh'
require_relative 'terraform/output'

module AutomateCluster
  class SSH
    attr_reader :config, :logger, :tfoutput
    def initialize
      @config = AutomateCluster::Config
      @logger = AutomateCluster.logger
      @tfoutput = AutomateCluster::Terraform.output
    end

    def connections(**options)
      service = options.delete(:service)
      logger.debug "SSH for #{service} nodes only"

      tfoutput.ssh_node_types.each do |type|
        next unless service.nil? || type == service
        tfoutput.fetch("#{type}_private_ips").each do |ip|
          begin
            logger.debug "Connecting to #{ip}"
            ssh = connect(ip, options)
            ssh.wait_until_ready
            yield type, ssh
          rescue Train::TransportError, Train::Transports::SSHFailed => e
            next
          end
        end
      end
    end

    def connect(ip, options)
      SSHConnection.new(ip, options)
    end
  end

  class SSHConnection
    attr_reader :ip, :options

    def initialize(ip, opts = {})
      @ip = ip
      @options = opts.merge({
        host: ip, port: 22, user: AutomateCluster::Config.ssh_user,
        key_files: AutomateCluster::Config.ssh_key_file, connection_timeout: 3, connection_retries: 5,
        connection_retry_sleep: 5, logger: AutomateCluster.logger, verify_host_key: :never
      })

      @ip = ip
      @train = Train.create('ssh', @options)
    rescue Train::TransportError, Train::Transports::SSHFailed => e
      AutomateCluster.logger.error "Error sshing to #{ip}"
      AutomateCluster.logger.error e.message
      raise e
    end

    def connection
      @connection ||= @train.connection
    end

    def wait_until_ready
      connection.wait_until_ready
    end

    def method_missing(method_name, *args)
      connection.send(method_name, *args) if connection.respond_to?(method_name)
    rescue Net::SCP::Error => e
      AutomateCluster.logger.error e.message
      AutomateCluster.logger.debug e.backtrace.join("\n")
    end

    def curl_bin
      "HAB_LICENSE=accept-no-persist hab pkg exec core/curl curl"
    end

    def curl_es(endpoint, opts = {})
      options = { host: 'localhost', port: 9200 }.merge(opts)

      cmd = [ curl_bin, '-s', File.join("http://#{options[:host]}:#{options[:port]}", endpoint) ]

      result = run(cmd, opts)
      if result
        JSON.parse(result.stdout)
      else
        {}
      end
    rescue JSON::ParserError
      {}
    end

    def curl_auth
      "-H \"Authorization: Bearer #{AutomateCluster.hab_info.auth_token}\""
    end

    def curl_hab(endpoint, auth_requred = true)
      cmd = [
        curl_bin, "-sk", curl_auth,
        File.join('https://localhost:9631', endpoint)
      ].compact

      result = run(cmd)
      if result
        JSON.parse(result.stdout)
      else
        {}
      end
    end

    def run(cmd, opts = {})
      opts = { valid_exit_codes: [0] }.merge(opts)
      cmd = Array(cmd).flatten.compact.join(' ')
      result = connection.run_command cmd

      unless opts[:valid_exit_codes].include?(result.exit_status)
        AutomateCluster.logger.error "Command '#{cmd}' failed, exited #{result.exit_status}", hostname: @ip
        AutomateCluster.logger.error "STDOUT: #{result.stdout.force_encoding('UTF-8')}", hostname: @ip unless result.stdout.empty?
        AutomateCluster.logger.error "STDERR: #{result.stderr.force_encoding('UTF-8')}", hostname: @ip unless result.stderr.empty?

        false
      else
        result
      end
    end
  end
end
