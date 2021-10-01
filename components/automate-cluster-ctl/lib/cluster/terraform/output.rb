require_relative 'base'

module AutomateCluster
  module Terraform
    class Output < Base
      include Enumerable

      SSH_NODE_TYPES = %w{ automate elasticsearch chef_server postgresql }.freeze

      attr_reader :output

      def initialize
        super

        output_json = terraform_run('output -json').stdout.chomp
        @output = JSON.parse(output_json)
      end

      def ssh_node_types
        SSH_NODE_TYPES
      end

      def [](name)
        fetch(name)
      end

      def each(&block)
        output.each do |k,v|
          yield k,v['value']
        end
      end

      def keys
        output.keys
      end

      def include?(name)
        output.include?(name)
      end

      def fetch(name)
        output.fetch(name, {}).fetch('value', [])
      end

      def ssh_commands(name, ssh_options = nil)
        return [] unless include?("#{name}_private_ips")

        logger.debug "Fetching data for '#{name}_private_ips'"
        fetch("#{name}_private_ips").map do |ip|
          [
            'ssh',
            ssh_options,
            '-i', config.ssh_key_file,
            "#{config.ssh_user}@#{ip}"
          ].compact.join(' ')
        end
      end

      def inspect
        output.inspect
      end
    end
  end
end
