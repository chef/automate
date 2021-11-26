require 'thor'
require_relative 'backend_utils.rb'

module AutomateCluster
  module SmokeTest
    class CLI < Thor
      default_command :start

      class_option :verbose, type: :boolean, default: false, desc: 'Set output to DEBUG level.'
      class_option :live_stream, type: :boolean, default: true, desc: 'Live stream STDOUT'
      class_option :sudo, type: :boolean, default: true, desc: 'Run with sudo.'
      class_option :sudo_password, type: :string, lazy_default: -1, desc: 'Specify a sudo password, if it is required.'
      class_option :sudo_options, type: :string, desc: 'Additional sudo options for remote.'
      class_option :sudo_command, type: :string, default: 'sudo', desc: 'Alternate command for sudo.'

      desc 'start', 'Start running Inspec smoke tests'
      def start
        utils = BackendUtils::Common.new(options)
        password_option = options[:sudo_password] ? '--sudo-password=' + options[:sudo_password] : nil
        sudo_cmd_option = options[:sudo_command] ? '--sudo-command=' + options[:sudo_command] : nil
        smoke_log_level = options[:verbose] ? '--log-level=debug' : '--log-level=info'
        base_inspec_cmd = "CHEF_LICENSE=accept-no-persist $(hab pkg path chef/inspec)/bin/inspec exec --sudo #{sudo_cmd_option} #{password_option} --key-files #{utils.ssh_key_file} #{smoke_log_level} --show-progress"

        utils.backend_logger.info ">>>> Running Automate Frontend inspec smoke tests against: #{utils.automate_private_ips}"
        utils.automate_private_ips.each do |ip|
          begin
            cmd = "#{base_inspec_cmd} --target ssh://#{utils.ssh_user}@#{ip} #{utils.top_level_dir}/inspec/automate-frontend-smoke/"
            utils.backend_logger.debug cmd
            result = utils.run_command cmd
            utils.backend_logger.info "STDOUT: #{result.stdout}" unless options[:live_stream]
          rescue BackendUtils::Common::MixlibCmdErr
            next
          end
        end

        utils.backend_logger.info ">>>> Running Automate Chef Server inspec smoke tests against: #{utils.chef_server_private_ips}"
        utils.chef_server_private_ips.each do |ip|
          begin
            cmd = "#{base_inspec_cmd} --target ssh://#{utils.ssh_user}@#{ip} #{utils.top_level_dir}/inspec/automate-frontend-chef-server-smoke/"
            utils.backend_logger.debug cmd
            result = utils.run_command cmd
            utils.backend_logger.info "STDOUT: #{result.stdout}" unless options[:live_stream]
          rescue BackendUtils::Common::MixlibCmdErr
            next
          end
        end

        utils.backend_logger.info ">>>> Running Automate PostgreSQL inspec smoke tests against: #{utils.postgresql_private_ips}"
        utils.postgresql_private_ips.each do |ip|
          begin
            cmd = "#{base_inspec_cmd} --target ssh://#{utils.ssh_user}@#{ip} #{utils.top_level_dir}/inspec/automate-backend-postgresql-smoke/"
            utils.backend_logger.debug cmd
            result = utils.run_command cmd
            utils.backend_logger.info "STDOUT: #{result.stdout}" unless options[:live_stream]
          rescue BackendUtils::Common::MixlibCmdErr
            next
          end
        end

        utils.backend_logger.info ">>>> Running Automate Elasticsearch inspec smoke tests against: #{utils.elasticsearch_public_ips}"
        utils.elasticsearch_public_ips.each do |ip|
          begin
            cmd = "#{base_inspec_cmd} --target ssh://#{utils.ssh_user}@#{ip} #{utils.top_level_dir}/inspec/automate-backend-elasticsearch-smoke/"
            utils.backend_logger.debug cmd
            result = utils.run_command cmd
            utils.backend_logger.info "STDOUT: #{result.stdout}" unless options[:live_stream]
          rescue BackendUtils::Common::MixlibCmdErr
            next
          end
        end
      end
    end
  end
end
