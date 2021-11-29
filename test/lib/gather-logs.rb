require 'fileutils'
require 'date'
require 'thor'
require_relative 'backend_utils.rb'

module AutomateCluster
  module GatherLogs
    class CLI < Thor
      default_command :start

      class_option :verbose, type: :boolean, desc: 'Set output to DEBUG level.'
      class_option :sudo, type: :boolean, default: true, desc: 'Run with sudo.'
      class_option :sudo_password, type: :string, lazy_default: -1, desc: 'Specify a sudo password, if it is required.'
      class_option :sudo_options, type: :string, desc: 'Additional sudo options for remote.'
      class_option :sudo_command, type: :string, default: 'sudo', desc: 'Alternate command for sudo.'
      class_option :remote_log_dir, type: :string, desc: 'Where to temporarily store remote log files', default: '/var/tmp'
      class_option :local_log_dir, type: :string, desc: 'Where to store the logs locally', default: '/var/tmp'
      class_option :log_lines, type: :numeric, desc: 'How many journalctl lines to capture', default: 500000

      desc 'start', 'Start gathering Chef Automate Cluster logs'
      def start
        utils = BackendUtils::Common.new(options)
        sudo_cmd = options[:sudo_command]
        sudo_pass = options[:sudo_password]
        sudo_options = options[:sudo_options]
        remote_log_dir = options[:remote_log_dir]
        local_log_dir = options[:local_log_dir]
        log_prefix = Time.now.strftime('%Y%m%d%H%M%S')
        remote_file = "#{remote_log_dir}/#{log_prefix}-logs.tar.gz"
        log_lines = options[:log_lines]
        gather_cmd = "echo #{sudo_pass} | #{sudo_cmd} #{sudo_options} -S chef-automate gather-logs #{remote_file} -l -o --no-check-version --log-lines #{log_lines} 2>&1 | grep -v 'Failed to fetch URL' && echo #{sudo_pass} | #{sudo_cmd} #{sudo_options} -S chmod 644 #{remote_file}"
        cleanup_cmd = "echo #{sudo_pass} | #{sudo_cmd} #{sudo_options} -S rm -f #{remote_file}"
        output_dir = "#{local_log_dir}/#{log_prefix}"
        FileUtils.mkdir_p output_dir
        hosts = utils.automate_private_ips + utils.chef_server_private_ips + utils.elasticsearch_private_ips + utils.postgresql_private_ips
        utils.each_alive(hosts) do |conn|
          utils.backend_logger.info ">>>> Gathering logs from remote #{conn.hostname}"
          utils.backend_logger.debug "Running: #{gather_cmd}"
          result = conn.run_command(gather_cmd)
          utils.backend_logger.debug "STDOUT: #{result.stdout}\nSTDERR: #{result.stderr}\nEXIT_STATUS: #{result.exit_status}"
          dest = "#{output_dir}/#{conn.hostname}-logs.tar.gz"
          utils.backend_logger.info "SCP'ing logs locally to: #{dest}"
          conn.download(remote_file, dest)
          utils.backend_logger.debug "Running: #{cleanup_cmd}"
          result = conn.run_command(cleanup_cmd)
          utils.backend_logger.debug "STDOUT: #{result.stdout}\nSTDERR: #{result.stderr}\nEXIT_STATUS: #{result.exit_status}"
          conn.close
        end
        puts "\n☞  Finished. Your log files to send to support are in: #{output_dir}\n\n"
      end
    end
  end
end
