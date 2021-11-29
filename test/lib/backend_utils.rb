require 'fileutils'
require 'json'
require 'logger'
require 'mixlib/shellout'
require 'securerandom'
require 'train'
require 'train/transports/ssh'
require 'toml-rb'

# Metrics/LineLength
module BackendUtils
  # common test methods
  class Common
    class MixlibCmdErr < RuntimeError
      def initialize(msg='Command Failed')
        super
      end
    end

    attr_accessor :repo_path, :backend_logger, :md5_table_created

    def initialize(options = {})
      Encoding.default_external = Encoding::UTF_8
      Encoding.default_internal = Encoding::UTF_8
      @repo_path = options['path'] ? File.expand_path(path) : File.expand_path(top_level_dir)
      @md5_table_created = false
      @backend_logger = Logger.new(STDOUT)
      options['verbose'] == true ? @backend_logger.level = Logger::DEBUG : @backend_logger.level = Logger::INFO
      create_test_db(pg_promoted_leader) if options['createdb'] == true
      @backend_logger.debug "Initialized BackendUtils::Common with top-level: #{repo_path}"
      @live_stream = options['live_stream'] == true ? true : false
    end

    def top_level_dir
      @top_level_dir ||= File.expand_path(File.dirname(__FILE__) + '/../../')
    end

    def run_command(cmd, opts = {})
      # this avoids forcing any pre-exising bundler settings on commands that are executed
      opts ||= {}
      opts[:environment] = {  HAB_LICENSE: 'accept-no-persist',
                              GEM_HOME: '',
                              GEM_PATH: '',
                              RUBYOPT: '',
                              BUNDLE_GEMFILE: '',
                              BUNDLE_BIN_PATH: '' }

      # check to see if the live_stream was disabled for this run_command call
      opts[:live_stream] ||= STDOUT if opts[:live_stream].nil? && @live_stream
      so = Mixlib::ShellOut.new(cmd, opts)
      so.run_command
      so.error!
      so
    rescue Mixlib::ShellOut::ShellCommandFailed => e
      @backend_logger.error "encountered exception!"
      if so.stderr
        message = "***********\n"
        message += "#{e}"
        message += "***********\n"
      end
      raise MixlibCmdErr, message
    end

    def tf_plan_dir
      @tf_plan_dir ||= File.expand_path(File.join(@top_level_dir, 'terraform'))
    end

    def accept_license_no_persist
      'HAB_LICENSE=accept-no-persist'
    end

    def terraform_bin
      "\$(#{accept_license_no_persist} hab pkg path core/terraform)/bin/terraform"
    end

    def jq_local_bin
      'jq'
    end

    def jq_bin
      "\$(#{accept_license_no_persist} hab pkg path core/jq-static)/bin/jq"
    end

    def curl_bin
      "\$(#{accept_license_no_persist} hab pkg path core/curl)/bin/curl"
    end

    def tf_exec
      "cd #{tf_plan_dir} && #{terraform_bin}"
    end

    def tf_state_pull
      "#{tf_exec} state pull"
    end

    def sudo_cmd
      ENV['SUDO_CMD'] || 'sudo'
    end

    def frontend_ssh_connect_string
      # returns string like: "ssh -i ~/.ssh/jmiller centos@54.193.49.151"
      tf_output_value("automate_ssh").first
    end

    def ssh_user
      @ssh_user ||= / (\S+)@\S+/.match(frontend_ssh_connect_string)[1]
    end

    def ssh_key_file
      @ssh_key_file ||= tf_output_value("ssh_key_file")
    end

    def train(host_ip, options = {})
      opts = { host: host_ip, port: 22, user: ssh_user,
               key_files: ssh_key_file, connection_timeout: 3, connection_retries: 5,
               connection_retry_sleep: 5, logger: backend_logger, verify_host_key: :never }
      Train.create('ssh', opts.merge(options))
    end

    def each_alive(host_ips, options = {})
      host_ips.each do |ip|
        begin
          conn = train(ip, options).connection
          conn.wait_until_ready
        rescue Train::TransportError, Train::Transports::SSHFailed => e
          @backend_logger.warn "Caught exception: #{e} for #{ip}"
          next
        end
        yield conn
      end
    end

    def terraform_output
      @terraform_output ||= JSON.parse(run_command("#{tf_exec} output -json", live_stream: false).stdout.chomp)
    end

    def tf_output_value(name)
      terraform_output[name]["value"]
    end

    def automate_private_ips
      tf_output_value("automate_private_ips")
    end

    def chef_server_private_ips
      tf_output_value("chef_server_private_ips")
    end

    def postgresql_private_ips
      tf_output_value("postgresql_private_ips")
    end

    def elasticsearch_private_ips
      tf_output_value("elasticsearch_private_ips")
    end

    def elasticsearch_public_ips
      tf_output_value("elasticsearch_public_ips")
    end

    def hab_config_path
      File.join(repo_path, 'terraform/a2ha_habitat.auto.tfvars')
    end

    def hab_config_vars
      @hab_config_vars ||= TomlRB.parse(File.read(hab_config_path))
    end

    def hab_sup_http_gateway_auth_token
      hab_config_vars['hab_sup_http_gateway_auth_token']
    end

    def pg_hab_elected_leader
      each_alive(postgresql_private_ips) do |conn|
        cmd = "#{curl_bin} -sk -H \"Authorization: Bearer #{hab_sup_http_gateway_auth_token}\" https://localhost:9631/census | #{jq_bin} \
          '.census_groups.\"automate-ha-postgresql.default\".population[] | select \
          (.leader == true) | \"\\(.sys.gossip_ip)\"'"
        @backend_logger.debug "Running: #{cmd}"
        output = conn.run_command(cmd).stdout.chomp
        @backend_logger.debug "Command Output: #{output}"
        conn.close
        next if output.nil? || output == ''

        ip = JSON.parse(output)
        index = index_of(postgresql_private_ips, ip)
        next unless index

        leader = postgresql_private_ips[index]
        @backend_logger.info "/census says Habitat elected PostgreSQL topology leader is: #{leader}"
        return leader
      end
      raise 'could not determine pg_hab_elected_leader!'
    end

    def pg_promoted_leader
      index = 0
      each_alive(postgresql_private_ips) do |conn|
        output = conn.run_command("#{curl_bin} -s localhost:6432 | #{jq_bin} .Status").stdout.chomp
        conn.close
        next if output.nil? || output == ''
        status = JSON.parse(output)
        @backend_logger.info "#{conn.hostname} pgleaderchk: #{output} NOTE: replicas will show critical"
        if status == 'ok'
          leader = postgresql_private_ips[index]
          @backend_logger.info "Read/Write PostgreSQL leader: #{leader}"
          return leader
        end
        index += 1
      end
      raise 'could not determine pg_promoted_leader!'
    end

    def pg_hab_elected_replicas
      postgresql_private_ips - [pg_hab_elected_leader]
    end

    def pg_demoted_replicas
      postgresql_private_ips - [pg_promoted_leader]
    end

    def index_of(arr, host_ip)
      index = 0
      arr.each do |item|
        return index if item.include?(host_ip)
        index += 1
      end
      nil
    end

    def ping_until_alive(host_ips)
      host_ips = [host_ips] unless host_ips.is_a?(Array)
      each_alive(host_ips, connection_retries: 6, connection_retry_sleep: 30) do |conn|
        cmd = 'whoami'
        conn.run_command(cmd)
        conn.close
        @backend_logger.info "#{conn.hostname} detected up"
      end
    end

    def reboot(host_ip)
      each_alive([host_ip], connection_retries: 0) do |conn|
        begin
          @backend_logger.warn "rebooting #{host_ip}"
          cmd = "echo ENV['SUDO_PASSWORD'] | #{sudo_cmd} -S reboot"
          conn.run_command(cmd)
          conn.close
        rescue Train::TransportError, Train::Transports::SSHFailed,
               IOError, Net::SSH::ConnectionTimeout, Errno::ECONNREFUSED => _ex
          @backend_logger.warn 'caught expected exception on shutdown'
        end
      end
    end

    def restart_svc(host_ip, svc)
      @backend_logger.warn "restarting #{svc} on #{host_ip}"
      each_alive([host_ip]) do |conn|
        begin
          cmd = "echo ENV['SUDO_PASSWORD'] | #{sudo_cmd} -S hab svc stop #{svc} && sleep 10 && echo ENV['SUDO_PASSWORD'] | #{sudo_cmd} -S hab svc start #{svc}"
          @backend_logger.info cmd
          res = conn.run_command(cmd)
          conn.close
          next if res.exit_status.zero?
          raise "unexpected failure stopping svc #{svc} #{res.stdout.chomp} #{res.stderr.chomp}"
        rescue Train::TransportError, Train::Transports::SSHFailed,
               IOError, Net::SSH::ConnectionTimeout, Errno::ECONNREFUSED => _ex
          @backend_logger.warn 'caught exception on svc restart'
        end
      end
    end

    def rolling_reboot(host_ips, options = {})
      host_ips.each do |host|
        reboot(host)
        ping_until_alive([host]) unless options[:fast]
      end
    end

    def rolling_svc_restart(host_ips, svc, options = {})
      host_ips.each do |host|
        restart_svc(host, svc)
        ping_until_alive([host]) unless options[:fast]
      end
    end

    def pg_bin_path
      pg_bin_path = "\$(#{accept_license_no_persist} hab pkg path core/postgresql11)/bin"
      pg_pw = "PGPASSWORD=$(echo ENV['SUDO_PASSWORD'] | #{sudo_cmd} -S cat /hab/svc/automate-ha-postgresql/config/pwfile)"
      @pg_bin_path ||= "#{pg_pw} #{pg_bin_path}"
    end

    def test_db_name
      'backend_tests'
    end

    def test_md5_table
      't_md5'
    end

    def create_test_db(host_ip)
      each_alive([host_ip]) do |conn|
        @backend_logger.info "Creating test db on #{conn.hostname}"
        cmd = "#{pg_bin_path}/createdb -h 127.0.0.1 -p 7432 -U admin -w #{test_db_name} || true"
        @backend_logger.info cmd
        res = conn.run_command(cmd)
        @backend_logger.info "#{res.stdout.chomp} #{res.stderr.chomp}"
        conn.close
      end
    end

    def create_test_md5_table(host_ip)
      series_max = SecureRandom.random_number(600)
      drop_table_sql = "DROP TABLE IF EXISTS #{test_md5_table}"
      random_data_sql = "CREATE TABLE #{test_md5_table} AS SELECT s, md5(random()::text) FROM generate_Series(1,#{series_max}) s;"
      each_alive([host_ip]) do |conn|
        @backend_logger.info "Dropping table #{test_md5_table} if exists"
        cmd = "#{pg_bin_path}/psql -h 127.0.0.1 -p 7432 -U admin -d #{test_db_name} -c '#{drop_table_sql}'"
        @backend_logger.info cmd
        res = conn.run_command(cmd)
        @backend_logger.info res.stdout.chomp.to_s

        @backend_logger.info "Creating table #{test_md5_table}"
        cmd = "#{pg_bin_path}/psql -h 127.0.0.1 -p 7432 -U admin -d #{test_db_name} -c '#{random_data_sql}'"
        @backend_logger.info cmd
        res = conn.run_command(cmd)
        @backend_logger.info res.stdout.chomp.to_s
        conn.close
      end
      @md5_table_created = true
    end

    def pg_md5(host_ip)
      hashsum = SecureRandom.base64
      create_test_md5_table(host_ip) unless md5_table_created
      each_alive([host_ip]) do |conn|
        @backend_logger.info 'Generating md5..'
        # https://stackoverflow.com/a/13948327
        sql_cmd = "SELECT md5(CAST((array_agg(f.* order by s)) AS text)) FROM #{test_md5_table} f"
        cmd = "#{pg_bin_path}/psql -h 127.0.0.1 -p 7432 -U admin -d #{test_db_name} -c '#{sql_cmd}'"
        @backend_logger.info cmd
        res = conn.run_command(cmd)
        @backend_logger.info "#{res.stdout.chomp} #{res.stderr.chomp}"
        hashsum = res.stdout.chomp if res.stdout =~ /1 row/
        conn.close
      end
      hashsum
    end

    def run_pgbench(host_ips)
      return_codes = []
      host_ips = [host_ips] unless host_ips.is_a?(Array)
      each_alive(host_ips) do |conn|
        @backend_logger.info "Running PgBench on #{conn.hostname}"
        cmd = "#{pg_bin_path}/pgbench -h 127.0.0.1 -p 7432 -U admin -d -s 15 -i #{test_db_name}"
        @backend_logger.info cmd
        res = conn.run_command(cmd)
        return_codes.push(res.exit_status)
        @backend_logger.info "#{res.stdout.chomp} #{res.stderr.chomp}"
        conn.close
      end
      return_codes
    end
  end
end
