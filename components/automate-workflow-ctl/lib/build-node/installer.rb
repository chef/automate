require 'logger'
require 'fileutils'
require 'uri'

require 'build-node/config'
require 'build-node/exceptions'
require 'build-node/local_knife'
require 'build-node/local_delivery'
require 'build-node/remote_connection'
require 'ctl-helpers/delivery_config'
require 'ctl-helpers/exceptions'

# This module is the user friendly command line interface to build node installer magic
module BuildNode
  class Installer
    attr_reader :connection, :log_path, :logger, :remote_temp_path, :extension, :stdout, :copied_files

    def initialize(args, log_proc)
      @args = args
      @remote_temp_path = "./tmp-build-node-installer-#{Time.now.to_s.gsub(/\s+/, '')}"
      # Avoid name conflicts with other files already present, and
      # in the case of re-runs.
      @stdout = log_proc
      @copied_files = []
    end

    def configure!()
      options = Config.parse_args!(@args, stdout)
      @config = Config.new(options)
      @config.validate_and_prompt!
      CtlHelpers::DeliveryConfig.validate! # Load and verify delivery.rb settings
      @log_path = "/var/log/delivery-ctl/build-node-install_#{@config.fqdn}.log"
      @local_knife = BuildNode::LocalKnife.new(@config)
      @local_delivery = BuildNode::LocalDelivery.new(@config)
    rescue CtlHelpers::Exceptions::ConfigurationError => e
      # Using puts here since the logger is not initialized yet
      puts "#{e.message}\n#{e.backtrace.join("\n")}"
      exit 1
    end

    def install
      FileUtils.mkdir_p(File.dirname(log_path))
      @logger = Logger.new(log_path)
      @connection = RemoteConnection.new(@config, logger)
      error_exit = true
      logger.info("------- Attempting to configure #{@config.fqdn} as #{@config.username}")
      begin
        stdout.call("\nConnecting to #{@config.fqdn}...")
        connection.connect!
        validate_package!
        install_and_configure
        error_exit = false
      rescue BuildNode::Exceptions::RemoteConnectionFailed,
             BuildNode::Exceptions::RemoteCopyFailed,
             BuildNode::Exceptions::RemoteExecutionFailed => e
        output_error(e)
        more_info
      rescue BuildNode::Exceptions::UnsupportedTargetOS,
             BuildNode::Exceptions::PackageTargetMismatch,
             BuildNode::Exceptions::CertFetchFailed => e
        output_error(e)
      rescue BuildNode::Exceptions::KnifeCommandFailed,
             BuildNode::Exceptions::LegacyNode,
             BuildNode::Exceptions::ClientExists,
             BuildNode::Exceptions::NodeExists => e
        logger.fatal("Error bootstrapping #{@config.fqdn}: #{e.message}")
        if e.remediation_steps
          stdout.call "Failed to register #{@config.fqdn} with Chef Server:"
          stdout.call "  #{e.message}"
          stdout.call "  #{e.remediation_steps}"
        else
          stdout.call "Failed to register #{@config.fqdn} with Chef Server."
          more_info
        end
      rescue BuildNode::Exceptions::DeliveryAPIRequestFailed => e
        stdout.call "Failed to register runner #{@config.fqdn} with Automate:"
        stdout.call "  #{e.message}"
        stdout.call "Please verify the provided values for admin-name, admin-token and enterprise."
      rescue RuntimeError => e
        logger.fatal("Error: #{e.class} #{e.message}:")
        e.backtrace.each { |line| logger.fatal(line) }
        stdout.call "Something unexpected happened."
        more_info
      end
      if error_exit
        remove_sensitive_files
        exit 1
      end
    end

    def validate_package!
      @extension = File.extname(@config.installer)
      os_name = connection.os_name
      case os_name
      when 'ubuntu', 'debian'
        raise BuildNode::Exceptions::PackageTargetMismatch.new(os_name, extension, ".deb") unless extension == ".deb"
      when 'centos', 'redhat', 'suse'
        raise BuildNode::Exceptions::PackageTargetMismatch.new(os_name, extension, ".rpm") unless extension == ".rpm"
      else
        raise BuildNode::Exceptions::UnsupportedTargetOS.new(os_name)
      end
    end

    def install_and_configure
      fetch_local_certificates
      node_precheck
      existing_client_d_move
      create_temp_path
      copy_chefdk
      copy_base_files
      copy_keys
      install_chefdk!
      # The Chef Server SSL certificate is required for knife commands used
      # during bootstrap. While we're fetching one certificate, may as well
      # fetch 'em all.
      fetch_certificates
      register_node!
      configure_node
      configure_job_dispatch
      cleanup
    end

    def node_precheck
      @local_knife.verify_node_not_legacy
    end

    def fetch_local_certificates
      stdout.call("Fetching Chef Server certificates for local use")
      @local_knife.fetch_ssl_certificates
    end

    def existing_client_d_move
      move_command = "sh -c ' if [ -f /etc/chef/client.d/runner_install.rb ]; then mv /etc/chef/client.d/runner_install.rb /etc/chef/client.d/runner_install.rb.old; fi'"
      run_command("Backing up any existing runner specific Chef Client configuration.", move_command)
    end

    def create_temp_path
      # All commands will run as sudo - which will cause scp to fail in later steps, so we'll change ownership
      # of the temp directory as part of setup.
      run_command("Creating temporary install directory", "mkdir #{remote_temp_path}")
      run_command("Setting ownership to #{@config.username}", "chown #{@config.username} #{remote_temp_path}")
    end

    def copy_chefdk
      copy_file("ChefDK package",
                @config.installer, "#{remote_temp_path}/chef_dk#{extension}")
    end

    def copy_base_files()
      copy_file("installation files",
                "/opt/delivery/embedded/service/omnibus-ctl/installer", "#{remote_temp_path}")
    end

    def copy_keys
      # NOTE In a future iteration, we'll probably need to take this path as an option too
      # as there's no predetermined location for this file.  For now, our install doc requires it to be placed in
      # the specified path
      copy_file("builder key",
                "/etc/delivery/builder_key", "#{remote_temp_path}/installer/builder_key")

      # NOTE This can be generalized by reading the /etc/delivery/delivery.rb
      # configuration file and parsing out delivery['chef_private_key'].
      copy_file("delivery chef private key",
                "/etc/delivery/delivery.pem", "#{remote_temp_path}/installer/delivery.pem")
    end

    def install_chefdk!
      message = "Installing ChefDK on build node..."
      case connection.os_name
      when 'ubuntu', 'debian'
        run_command(message, "dpkg -i #{remote_temp_path}/chef_dk.deb")
      when 'centos', 'redhat', 'suse'
        run_command(message, "rpm -Uvh --replacepkgs #{remote_temp_path}/chef_dk.rpm")
      end
    end

    def register_node!
      override_tag = "delivery-job-runner" if @config.job_dispatch_version == "v2"
      begin
      stdout.call("Registering #{@config.fqdn} with the Chef Server")
      @local_knife.bootstrap(tag: override_tag)
      rescue BuildNode::Exceptions::AlreadyRegistered => e
        raise unless @config.overwrite_registration?
        @local_knife.bootstrap(overwrite: true, tag: override_tag)
      end
    end

    def configure_node
      run_command("Installing components on #{@config.fqdn}...",
                  "sh -c 'cd #{remote_temp_path}/installer && ./install-build-node.sh'")
    end

    def fetch_certificates
      run_command("Creating /etc/chef/trusted_certs", "mkdir -p /etc/chef/trusted_certs")
      # Note: this `== true` looks odd, but is correct: if it has not been set,
      #       we'd still get NestedHash.new here, which is truthy
      hosts = if CtlHelpers::DeliveryConfig.delivery[:chef_server_proxy] == true
                []
              else
                [URI.parse(CtlHelpers::DeliveryConfig.delivery[:chef_server]).host]
              end
      hosts += [CtlHelpers::DeliveryConfig.delivery_fqdn].flatten
      hosts << CtlHelpers::DeliveryConfig.delivery[:supermarket_fqdn]
      hosts.each do |fqdn|
        fetch_certificate(fqdn) if !CtlHelpers::DeliveryConfig.value_defaulted?(fqdn)
      end
    end

    def fetch_certificate(fqdn)
      # Knife's "ssl fetch" saves certificates with "."s replaced with "_"s.
      # Follow this pattern so we don't double-fetch certificates.
      cmd = "sh -c 'openssl s_client -showcerts -connect #{fqdn}:443" \
        " </dev/null 2> /dev/null | openssl x509 -outform PEM >" \
        " /etc/chef/trusted_certs/#{fqdn.gsub(".", "_")}.crt'"
      run_command("Fetching SSL certificate from #{fqdn}", cmd)
    end

    def configure_job_dispatch
      case @config.job_dispatch_version
      when "v1"
        configure_push_client
      when "v2"
        configure_job_runner
      end
    end

    def configure_push_client
      run_command("Configuring push jobs client on #{@config.fqdn}...",
                  "sh -c 'cd #{remote_temp_path}/installer && sh -c ./gen_push_config.sh'")
    end

    def configure_job_runner
      runner_info_hash = @local_knife.gather_node_os_info_hash
      runner_info_hash['hostname'] = @config.fqdn
      runner = @local_delivery.create_runner(runner_info_hash)
      pubkey = runner['openssh_public_key']
      run_command("Configuring #{@config.fqdn} as job runner...",
                  "chef-apply #{remote_temp_path}/installer/install-job-runner.rb \'#{pubkey}\'")
    end

    def cleanup
      run_command("Removing temporary files", "rm -rf #{remote_temp_path}/")
    end

    def sensitive_files
      ["builder_key", "delivery.pem"]
    end

    def remove_sensitive_files
      remaining_files = []
      copied_files.each do |file|
        basename = File.basename(file)
        next unless sensitive_files.include?(basename)
        begin
          run_command("Attempting to remove #{basename}", "rm #{file}")
        rescue BuildNode::Exceptions::RemoteExecutionFailed,
          BuildNode::Exceptions::RemoteConnectionFailed
          remaining_files << file
        end
      end

      unless remaining_files.empty?
        subject, object = remaining_files.length == 1 ? ["file", "it"] : ["files", "them"]
        stdout.call("I was unable to remove the following sensitive #{subject} from the build node:")
        stdout.call("")
        remaining_files.each { |path| stdout.call(" * #{path}") }
        stdout.call("")
        stdout.call("Please log into #{@config.fqdn} and remove #{object}.")
      end
    end

    def run_command(description, command)
      stdout.call(description)
      connection.run_command(command)
    end

    def copy_file(description, from, to)
      stdout.call("Copying #{description}...")
      connection.copy_file(from, to)
      copied_files << to
    end

    def output_error(ex)
      logger.fatal("Received error #{ex.class} with message #{ex.message}")
      stdout.call ex.message
      if ex.remediation_steps
        stdout.call ""
        stdout.call ex.remediation_steps
      end
    end

    private

    def more_info
      stdout.call ""
      stdout.call "Please check the log files at #{log_path} for more information"
    end
  end
end
