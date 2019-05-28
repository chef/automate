require 'logger'
require 'fileutils'
require 'uri'

require 'runner/install/config'
require 'runner/chefdk_package'
require 'runner/exceptions'
require 'runner/local_knife'
require 'runner/manage_runner_ctl'
require 'runner/remote_connection'

require 'runner/delete/config'

require 'ctl-helpers/delivery_config'
require 'ctl-helpers/exceptions'
require 'ctl-helpers/color_printer'

# This module is the user friendly command line interface to runner installer magic
module Runner
  class ManageRunner

    # overrides puts / print
    include CtlHelpers::ColorPrinter

    attr_reader :args, :connection, :copied_files, :description,
                :log_path, :logger, :remote_temp_path, :action

    FALLBACK_PATH = ["/usr/local/sbin",
                  "/usr/local/bin",
                  "/usr/sbin",
                  "/usr/bin",
                  "/sbin",
                  "/bin"].join(":")

    def initialize(args, description)
      @args = args
      @description = description
      @remote_temp_path = "./tmp-runner-installer-#{Time.now.to_s.gsub(/\s+/, '')}"
      @copied_files = []
    end

    def configure!
      call_with_rescues do
        @config = Install::Config.new(args, description)
        @config.validate!

        @log_path = "/var/log/delivery-ctl/runner-install_#{@config.fqdn}.log"
        log_dir = File.dirname(log_path)
        FileUtils.mkdir_p(log_dir) unless File.exists?(log_dir)
        @logger = Logger.new(log_path)

        @config.needs_sudo! if RemoteConnection.new(@config, @logger).need_sudo?

        CtlHelpers::DeliveryConfig.validate! # Load and verify delivery.rb settings
        @local_knife = Runner::LocalKnife.new(@config.fqdn)

        puts_top_level_step "Preflight check"
        preflight_check

        @config.confirm_config(@local_knife.node_registration_status)

        @client = Runner::ManageRunnerCtl.new(@config)
      end
    end

    def install
      @connection = RemoteConnection.new(@config, logger)
      logger.info("------- Attempting to configure #{@config.fqdn} as #{@config.username}")

      call_with_rescues do
        puts_top_level_step "Connecting to #{@config.fqdn} and retrieving information about your node"
        connection.connect!
        puts_substep "Connected to #{@config.fqdn}."
        puts_substep "Retrieved os info. Platform: #{@connection.os_name} Version: #{@connection.os_release}"

        chefdk_filepath = ensure_chefdk_package
        install_and_configure(chefdk_filepath)
      end
    end

    def delete
      call_with_rescues("delete") do
        @config = Delete::Config.new(args, description)
        @config.validate!

        @client = Runner::ManageRunnerCtl.new(@config)
        runner_info_hash = {}
        runner_info_hash['hostname'] = @config.fqdn
        @client.validate_runner(runner_info_hash)

        # Confirm config with user after checking that runner and enterprise exist.
        @config.confirm_config
        @client.delete_runner(runner_info_hash)
      end
    end

    def call_with_rescues(action = "install", &block)
      begin
        error_exit = true
        block.call
        error_exit = false
      rescue Runner::Exceptions::RemoteConnectionFailed,
             Runner::Exceptions::RemoteCopyFailed,
             Runner::Exceptions::RemoteExecutionFailed => e
        output_error(e)
        more_info
      rescue Runner::Exceptions::UnsupportedTargetOS,
             Runner::Exceptions::PackageTargetMismatch,
             Runner::Exceptions::CertFetchFailed,
             Runner::Exceptions::CustomCertfileNotFound,
             CtlHelpers::Exceptions::ConfigurationError,
             Runner::Exceptions::BadArgumentError => e
        output_error(e, true)
      rescue Runner::Exceptions::KnifeCommandFailed,
             Runner::Exceptions::LegacyNode => e
        logger.fatal("Error bootstrapping #{@config.fqdn}: #{e.message}") if logger
        if e.remediation_steps
          puts "Failed to register #{@config.fqdn} with Chef Server:"
          puts "  #{e.message}"
          puts "  #{e.remediation_steps}"
        else
          puts "Failed to register #{@config.fqdn} with Chef Server."
          more_info
        end
      rescue Runner::Exceptions::RunnerCtlFailed => e
        puts "Failed to configure runner #{@config.fqdn}:" if @action.eql?("install")
        puts "  #{e.message}"
      rescue Runner::Exceptions::ChefDK404
        puts_error "You requested ChefDK version #{@config.chefdk_version} via --chefdk-version but the package did not exist.\nFor a valid list of versions for your platform, see https://downloads.chef.io/chef-dk/."
      rescue Runner::Exceptions::ChefDKHTTPError
        puts "An unexpected error occurred when trying to download the latest ChefDK. Please try again."
      rescue RuntimeError => e
        if logger
          logger.fatal("Error: #{e.class} #{e.message}:")
          e.backtrace.each { |line| logger.fatal(line) }
        end
        puts "Something unexpected happened."
        more_info
      end

      if error_exit
        remove_sensitive_files
        exit 1
      end
    end

    def ensure_chefdk_package
      if @config.installer.nil?
        version = @config.chefdk_version || "latest"
        puts_top_level_step "Downloading the #{version} version of the ChefDK. This make take a few minutes"
        filepath = Runner::ChefDKPackage.new(@config.chefdk_version,
                                             connection.os_name,
                                             connection.os_release).download
        puts_substep "Package successfully downloaded to #{filepath}."

        filepath
      else
        puts_top_level_step "Validating local ChefDK package"
        validate_package!(@config.installer)
        puts_substep "Local package is valid for your your target system and version."

        @config.installer
      end
    end

    def validate_package!(chefdk_filepath)
      extension = File.extname(chefdk_filepath)
      os_name = connection.os_name
      case os_name
      when 'ubuntu', 'debian'
        raise Runner::Exceptions::PackageTargetMismatch.new(os_name, extension, ".deb") unless extension == ".deb"
      when *Runner::ChefDKPackage::ENTERPRISE_LINUX, 'suse'
        raise Runner::Exceptions::PackageTargetMismatch.new(os_name, extension, ".rpm") unless extension == ".rpm"
      when 'mac_os_x'
        raise Runner::Exceptions::PackagetargetMismatch.new(os_name, extension, ".dmg") unless extension == ".dmg"
      else
        raise Runner::Exceptions::UnsupportedTargetOS.new(os_name)
      end
    end

    def install_and_configure(chefdk_filepath)
      puts_top_level_step "Check existing key"
      existing_key_backup
      puts_top_level_step "Backing up any existing runner specific Chef Client configuration."
      existing_client_d_move
      puts_top_level_step "Preparing files necessary for installation"
      create_temp_path
      copy_chefdk(chefdk_filepath)
      copy_base_files
      copy_keys
      copy_custom_certificate_chain

      puts_top_level_step "Installing the ChefDK on runner"
      install_chefdk!

      # The Chef Server SSL certificate is required for knife commands used
      # during bootstrap. While we're fetching one certificate, may as well
      # fetch 'em all.
      puts_top_level_step "Fetching Automate certificates to runner"
      fetch_certificates

      puts_top_level_step "Registering runner to Chef Server"
      register_node!

      puts_top_level_step "Registering node as an Automate Runner and installing software"

      configure_node
      configure_job_runner

      puts_top_level_step "Restore validation key"
      restore_backup_key

      puts_top_level_step "Cleaning up"
      cleanup
    end

    def preflight_check
      puts_substep "Fetching Chef Server certificates for local use."
      @local_knife.fetch_ssl_certificates
      puts_substep "Verifying no legacy runners exist for this name."
      @local_knife.verify_node_not_legacy
    end

    def create_temp_path
      # All commands will run as sudo - which will cause scp to fail in later steps,
      # so we'll change ownership of the temp directory as part of setup.
      run_command("mkdir #{remote_temp_path}")
      puts_substep "Created temporary install directory."

      run_command("chown #{@config.username} #{remote_temp_path}")
      puts_substep "Set ownership to user #{@config.username}."
    end

    def copy_chefdk(chefdk_filepath)
      extension = File.extname(chefdk_filepath)
      filepath = "#{remote_temp_path}/chef_dk#{extension}"
      copy_file(chefdk_filepath, filepath)
      puts_substep "Copied ChefDK to temporary directory."
    end

    def existing_key_backup
      if File.exist?('/etc/chef/validation.pem')
        puts_substep "Taking backup of existing key."
        File.rename('/etc/chef/validation.pem', '/etc/chef/validation.pem.bak')
        @key_backup = true
      end
    end

    def existing_client_d_move
      run_command("sh -c ' if [ -f /etc/chef/client.d/runner_install.rb ]; then mv /etc/chef/client.d/runner_install.rb /etc/chef/client.d/runner_install.rb.old; fi'")
    end

    def base_files
      if ENV['OMNIBUS_FILES'] != nil
        "#{ENV['OMNIBUS_FILES']}/installer"
      else
        "/opt/delivery/embedded/service/omnibus-ctl/installer"
      end
    end

    def copy_base_files
      copy_file(base_files, "#{remote_temp_path}")
      puts_substep "Copied installer software to temporary directory."
    end

    def builder_key_path
      ENV['BUILDER_KEY_PATH'] || "/etc/delivery/builder_key"
    end

    def private_key_path
      CtlHelpers::DeliveryConfig.delivery[:chef_private_key]
    end

    def copy_keys
      # NOTE In a future iteration, we'll probably need to take this path as an option too
      # as there's no predetermined location for this file.  For now, our install doc requires it to be placed in
      # the specified path
      copy_file(builder_key_path, "#{remote_temp_path}/installer/builder_key")

      # NOTE This can be generalized by reading the /etc/delivery/delivery.rb
      # configuration file and parsing out delivery['chef_private_key'].
      copy_file(private_key_path, "#{remote_temp_path}/installer/delivery.pem")
      puts_substep "Copied keys to temporary directory."
    end

    def install_chefdk!
      case connection.os_name
      when 'ubuntu', 'debian'
        run_command("dpkg -i #{remote_temp_path}/chef_dk.deb")
        puts_substep "Installation via dpkg completed."
      when *Runner::ChefDKPackage::ENTERPRISE_LINUX, 'suse'
        run_command("rpm -Uvh --replacepkgs #{remote_temp_path}/chef_dk.rpm")
        puts_substep "Installation via rpm completed."
      when 'mac_os_x'
        run_command("hdiutil mount #{remote_temp_path}/chef_dk.dmg")
        run_command("find /Volumes/Chef\\ Development\\ Kit/*.pkg -exec installer -pkg {} -target '/' \\;")
        run_command("hdiutil unmount '/Volumes/Chef Development Kit'")
        puts_substep "Installation via dmg completed"
      end
    end

    def register_node!
      @local_knife.bootstrap(@config.username, @config.password, @config.ssh_identity_file, @config.sudo, @config.client_config, @config.port)
      puts_substep "Registered #{@config.fqdn}."
    end

    def configure_node
      run_command("sh -c 'cd #{remote_temp_path}/installer && ./install-build-node.sh'")
      puts_substep "Installed job runner software on runner."
    end

    def fetch_certificates
      run_command("mkdir -p /etc/chef/trusted_certs")
      puts_substep "Creating /etc/chef/trusted_certs on runner."
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
      cmd = "/opt/chefdk/bin/knife ssl fetch --server https://#{fqdn} --config-option trusted_certs_dir=/etc/chef/trusted_certs"
      run_command(cmd)
      puts_substep "Fetched Automate certificate from #{fqdn}."
    end

    def copy_custom_certificate_chain
      return if @config.fips_custom_cert_filename.nil?
      copy_file(@config.fips_custom_cert_filename,
                "#{remote_temp_path}/installer/custom_certificate_chain.crt")
      puts_substep "Copied custom certificate chain to runner."
    end

    def configure_job_runner
      runner_info_hash = @local_knife.gather_node_os_info_hash
      runner_info_hash['hostname'] = @config.fqdn
      pubkey = @client.create_runner(runner_info_hash)
      case connection.os_name
      when 'mac_os_x'
        # Darwin fails to find chef-apply when not given the full path
        run_command("/usr/local/bin/chef-apply #{remote_temp_path}/installer/install-job-runner.rb \'#{pubkey}\'")
      else
        run_command("#{sane_paths} chef-apply #{remote_temp_path}/installer/install-job-runner.rb \'#{pubkey}\'")
      end
      puts_substep "Configured runner as job runner on Automate server."
    end

    def restore_backup_key
      if File.exist?('/etc/chef/validation.pem.bak') && @key_backup
        File.rename('/etc/chef/validation.pem.bak', '/etc/chef/validation.pem')
        puts_substep "Restored existing key."
      end
    end

    def sane_paths
      "PATH=$PATH:#{FALLBACK_PATH}"
    end

    def cleanup
      run_command("rm -rf #{remote_temp_path}/")
      puts_substep "All temporary files removed."
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
          puts "Attempting to remove #{basename}"
          run_command("rm #{file}")
        rescue Runner::Exceptions::RemoteExecutionFailed,
               Runner::Exceptions::RemoteConnectionFailed
          remaining_files << file
        end
      end

      unless remaining_files.empty?
        subject, object = remaining_files.length == 1 ? ["file", "it"] : ["files", "them"]
        puts("I was unable to remove the following sensitive #{subject} from the runner:")
        puts("")
        remaining_files.each { |path| puts(" * #{path}") }
        puts("")
        puts("Please log into #{@config.fqdn} and remove #{object}.")
      end
    end

    def run_command(command)
      connection.run_command(command)
    end

    def copy_file(from, to)
      connection.copy_file(from, to)
      copied_files << to
    end

    def output_error(ex, show_usage=false)
      logger.fatal("Received error #{ex.class} with message #{ex.message}") if logger

      error_str = ex.message
      extra_info_str = nil
      if ex.remediation_steps
        extra_info_str = "#{ex.remediation_steps}"
      end

      puts_error(error_str, extra_info_str)

      if show_usage
        @config.display_usage if @config
      end
    end

    private

    def more_info
      puts ""
      puts "Please check the log files at #{log_path} for more information"
    end
  end
end
