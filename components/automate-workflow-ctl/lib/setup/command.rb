module Setup
  class Command
    def initialize(ctl, args)
      require 'setup/enterprise_ctl_helper'
      require 'setup/attributes_file'
      require 'setup/config'
      require 'setup/exceptions'
      require 'setup/log'
      @ctl = ctl
      @config = Setup::Config.new(args)
      @eh = Setup::EnterpriseCtlHelper.new
    end

    def run
      @config.prompt_for_missing
      Setup::Log.info "Setting up Chef Automate Server. This will take a moment."
      run_chef_client
    end

    def run_minimal_setup
      Setup::Log.info "Setting up Chef Automate Server with default settings. This will take a moment."
      attributes_file = Setup::AttributesFile.new(@ctl.base_path, @config)
      attributes_file.write_minimal
      log_params = "-l info -L #{Setup::Log::LOG_LOCATION} > #{Setup::Log::FULL_LOG_LOCATION} 2>&1"
      chef_run = @ctl.run_chef(attributes_file.path, log_params)

      unless chef_run.success?
        raise Setup::Exceptions::ChefClientRunFailed, chef_run.exitstatus
      end
    end

    def minimal_setup?
      @config.minimal
    end

    def fqdn
      @config.delivery_fqdn
    end

    def install_build_node?
      return @install_build_node unless @install_build_node.nil?
      @install_build_node = if @config.install_build_node.nil?
                              prompt_build_node
                            else
                              @config.install_build_node
                            end
    end

    def install_build_node
      require 'setup/install_runner_helper'
      require 'runner/install/command'
      options = Setup::InstallRunnerHelper.new.gather_options
      install_command = Runner::Install::Command.new(options)
      install_command.run
    rescue CtlHelpers::Exceptions::ActionCanceled
      Setup::Log.info
      Setup::Log.info "Addition of runner has been canceled."
      Setup::Log.info "Hit Ctrl+C again to cancel Chef Automate setup."
      Setup::Log.info
    rescue Exception => e
      Setup::Log.error
      Setup::Log.error e.message
      Setup::Log.error "The runner setup did not complete."
      Setup::Log.error "Please run `automate-ctl install-runner` to try again"
      Setup::Log.error
    end

    def create_enterprise?
      !@eh.any_enterprise_exists?
    end

    def create_enterprise
      Setup::Log.info "Creating Chef Automate enterprise #{@config.delivery_enterprise}."
      @eh.create_enterprise(@config.delivery_enterprise)
    end

    def reconfigure?
      return @reconfigure unless @reconfigure.nil?
      @reconfigure = if @config.reconfigure.nil?
                       prompt_reconfigure
                     else
                       @config.reconfigure
                     end
    end

    def reconfigure
      Setup::Log.info "Configuring and bringing the Chef Automate Server online, this can take up to 10 minutes."
      progress_pid = print_spinner

      begin
        result = @ctl.run_command("automate-ctl reconfigure >> #{Setup::Log::FULL_LOG_LOCATION} 2>&1")

        raise Setup::Exceptions::ReconfigureFailed, result.exitstatus if result.exitstatus != 0
      ensure
        Process.kill 9, progress_pid
        Process.wait progress_pid
        print "\n" #print newline after spinner stops for formatting
      end
    end

    private

    def print_spinner
      fork do
        while true
          ('/-\\|' * 10).each_char { |c| print c; sleep(0.1); print "\b" }
        end
      end
    end

    def run_chef_client
      attributes_file = create_attributes_file
      # Set the log level explicitly to :info to ensure readable logs are written
      # to the log file.
      #
      # Chef Zero is very noisy when the log level is set to :info, and we don't
      # want to display that STDOUT. This should be resolved when Chef Client picks
      # up Chef Zero 4.6.2+. (See https://github.com/chef/chef-zero/pull/216)
      #
      # Forward the full STDOUT and STDERR to log file at FULL_LOG_LOCATION for easier
      # debugging. Note that this hides all Chef Client run output. We log a brief
      # message to STDOUT first to let folks know what is happening.
      log_params = "-l info -L #{Setup::Log::LOG_LOCATION} > #{Setup::Log::FULL_LOG_LOCATION} 2>&1"
      chef_run = @ctl.run_chef(attributes_file.path, log_params)

      unless chef_run.success?
        raise Setup::Exceptions::ChefClientRunFailed, chef_run.exitstatus
      end
    end

    def create_attributes_file
      attributes_file = Setup::AttributesFile.new(@ctl.base_path, @config)
      attributes_file.verify!
      attributes_file.write
      attributes_file
    end

    def prompt_build_node
      CtlHelpers::Prompt.yes_no_prompt "Would you like to add a runner now? (yes/no)"
    end

    def prompt_reconfigure
      CtlHelpers::Prompt.yes_no_prompt "Would you like to apply Chef Automate Server configuration now? (yes/no)"
    end
  end
end
