require 'io/console'
require 'ctl-helpers/prompt'

module BuildNode
  class Config
    attr_reader :installer, :fqdn, :username, :password, :port, :ssh_identity_file,
      :job_dispatch_version, :admin_user, :admin_token, :enterprise, :client_config

    def initialize(options)
      @options = options
    end

    def validate_and_prompt!
      set_installer(@options[:installer])
      set_fqdn(@options[:fqdn])
      set_username(@options[:username])
      set_ssh_identity_file(@options[:ssh_identity_file],
                            @options[:ssh_identity_file_flag_present?])
      set_password(@options[:password])
      set_port(@options[:port])
      set_job_dispatch_version(@options[:job_dispatch_version])
      set_admin_user(@options[:admin_user])
      set_admin_token(@options[:admin_token])
      set_enterprise(@options[:enterprise])
      set_client_config
    end

    # Note: This function will prompt the user when called if answer is unknown
    def overwrite_registration?
      return @options[:overwrite_registration] unless @options[:overwrite_registration].nil?
      prompt = "This node(#{fqdn}) has already been registered with Chef Server.\nWould you like to overwrite it? (y/n)"
      @options[:overwrite_registration] = CtlHelpers::Prompt.yes_no_prompt(prompt)
    end

    def set_installer(installer)
      installer ||= prompt_user("ChefDK package path")
      @installer = verify_file!(installer)
    end

    def set_fqdn(fqdn)
      @fqdn = fqdn || prompt_user("Build node FQDN")
    end

    def set_username(username)
      @username = username || prompt_user("Username for #{fqdn}")
    end

    def set_password(password)
      show_typing = false
      if password.nil? && @ssh_identity_file.nil?
        @password = prompt_user(
          "Password for #{username} on #{fqdn}",
          show_typing,
        )
      elsif password.nil? && !@ssh_identity_file.nil?
        @password = prompt_user(
          "Password for #{username} on #{fqdn} (press enter for 'none' if passwordless sudo is enabled)",
          show_typing,
          :none,
        )
        # If none was selected, set the password to nil
        @password = nil if @password == :none
      else
        @password = password
      end
    end

    def set_port(port)
      @port = validate_port!(port || 22)
    end

    def set_ssh_identity_file(ssh_identity_file, is_ssh_identity_file_flag_present)
      if is_ssh_identity_file_flag_present && ssh_identity_file.nil?
        filename_from_prompt = prompt_user("SSH identity file path (press enter for none)", true, :none)
      end
      if ssh_identity_file
        filename = ssh_identity_file
      elsif filename_from_prompt && filename_from_prompt != :none
        filename = filename_from_prompt
      else
        filename = nil
      end
      @ssh_identity_file = verify_file!(filename) unless filename.nil?
    end

    def set_job_dispatch_version(version)
      @job_dispatch_version = validate_job_dispatch_version!(version || "v1")
    end

    def set_admin_user(username)
      @admin_user = username
      if job_dispatch_version == "v2"
        @admin_user ||= prompt_user("Admin user on local Workflow instance to use for registering runner", true, 'admin')
      end
    end

    def set_admin_token(token)
      @admin_token = token
      if job_dispatch_version == "v2"
        @admin_token ||= prompt_user("Token for admin user in Workflow", false)
      end
    end

    def set_enterprise(ent)
      @enterprise = ent
      if job_dispatch_version == "v2"
        @enterprise ||= prompt_user("Enterprise of admin user in Workflow", false)
      end
    end

    def set_client_config
      @client_config = if @options[:full_ohai]
        nil
      else
        "minimal_ohai true\n"
      end
    end

    def validate_port!(port)
      min_port = 1
      max_port = 65535
      port_int = port.to_i
      if port_int < min_port || port_int > max_port
        raise BuildNode::Exceptions::BadArgumentError.new "Error, port '#{port}' is invalid. Expected value between #{min_port} and #{max_port}."
      end
      port_int
    end

    def verify_file!(filename)
      raise BuildNode::Exceptions::BadArgumentError, "Error, file '#{filename}' does not exist. Please verify the path provided." unless File.exist?(filename)
      raise BuildNode::Exceptions::BadArgumentError, "Error, file '#{filename}' is not readable by the current user. Please check permissions on the file, or retry with 'sudo'." unless File.readable?(filename)
      filename
    end

    def validate_job_dispatch_version!(version)
      unless version == "v1" || version == "v2"
        raise BuildNode::Exceptions::BadArgumentError.new "Error, job dispatch version '#{version}' is invalid. Expected values are 'v1' or 'v2'."
      end
      version
    end

    def prompt_user(prompt, echo = true, default = nil)
      CtlHelpers::Prompt.prompt_user(prompt, echo, default)
    end

    def self.parse_args!(args, stdout)
      options = {}
      OptionParser.new do |opts|
        opts.banner = "Command: workflow-ctl install-build-node [...options...]"
        opts.on("-h", "--help", "Prints this help") do
          stdout.call opts
          exit
        end
        opts.on("-I", "--installer PATH_TO_INSTALLER",
                "The location of the ChefDK package for the build node (Required)") do |installer|
          options[:installer] = installer
        end
        opts.on("-f", "--fqdn FQDN", "FQDN of the remote host that will be configured into a build node") do |fqdn|
          options[:fqdn] = fqdn
        end
        opts.on("-u", "--username USERNAME", "Username to use for authentication to the remote host") do |username|
          options[:username] = username
        end
        opts.on("-P", "--password PASSWORD", "Password to use for authentication to the remote host") do |password|
          options[:password] = password
        end
        opts.on("-p", "--port PORT", "Port to connect to on the remote host") do |port|
          options[:port] = port
        end
        opts.on("-i", "--ssh-identity-file [IDENTITY_FILE]",
                "The SSH identity file used for authentication -",
                "will prompt if flag is specified but no filename is given") do |identity|
          options[:ssh_identity_file_flag_present?] = true
          options[:ssh_identity_file] = identity
        end
        opts.on("-o", "--[no-]overwrite-registration", "overwrite this node's entry in chef server if it's already registered") do |overwrite|
          options[:overwrite_registration] = overwrite
        end
        opts.on("-V", "--job-dispatch-version VERSION", "Job dispatch version to use (v1 [default] or v2)") do |job_dispatch_version|
          options[:job_dispatch_version] = job_dispatch_version
        end
        opts.on("-a", "--admin-user NAME", "Admin user name (necessary for job dispatch version or v2)") do |admin_user|
          options[:admin_user] = admin_user
        end
        opts.on("-t", "--admin-token TOKEN", "Admin token (necessary for job dispatch version or v2)") do |admin_token|
          options[:admin_token] = admin_token
        end
        opts.on("-e", "--enterprise ENTERPRISE", "Enterprise to use (necessary for job dispatch version or v2)") do |enterprise|
          options[:enterprise] = enterprise
        end
        opts.on("--full-ohai", "If `--full-ohai` flag set, Chef will run with full Ohai plugins.") do |full_ohai|
          options[:full_ohai] = full_ohai
        end
      end.parse!(args)
      options
    rescue OptionParser::MissingArgument,
           OptionParser::InvalidArgument,
           OptionParser::InvalidOption,
           OptionParser::MissingArgument => e
      raise BuildNode::Exceptions::BadArgumentError.new e.message
    end
  end
end
