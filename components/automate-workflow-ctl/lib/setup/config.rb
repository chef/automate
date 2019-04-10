module Setup
  class Config
    def initialize(args)
      require 'setup/exceptions'
      require 'ctl-helpers/ohai'
      require 'ctl-helpers/prompt'
      require 'ctl-helpers/delivery_config'

      @options = {}
      # Load any existing config first so that parse_args
      # has that info available to disable associated command line arguments
      begin
        CtlHelpers::DeliveryConfig.from_file(CtlHelpers::DeliveryConfig.delivery_config_path)
      rescue Errno::ENOENT, IOError
        # this can be received if there is no delivery.rb file - this is not an error
      end
      parse_args(args)
      hydrate_config_values
    end

    def prompt_for_missing
      set_from_prompt_if_missing(@options, :license, "Chef Automate license path")
      fqdn = CtlHelpers::Ohai.attribute(:fqdn)
      set_from_prompt_if_missing(@options, :delivery_fqdn, "Fully qualified domain name of the current host", fqdn)
      set_from_prompt_if_missing(@options, :delivery_user_key, "Chef Automate user key path")
      set_from_prompt_if_missing(@options, :chef_server, "Chef Server URL")
      set_from_prompt_if_missing(@options, :delivery_enterprise, "Name of your Enterprise")
    end

    def set_from_prompt_if_missing(options, key, prompt_text, default = nil)
      require 'ctl-helpers/prompt'
      options[key] ||= CtlHelpers::Prompt.prompt_user(prompt_text, true, default)
      options
    end

    def minimal
      @options[:minimal]
    end

    def license
      @options[:license]
    end

    def delivery_enterprise
      @options[:delivery_enterprise]
    end

    def delivery_user_key
      @options[:delivery_user_key]
    end

    def delivery_fqdn
      @options[:delivery_fqdn]
    end

    def chef_server
      @options[:chef_server]
    end

    def supermarket_fqdn
      @options[:supermarket_fqdn]
    end

    def install_build_node
      @options[:install_build_node]
    end

    def reconfigure
      @options[:reconfigure]
    end

    private

    def hydrate_config_values
      value = CtlHelpers::DeliveryConfig['delivery']['chef_private_key']
      unless CtlHelpers::DeliveryConfig.value_defaulted?(value)
        @options[:delivery_user_key] ||= value
      end

      value = CtlHelpers::DeliveryConfig['delivery']['chef_server']
      unless CtlHelpers::DeliveryConfig.value_defaulted?(value)
        @options[:chef_server] ||=  value
      end

      value = CtlHelpers::DeliveryConfig['delivery']['supermarket_fqdn']
      unless CtlHelpers::DeliveryConfig.value_defaulted?(value)
        @options[:supermarket_fqdn] ||= value
      end

      value = CtlHelpers::DeliveryConfig['delivery_fqdn']
      unless CtlHelpers::DeliveryConfig.value_defaulted?(value)
        @options[:delivery_fqdn] ||= value
      end
    end

    def parse_args(args)
      require 'optparse'

      OptionParser.new do |opts|
        opts.banner = "Command: automate-ctl setup [...options...] (requires root)"

        opts.on("-h", "--help", "Prints this help") do
          $stdout.puts opts.to_s
          exit 0
        end

        opts.on("--minimal", "[Pre-Release] Set up Chef Automate with a minimal default configuration.") do
          @options[:minimal] = true

          # Run reconfigure by default
          @options[:reconfigure] = true

          # Use the Ohai-supplied IP address as the default FQDN
          @options[:delivery_fqdn] = CtlHelpers::Ohai.attribute(:ipaddress)
        end

        opts.on("-l", "--license LICENSE", "Location of Chef Automate license file.") do |license|
          @options[:license] = license
        end

        add_config_opt(opts, :delivery_fqdn, ["-f", "--fqdn FQDN"], "The external fully qualified domain name of this node", ['delivery_fqdn'])
        add_config_opt(opts, :delivery_user_key, ["-k", "--key CHEF_AUTOMATE_USER_KEY"], "Location of Chef Automate user key", ['delivery', 'chef_private_key'])
        add_config_opt(opts, :chef_server, ["--server-url CHEF_SERVER_URL"], "Chef Server URL", ['delivery', 'chef_server'])
        add_config_opt(opts, :supermarket_fqdn, ["--supermarket-fqdn SUPERMARKET_FQDN"], "Internal Supermarket FQDN", ['delivery', 'supermarket_fqdn'])

        opts.on("-e", "--enterprise CHEF_AUTOMATE_ENTERPRISE_NAME", "Name of the Chef Automate Enterprise to create.") do |enterprise|
          @options[:delivery_enterprise] = enterprise
        end
        opts.on("--[no-]build-node", "Install a build node after Chef Automate Server setup completes.") do |build_node|
          @options[:install_build_node] = build_node
        end

        opts.on("--[no-]configure", "Apply configuration changes automatically after Chef Automate Server setup completes.") do |reconfigure|
          @options[:reconfigure] = reconfigure
        end
      end.parse!(args)
    rescue OptionParser::MissingArgument,
           OptionParser::InvalidArgument,
           OptionParser::InvalidOption,
           OptionParser::MissingArgument => e
      raise Setup::Exceptions::BadArgumentError.new e.message
    end

    def add_config_opt(opts, option_key, option_flags, option_description, key_names)
      initial_description = option_description.dup
      # Create any parent keys with default values - lookup of the deepest node
      # will still return a defaulted value even if we created the parent keys by
      # doing this.
      config_value = CtlHelpers::DeliveryConfig
      key_names.each {|v| config_value = config_value[v] }
      set_in_config = !CtlHelpers::DeliveryConfig.value_defaulted?(config_value)

      option_description << " (Already set in delivery.rb.  Do not set via flag.)" if set_in_config
      option_flags << option_description

      opts.on(*option_flags) do |value|
        if set_in_config
          raise Exceptions::ArgumentExistsAsConfigValueError.new(initial_description,
                                                                 CtlHelpers::DeliveryConfig.delivery_config_path,
                                                                 CtlHelpers::DeliveryConfig.stringify(key_names.shift, *key_names),
                                                                 config_value)
        end
        @options[option_key] = value
      end
    end

  end
end
