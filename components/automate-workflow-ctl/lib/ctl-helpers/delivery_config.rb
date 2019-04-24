require 'ctl-helpers/exceptions'

# Supports reading of delivery.rb configuration file and its supplied options.

module CtlHelpers

  # This class is compatible with  MixlibConfig but will auto-vivify
  # missing top-level and nested entries found in configuration files.
  # It supports both method-based access to config entries as well
  # as hash key.
  # To determine if a nested value has been auto-vivified
  # use `DeliveryConfig.value_defaulted?`, eg DeliveryConfig.value_defaulted?(DeliveryConfig['something']['here'])
  class DeliveryConfig
    def self.config
      @config ||= NestedHash.new
    end

    def self.reset
      @config = NestedHash.new
    end

    def self.from_file(filename)
      self.instance_eval(IO.read(filename), filename, 1)
    end

    def self.[](key)
      config[key]
    end

    def self.[]=(key, value)
      config[key] = value
    end

    def self.method_missing(method_symbol, *args)
      method = method_symbol.to_s
      method = $1 if method =~ /(.+)=$/
      if args.length > 0
        config[method] = args[0]
      else
        self.config[method]
      end
    end

    def self.value_defaulted?(value)
      value.respond_to?(:defaulted?) ? value.defaulted? : false
    end

    def self.delivery_config_path
      ENV['DELIVERY_RB'] || '/etc/delivery/delivery.rb'
    end

    # Leads configuration from delivery_config_path then validates it
    # for fields required for build node configuration.
    # Returns true on success.
    # Raises CtlHelpers::ExceptionsConfigurationError when:
    #   - the configuration file does not exist
    #   - the configuration file cannot be read
    #   - any required configuration option is missing
    def self.validate!
      from_file(delivery_config_path)
      required_options.each do |option|
        required!(*option)
      end
      true
    rescue Errno::ENOENT => e
      raise CtlHelpers::Exceptions::ConfigurationError.new(
        "Missing required configuration file '#{delivery_config_path}'")
    rescue IOError => e
      raise CtlHelpers::Exceptions::ConfigurationError, e.message
    end

    # Returns a string representation of the provided option + suboptions.
    # Examples:
    #   stringify(:foo) => "foo"
    #   stringify(:foo, :bar, :baz) => "foo['bar']['baz']"
    def self.stringify(option, *suboptions)
      return option.to_s if suboptions.nil? || suboptions.empty?

      suboptions_str = "['" + suboptions.join("']['") + "']"
      option.to_s + suboptions_str
    end

    private

    # Raises a CtlHelpers::Exceptions::ConfigurationError if configuration
    # option is missing from the loaded delivery.rb file.
    def self.required!(*option)
      setting = config
      option.each do |opt|
        setting = setting[opt]
      end
      setting = nil if value_defaulted?(setting)

      raise CtlHelpers::Exceptions::ConfigurationError,
        "Missing configuration option '#{stringify(*option)}'." \
        " Please ensure the configuration file at" \
        " #{delivery_config_path} contains the following settings:" \
        " #{required_options.map{ |opt| stringify(*opt) }.join(", ")}." if setting.nil?

      setting
    end

    # Returns an Array of required configuration options, represented as Arrays.
    # For example, [:delivery, :chef_username] represents configuration option
    # delivery['chef_username'].
    def self.required_options
      [
        :delivery_fqdn,
        [:delivery, :chef_username],
        [:delivery, :chef_private_key],
        [:delivery, :chef_server]
      ]
    end

    class NestedHash < Hash
      def initialize
        @defaulted = true
        super {|h,k| h[k] = NestedHash.new(&h.default_proc)}
      end

      def [](k)
        super(k.to_s)
      end

      def []=(k,v)
        @defaulted = false
        super(k.to_s, v)
      end

      def defaulted?
        @defaulted
      end

      # This allows us to access nested members
      # via dot-notation, consistent with Mixlib::Config's behavior
      def method_missing(method_symbol, *args)
        method = method_symbol.to_s
        method = $1 if method =~ /(.+)=$/
        if args.length > 0
          self[method] = args[0]
        else
          self[method]
        end
      end
    end
  end
end
