require 'setup/exceptions'
require 'json'

module Setup
  class AttributesFile
    def initialize(base_path, config)
      @base_path = base_path
      @config = config
    end

    def verify!
      verify_file!(@config.license, "Delivery license file")
      verify_file!(@config.delivery_user_key, "Delivery user key")
      verify_chef_server!
    end

    def path
      "#{@base_path}/embedded/cookbooks/delivery/setup_params.json"
    end

    def write
      attributes = {
        run_list: [ "recipe[delivery::delivery_setup]" ],
        delivery_setup: {
          fqdn: @config.delivery_fqdn,
          chef_server: @config.chef_server,
          delivery_pem: @config.delivery_user_key,
          license_file: @config.license,
          supermarket_fqdn: @config.supermarket_fqdn
        }
      }

      File.open(path, 'w') { |file| file.write(attributes.to_json) }
    rescue Exception => e
      # File operations raise a variety of errors. Let's catch 'em all.
      raise Setup::Exceptions::FileIOError,
          "Failed to write attributes file '#{path}': #{e.message}"
    end

    def write_minimal
      attributes = {
        run_list: [ "recipe[delivery::minimal_delivery_setup]" ],
        delivery_setup: {
          fqdn: @config.delivery_fqdn,
          adoption: true
        }
      }

      File.open(path, 'w') { |file| file.write(attributes.to_json) }
    rescue Exception => e
      # File operations raise a variety of errors. Let's catch 'em all.
      raise Setup::Exceptions::FileIOError,
          "Failed to write attributes file '#{path}': #{e.message}"
    end

    private

    def verify_file!(path, file_description)
      raise Setup::Exceptions::BadArgumentError, "#{file_description}" \
          " '#{path}' does not exist. Please ensure the provided" \
          " #{file_description} exists before continuing." unless File.exist?(path)
      raise Setup::Exceptions::BadArgumentError, "#{file_description}" \
          " '#{path}' is not readable by current user. Please ensure the" \
          " provided #{file_description} is readable by current user" \
          " or try re-running with `sudo`." unless File.readable?(path)
    end

    def verify_chef_server!
      raise Setup::Exceptions::BadArgumentError, "Chef Server URL" \
          " '#{@config.chef_server}' does not match format:" \
          " SCHEME://CHEF_SERVER_FQDN/organizations/CHEF_DELIVERY_ORG" unless @config.chef_server =~ /organizations\/\S+/
    end
  end
end
