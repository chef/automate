require 'automate/backend/connector'
require 'automate/backend/automate_config'
require 'automate/backend/ctl/version'
require 'automate/backend/log'
require 'automate/backend/service_config'
require 'thor'

module Automate
  module Backend
    # Cli class
    class CLI < Thor
      AUTOMATE_CONF_OUT = '/tmp/automate_conf.toml'.freeze

      class_option :verbose, type: :boolean
      class_option :'chef-automate', type: :string, desc: 'path to chef-automate binary'

      option :toml, required: true, type: :string, desc: 'path to a connector.toml file'
      option :erb, required: true, type: :string, desc: 'path to a automate_conf.toml.erb template file'
      option :'conf-out', required: false, type: :string, desc: 'File path to output (if set, will not run a deploy)'
      desc 'connect', 'Connect up Backend HA Services'
      def connect
        connector = Automate::Backend::Connector.new(options[:toml]).content
        config = {}
        connector[:services].each do |name, svc|
          svc[:ips].each do |ip|
            config[name] = Automate::Backend::ServiceConfig.new(ip, svc[:sup_port], svc[:svc_name]).config
            break if config
          end
        end
        out_file = options[:'conf-out'] ? options[:'conf-out'] : AUTOMATE_CONF_OUT
        Automate::Backend::AutomateConfig.new(config, connector, options[:erb], out_file).save

        bin = ENV['BACKEND_CTL_AUTOMATE_BIN'] ? ENV['BACKEND_CTL_AUTOMATE_BIN'] : 'chef-automate'
        unless options[:'conf-out']
          cmd = "#{bin} deploy #{out_file} --accept-terms-and-mlsa"
          Log.info "Running #{cmd}"
          system("#{cmd}") unless options[:'conf-out']
        end
      end

      option :svc, required: true, type: :string, desc: 'service name ie. automate-ha-postgresql'
      desc 'show', 'Show running config for locally running service'
      def show
        puts Automate::Backend::ServiceConfig.new('localhost', '9631', options[:svc]).toml_config
      end

      option :svc, required: true, type: :string, desc: 'service name ie. automate-ha-postgresql'
      desc 'applied', 'Show gossip config that was hab config applied for locally running service'
      def applied
        puts Automate::Backend::ServiceConfig.new('localhost', '9631', options[:svc]).hab_config_applied
      end

      desc 'version', 'Version of automate-backend-ctl'
      def version
        puts "VERSION: #{AutomateBackend::Ctl::VERSION}"
      end
    end
  end
end
