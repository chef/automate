#!/usr/bin/env ruby

require 'bcrypt'
require 'fileutils'
require 'http'
require 'json'
require 'logger'
require 'toml-rb'
require 'mixlib/shellout'
require 'chefstyle'

module AutomateCluster
  class ElasticSidecar
    def initialize
      logger.level = config['log_debug'] == true ? Logger::DEBUG : Logger::WARN
      logger.warn 'AutomateCluster::ElasticSidecar initialized!'
    end

    def logger
      @logger ||= Logger.new(STDERR)
    end

    def config
      @config ||= load_config
    end

    def valid_json?(json)
      JSON.parse(json)
      true
    rescue JSON::ParserError
      false
    end

    def test_authentication(user, pass)
      http = HTTP.accept(:json)
                 .basic_auth(user: user, pass: pass)
      http.get('https://localhost:9200', ssl_context: config['ctx_basic'])
    rescue HTTP::Error => e
      logger.error e.message
      'error'
    end

    def run_command(cmd)
      environment = { HAB_LICENSE: 'accept-no-persist',
                      JAVA_HOME: config['java_home'] }
      so = Mixlib::ShellOut.new(cmd, environment: environment)
      logger.warn "Running CMD: #{cmd}"
      so.run_command
      so.error!
      so
    rescue Mixlib::ShellOut::ShellCommandFailed => e
      logger.error "encountered exception #{e}!"
      if so.stderr
        message = "\n***********\n"
        message += so.stderr
        message += "***********\n"
      end
      raise message
    end

    def load_config
      config_file = '/hab/svc/automate-ha-elasticsidecar/config/elastic_sidecar.toml'
      logger.warn "Attempting to load config from #{config_file}"
      conf = TomlRB.load_file(config_file)
      conf['ssl_mode'] = if conf['ssl_verify'] == false
                           0
                         else
                           1
                         end

      conf['ctx_basic'] = OpenSSL::SSL::SSLContext.new
      conf['ctx_basic'].set_params(
        verify_mode: conf['ssl_mode'],
        verify_hostname: conf['ssl_mode'].zero? ? false : true
      )
      conf
    end

    def hash_password(password)
      password_hash = BCrypt::Password.create(password)
      password_hash.to_s
    end

    def write_internal_users
      internal_users = {
        config['admin_username'] => {
          'hash' => hash_password(config['admin_password']),
          'roles' => [config['admin_username']]
        },
        config['dashboard_username'] => {
          'hash' => hash_password(config['dashboard_password']),
          'roles' => [config['dashboard_username']]
        }
      }
      f = File.new("#{config['securityconfig_path']}/internal_users.yml", 'w+')
      f.write(internal_users.to_yaml)
      f.close
    end

    def write_roles_mapping
      roles_mapping = {
        'all_access' => {
          'backendroles' => [config['admin_username']]
        },
        'dashboard_user' => {
          'backendroles' => [config['dashboard_username']]
        }
      }
      f = File.new("#{config['securityconfig_path']}/roles_mapping.yml", 'w+')
      f.write(roles_mapping.to_yaml)
      f.close
    end

    def insert_credentials
      insert_command = "#{config['tool_path']}/securityadmin.sh -h #{config['elasticsearch_ip']} \
        -p #{config['elasticsearch_port']} -cacert #{config['elasticsearch_ca']} \
        -cert #{config['admin_cert']} -key #{config['admin_key']} -nhnv -icl \
        -cd #{config['securityconfig_path']}"
      result = run_command(insert_command)
      if result.exitstatus.zero? && result.stdout !~ /ERR: /
        logger.warn 'Credentials successfully rotated'
        logger.warn result
      else
        logger.error "Credentials failed to rotate with error code #{result.exitstatus}"
        logger.error result
      end
    end

    def rotate_credentials
      logger.warn 'Preparing to rotate credentials'
      setup_templates
      write_internal_users
      write_roles_mapping
      insert_credentials
    end

    def setup_templates
      unless Dir.exist?(config['securityconfig_path'])
        FileUtils.mkdir_p(config['securityconfig_path'])
      end
      FileUtils.cp %w[
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/action_groups.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/config.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/roles.yml
      ], config['securityconfig_path']
    end

    def wait
      logger.debug "Sleeping #{config['wait_period']}s"
      sleep config['wait_period']
    end

    def insert_dashboards
      if File.directory?(config['dashboard_directory'])
        Dir.glob("#{config['dashboard_directory']}*.json") do |dashboard|
          dashboard_uuid = File.basename(dashboard, ".json")
          http = HTTP.accept(:json)
                     .basic_auth(user: config['admin_username'], pass: config['admin_password'])
          response = http.get("https://localhost:9200/.kibana/doc/dashboard:#{dashboard_uuid}", ssl_context: config['ctx_basic'])
          unless JSON.parse(response.body)['found']
            logger.warn "inserting dashboard: #{dashboard_uuid}"
            http = HTTP.accept(:json)
                       .headers("kbn-xsrf" => "true")
                       .basic_auth(user: config['admin_username'], pass: config['admin_password'])
            response = http.post("https://localhost:5601/api/kibana/dashboards/import", ssl_context: config['ctx_basic'], json: (File.read(dashboard)))
          end
          # Set a default index pattern, so the users don't need to dig around in settings to get a working dashboard
          http = HTTP.accept(:json)
                     .headers("kbn-xsrf" => "true")
                     .basic_auth(user: config['admin_username'], pass: config['admin_password'])
          kibana_config = http.get("https://localhost:5601/api/saved_objects/config/6.5.4", ssl_context: config['ctx_basic'])
          # If we get a 404 then the fields we need to check later won't exist
          if kibana_config.code == 404
            override_default = true
          end
          if kibana_config.code == 200
            if JSON.parse(kibana_config.body)['attributes']['defaultIndex'].nil?
              override_default = true
            end
          end
          if  override_default == true
            logger.warn "Default index pattern is not configured, setting to metricbeat"
            response = http.post("https://localhost:5601/api/saved_objects/config/6.5.4?overwrite=true", ssl_context: config['ctx_basic'], json: '{"attributes":{"defaultIndex":"b40cfb40-db5a-11e9-9a8d-7f87f55fd222"}}')
          end
        end
      end
    rescue HTTP::Error => e
      logger.error e.message
      'error'
    end

    def run
      loop do
        response = test_authentication(config['admin_username'], config['admin_password'])
        if response.eql? 'error'
          wait
          next
        elsif response.code == 200
          logger.debug "auth successful for #{config['admin_username']} now testing #{config['dashboard_username']}"
          response = test_authentication(config['dashboard_username'], config['dashboard_password'])
        end

        case response.code
        when 200
          logger.debug 'Authentication successful, doing nothing'
          insert_dashboards
        when 401, 403
          logger.warn 'Authentication failed, inserting credentials'
          rotate_credentials
        when 503
          if response.body.to_s.eql? 'Open Distro not initialized'
            logger.warn 'Open Distro appears to not be setup, inserting credentials'
            rotate_credentials
          elsif response.body.to_s.valid_json?
            logger.warn 'Auth successful, but elasticsearch appears to be broken, doing nothing.'
          else
            logger.warn 'Elasticsearch returned 503 error with unexpected message'
            logger.warn response.body
          end
        else
          logger.error "Elasticsearch returned #{response.code} code with unexpected message"
          logger.error response.body
        end
        wait
      end
    end
  end
end

svc = AutomateCluster::ElasticSidecar.new
svc.run
