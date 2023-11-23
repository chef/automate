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
      logger.level = Logger::WARN
      logger.warn 'AutomateCluster::ElasticSidecar initialized!'
    end

    def logger
      @logger ||= Logger.new(STDERR)
    end
    
    def config
      @config ||= load_config
    end
#
    def valid_json?(json)
      JSON.parse(json)
      true
    rescue JSON::ParserError
      false
    end
#
    def test_authentication(user, pass)
      http = HTTP.accept(:json)
                 .basic_auth(user: user, pass: pass)
      http.get('https://localhost:9200', ssl_context: config['ctx_basic'])
    rescue HTTP::Error => e
      logger.error e.message
      'error'
    end
#
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
#
    def load_config
      config_file = '/hab/svc/automate-ha-elasticsidecar/config/elastic_sidecar.toml'
      logger.warn "Attempting to load config from #{config_file}"
      conf = TomlRB.load_file(config_file)
      conf['ssl_mode'] = if conf['ssl_verify'] == false
                           0
                         else
                           1
                         end
#
      conf['ctx_basic'] = OpenSSL::SSL::SSLContext.new
      conf['ctx_basic'].set_params(
        verify_mode: conf['ssl_mode'],
        verify_hostname: conf['ssl_mode'].zero? ? false : true
      )
      conf
    end
#
    def hash_password(password)
      password_hash = BCrypt::Password.create(password)
      password_hash.to_s
    end
#
    def write_internal_users
      puts config.inspect
      internal_users = {
        "_meta" => {
          "type" => "internalusers",
          "config_version" => "2",
        },
        config['admin_username'] => {
          'hash' => hash_password(config['admin_password']),
          'reserved' => true,
          'backend_roles' => [config['admin_username']],
        }
      }
      f = File.new("#{config['securityconfig_path']}/internal_users.yml", 'w+')
      f.write(internal_users.to_yaml)
      f.close
    end
#
    def write_roles_mapping
      roles_mapping = {
        "_meta" => {
          "type" => "rolesmapping",
          "config_version" => "2",
        },
        'all_access' => {
          'reserved' => 'false',
          'backend_roles' => [config['admin_username']],
        },
        'own_index' => {
          'reserved' => 'false',
          'backend_roles' => ['*'],
        }
      }
      f = File.new("#{config['securityconfig_path']}/roles_mapping.yml", 'w+')
      f.write(roles_mapping.to_yaml)
      f.close
    end
#
    def insert_credentials
      insert_command = "#{config['tool_path']}/securityadmin.sh -h #{config['opensearch_ip']} \
        -p #{config['opensearch_port']} -cacert #{config['opensearch_ca']} \
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
#
    def rotate_credentials
      logger.warn 'Preparing to rotate credentials'
      setup_templates
      write_internal_users
      write_roles_mapping
      insert_credentials
    end
#
    def setup_templates
      unless Dir.exist?(config['securityconfig_path'])
        FileUtils.mkdir_p(config['securityconfig_path'])
      end
      FileUtils.cp %w[
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/action_groups.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/config.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/roles.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/tenants.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/nodes_dn.yml
        /hab/svc/automate-ha-elasticsidecar/config/securityconfig/whitelist.yml
      ], config['securityconfig_path']
    end
#
    def wait
      logger.debug "Sleeping 60s"
      sleep 60
    end
#
    def run
      loop do
        response = test_authentication(config['admin_username'], config['admin_password'])
        if response.eql? 'error'
          wait
          next
        elsif response.code == 200
          logger.debug "auth successful for #{config['admin_username']}"
        end
#
        case response.code
        when 200
          logger.debug 'Authentication successful, doing nothing'
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
