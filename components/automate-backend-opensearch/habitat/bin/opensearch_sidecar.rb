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
  class OpensearchSidecar
    def initialize
      logger.level = config['log_debug'] == true ? Logger::DEBUG : Logger::WARN
      logger.warn 'AutomateCluster::OpensearchSidecar initialized!'
    end

    def logger
      @logger ||= Logger.new(STDERR)
    end

    def config
      @config ||= load_config
    end

    def load_config
      config_file = '/hab/svc/automate-ha-opensearch/config/opensearch_sidecar.toml'
      logger.warn "Attempting to load config from #{config_file}"
      conf = TomlRB.load_file(config_file)
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
        }
      }
      f = File.new("#{config['securityconfig_path']}/roles_mapping.yml", 'w+')
      f.write(roles_mapping.to_yaml)
      f.close
    end

    def rotate_credentials
      logger.warn 'Preparing to put internal user and roles mapping'
      write_internal_users
      write_roles_mapping
    end

    def run
      rotate_credentials
    end
  end
end

svc = AutomateCluster::OpensearchSidecar.new
svc.run
