#
# Author:: Faizan Fulara (<ffulara@progress.com>)
# Copyright:: Copyright (c) Chef Software Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
require_relative 'helpers'
require 'fileutils'

module AutomateCluster
  class Workspace
    include AutomateCluster::Helpers
    include AutomateCluster::Terraform::Helpers

    attr_reader :workspace_path, :source

    def initialize(working_path)
      @workspace_path = working_path

      # We want to cache this state when initialized.
      # If we just use File.exist?(path) it give us the wrong correct info
      # once the workspace location has been created
      check_for_new_workspace(working_path)
    end

    def check_for_new_workspace(working_path)
      if File.realpath(working_path) == Dir.pwd
        @new_workspace = !a2ha_config_exist?
      else
        @new_workspace = !File.exist?(working_path) && !a2ha_config_exist?
      end
    end

    def logger
      AutomateCluster.logger
    end

    def new_workspace?
      # Use the cached value that was setup in the initialize
      @new_workspace
    end

    def path(file = '.')
      File.expand_path file.dup, workspace_path
    end

    def version
      @version ||= read_version
    end

    def read_version
      File.read(path('VERSION')).chomp
    end

    def set_source(from)
      @source = Workspace.new(from)
    end

    def clone(opts = {})
      options = {
        copy_configs: false,
        copy_aibs: false,
        copy_terraform_files: true,
        copy_tools: true
      }.merge(opts)

      if source.path == path
        logger.info "Nothing to do"
        return true
      end

      FileUtils.mkdir_p(workspace_path)
      # Make sure this directory exists
      FileUtils.mkdir_p path('terraform/transfer_files')

      sync_aibs if options[:copy_aibs]
      sync_terraform_plans if options[:copy_terraform_files]
      sync_tools if options[:copy_tools]
      sync_terraform_plugins

      update_version(ENV['_CLUSTER_CTL_VERSION'])
    end

    def update_version(ver)
      File.open(path('VERSION'), 'w') { |fp| fp << ver }
    end

    def set_owner(owner)
      return unless owner
      shellout!("chown -R #{owner} #{path}")
    end

    def sync_tools
      tool_files = {
        'scripts/*' => 'scripts',
        'test/*' => 'test',
        'test/.bundle' => 'test',
        'components/*' => 'components',
        'Makefile' => '.',
        'certs/*' => 'certs'
      }

      tool_files.each do |file,dest|
        source_path = source.path(file)
        logger.debug "Syncing #{source_path} to #{path(dest)}"
        sync_files(source_path, path(dest)) if Dir.glob(source_path).length > 0
      end
    end

    def sync_terraform_plans
      terraform_files = {
        'terraform/reference_architectures/*' => 'terraform/reference_architectures',
        'terraform/modules/*' => 'terraform/modules',
        'terraform/*.tf' => 'terraform/'
      }

      terraform_files.each do |file,dest|
        source_path = source.path(file)
        logger.debug "Syncing #{source_path} to #{path(dest)}"
        sync_files(source_path, path(dest)) if Dir.glob(source_path).length > 0
      end
    end

    def sync_terraform_plugins
      plugins = { 'terraform/.terraform/plugins/*' => 'terraform/.terraform/plugins/'}

      plugins.each do |file,dest|
        source_path = source.path(file)
        logger.debug "Syncing #{source_path} to #{path(dest)}"
        FileUtils.mkdir_p(path(dest))
        sync_files(source_path, path(dest)) if Dir.glob(source_path).length > 0
      end
    end

    def aib_tfvar_files
      aibs = {}

      logger.debug path('terraform/a2ha_aib_be.auto.tfvars')
      be = load_aib_tfvars(path('terraform/a2ha_aib_be.auto.tfvars'))
      file = be.fetch(:backend_aib_local_file, false)

      if file
        aib = File.join('terraform/transfer_files', file)
        aibs[aib] = aib
        aibs["#{aib}.md5"] = "#{aib}.md5"
      else
        logger.debug "Couldn't find backend aib file to sync"
      end

      fe = load_aib_tfvars(path('terraform/a2ha_aib_fe.auto.tfvars'))
      file = fe.fetch(:frontend_aib_local_file, false)

      if file
        aib = File.join('terraform/transfer_files', file)
        aibs[aib] = aib
        aibs["#{aib}.md5"] = "#{aib}.md5"
     else
        logger.debug "Couldn't find frontend aib file to sync"
      end

      aibs
    end

    def aib_files
      @aib_files ||= {
        'manifest.json' => '.',
        'terraform/a2ha_manifest.auto.tfvars' => 'terraform/',
        'terraform/a2ha_aib_fe.auto.tfvars' => 'terraform/',
        'terraform/a2ha_aib_be.auto.tfvars' => 'terraform/',
      }.merge(aib_tfvar_files)
    end

    def sync_aibs
      unless source.aibs_exist?
        logger.error "Could not find all required aib files to sync, run again with `-v` for more info"
        return false
      end
      source.aib_files.each do |file,dest|
        source_path = source.path(file)
        logger.debug "Syncing #{source_path} to #{path(dest)}"
        sync_files(source_path, path(dest)) if Dir.glob(source_path).length > 0
      end
    end

    def aibs_exist?
      aib_files.keys.each do |file|
        found = Dir.glob(path(file)).length
        logger.debug "Checking for: #{file}: #{found > 0}"
        return false unless found > 0
      end

      return true
    end

    def a2ha_config_exist?
      File.exist? path('a2ha.rb')
    end

    def load_cluster_config(config_file)
      if File.exist?(config_file)
        AutomateCluster::Config.from_file(config_file)
      end

      # Update the workspace path to our new location
      AutomateCluster::Config.workspace_path = path

      if AutomateCluster::Config.architecture.nil?
        choices = AutomateCluster::Config::VALID_ARCHS.sort
        arch = prompt.select("Select an architecture", choices)
        AutomateCluster::Config.architecture = arch
      end
    end

    def copy_configs
      logger.debug "Syncing a2ha.rb config from source..."

      sync_files source.path('a2ha.rb'), path('a2ha.rb')
      load_cluster_config path('a2ha.rb')

      # Save source paths before we change any config settings
      source_key = source.path(AutomateCluster::Config.secrets_key_file)
      source_store = source.path(AutomateCluster::Config.secrets_store_file)
      source_automate_config = source.path(AutomateCluster::Config.automate.config_file)

      # Move secrets to the workspace
      FileUtils.mkdir_p path('secrets')
      AutomateCluster::Config.secrets_key_file = path('secrets/secrets.key')
      AutomateCluster::Config.secrets_store_file = path('secrets/secrets.json')

      sync_files source_key, AutomateCluster::Config.secrets_key_file if File.exist? source_key
      # Make sure secrets key is locked down
      FileUtils.chmod(0600, AutomateCluster::Config.secrets_key_file)
      sync_files source_store, AutomateCluster::Config.secrets_store_file if File.exist? source_store

      # Move automate config to new workspace
      AutomateCluster::Config.automate.config_file = path('configs/automate.toml')
      if File.exist?(source_automate_config)
        FileUtils.mkdir_p File.dirname(AutomateCluster::Config.automate.config_file)
        sync_files source_automate_config, AutomateCluster::Config.automate.config_file
      end
    end

    def sync_configs(opts = {})
      raise "No source set" unless source

      FileUtils.mkdir_p(workspace_path)

      if opts[:copy_configs] && source.a2ha_config_exist?
        copy_configs
      else
        load_cluster_config path('a2ha.rb')
      end

      logger.debug("Updating config file to latest version")
      save_cluster_config path('a2ha.rb')
    end

    def save_cluster_config(config_file)
      generator = AutomateCluster::ConfigGenerator.new(AutomateCluster::Config)
      File.open(config_file, 'w') do |fp|
        fp << generator.render
      end
    end

    def sync_files(from, dest)
      cmd = ['rsync -av', from, dest].join(' ')
      so = shellout!(cmd)

      if so.error?
        logger.error "rsync exited #{so.exitstatus}"
        logger.error so.stdout.chomp unless so.stdout.empty?
        logger.error so.stderr.chomp unless so.stderr.empty?
        exit(1)
      end

      logger.debug so.stdout.chomp unless so.stdout.empty?
    end
  end
end
