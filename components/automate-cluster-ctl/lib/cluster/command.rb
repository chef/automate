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
require 'clamp'
require 'tty-logger'
require_relative '../cluster'
require_relative 'helpers'
require_relative 'config'
require_relative 'terraform'
require_relative 'secrets'

$PROGRAM_NAME=ENV['_CLUSTER_CTL_CMD']

module AutomateCluster
  class Command < Clamp::Command
    include AutomateCluster::Helpers

    option ["-v", "--verbose"], :flag, "Enable verbose output"
    option ["-c", "--config-file"], 'CONFIG_FILE', "Load settings from config file", default: File.join(Dir.pwd, 'a2ha.rb')

    def parse(*args)
      super
      AutomateCluster::log_level = verbose? ? :debug : :info
    end

    def run(*args)
      _load_config_file
      _load_secrets
      super
    end

    def logger
      AutomateCluster.logger
    end

    def config
      AutomateCluster::Config
    end

    def secrets
      AutomateCluster::Secrets.instance
    end

    # Load the secrets config into the secrets singleton
    def _load_secrets
      AutomateCluster::Secrets.from_file(config.secrets_key_file, config.secrets_store_file)
      # Make sure to filter out any secrets from the logs
      logger.filter(secrets.map { |k,v| v })
    end

    # The default is to require a config file be present for all commands.
    # In the case of the `config init` command and other we might want to skip this requirement
    # Example:
    #
    # class AutomateClusterConfigInit < AutomateCluster::SubCommand
    #   skip_config_file
    #
    #   option ['-a', '--architecture'], 'ARCH', 'Architecture type to use when generating the config'
    #
    #   def execute
    #     ...
    #   end
    # end
    def self.skip_config_file
      @skip_config_file = true
    end

    def self.require_config_file
      !@skip_config_file
    end

    def require_config_file
      self.class.require_config_file
    end

    def _load_config_file
      signal_usage_error "Unable to load config file #{config_file}" if require_config_file && !File.exists?(config_file)
      AutomateCluster::Config.from_file(config_file) if File.exists?(config_file)
    rescue Mixlib::Config::UnknownConfigOptionError => e
      logger.error "Errors detected in '#{config_file}': #{e.message}"
      exit
    end
  end

  class SubCommand < Command
    # This is to prevent loading the secrets twice from a subcommand
    def _load_secrets
    end
  end
end
Clamp.allow_options_after_parameters = true
