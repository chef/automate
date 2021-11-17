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
require 'logger'
require 'mixlib/shellout'
require 'tty-spinner'
require 'tty-prompt'

module AutomateCluster
  class UsageError < StandardError; end

  module Helpers
    def config
      AutomateCluster::Config
    end

    def term
      @term ||= Pastel.new
    end

    def spinner
      TTY::Spinner.new("[:spinner] :title")
    end

    def prompt
      @prompt ||= TTY::Prompt.new
    end

    def root_path
      ENV['_CLUSTER_CTL_ROOT']
    end

    def workspace_path
      AutomateCluster::Config.workspace_path
    end

    def profile_path
      path = ENV['_CLUSTER_CTL_VERSION'] == 'DEV' ? '/src' : root_path
      File.join(path, 'inspec')
    end

    def terraform_path
      File.join(workspace_path, 'terraform')
    end

    def shellout!(cmd, opts = {})
      opts[:environment] ||= {}
      opts[:environment]['HAB_LICENSE'] ||= 'accept-no-persist'

      AutomateCluster.logger.debug("Running: `#{cmd}`", opts: opts)
      so = _shellout(cmd, opts)

      so.run_command
      so.error!
      so
    rescue Mixlib::ShellOut::CommandTimeout => e
      AutomateCluster.logger.error e.message, cmd: cmd, opts: opts

      so
    rescue Mixlib::ShellOut::ShellCommandFailed => e
      message = ["Error running #{cmd}"]
      if so.stderr
        message << term.red("\nCommand exited '#{so.exitstatus}' with the following messages")
        message << '-' * 80
        message << so.stdout.chomp unless so.stdout.empty?
        message << so.stderr.chomp unless so.stderr.empty?
        message << '-' * 80 + "\n"
      end
      AutomateCluster.logger.error message.compact.join("\n").force_encoding('UTF-8')

      so
    end

    def _shellout(cmd, opts)
      Mixlib::ShellOut.new(cmd, **opts)
    end

    def run_make_cmd(cmd, opts = {})
      opts ||= {}
      opts[:cwd] ||= workspace_path

      so = shellout!("make #{cmd}", opts)
      AutomateCluster.logger.debug so.stdout, cmd: "make #{cmd}"
      so
    end

    def wait_while(title, exit_on_failure = false, &block)
      spinner.run do |spinner|
        spinner.update(title: title)
        if yield spinner
          spinner.success("(#{term.green('completed')})")
        else
          spinner.success("(#{term.red('failed')})")
          exit 1 if exit_on_failure
        end
      end
    end

    def login_with_okta
      okta_cmd = "okta_aws #{config.aws.profile}"
      so = shellout!(okta_cmd)

      if so.error?
        AutomateCluster.logger.error "okta_aws failed to run, please run `#{okta_cmd}` manually and resolve any errors"
        exit 1
      end

      so
    end

    def terraform_run(cmd, opts = {})
      opts ||= {}
      opts[:cwd] = terraform_path
      opts[:environment] ||= {}
      opts[:environment]['TF_IN_AUTOMATION'] ||= 'true'
      opts[:environment]['TF_INPUT'] = '0'

      if opts.key?(:timeout)
        # we want to make sure we have at least 1 hour for terraform commands to timeout
        opts[:timeout] = [opts[:timeout], 3600].max
      end
      opts[:timeout] ||= 3600

      so = shellout!("terraform #{cmd}", opts)
      AutomateCluster.logger.debug so.stdout, cmd: "terraform #{cmd}" unless so.stdout.empty?
      AutomateCluster.logger.debug so.stderr, cmd: "terraform #{cmd}" unless so.stderr.empty?

      so
    end
  end
end
