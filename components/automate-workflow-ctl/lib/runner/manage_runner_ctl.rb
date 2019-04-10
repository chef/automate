require 'httpclient'
require 'json'
require 'mixlib/shellout'
require 'runner/exceptions'
require "ctl-helpers/erl"

module Runner
  # Wraps API calls to the local Delivery Server
  class ManageRunnerCtl

    attr_reader :enterprise

    def initialize(config)
      @enterprise = config.enterprise || ""
    end

    def create_runner(runner_info)
      cmd = "./runner_ctl create #{enterprise}"
      [:hostname, :os, :platform_family, :platform, :platform_version].each do |k|
        cmd += " #{runner_info[k.to_s]}"
      end
      status = escript_run(cmd)
      if status.error?
        raise Runner::Exceptions::RunnerCtlFailed.new("\nThe runner could not be created because of the following error:\n\n #{status.stdout}\n #{status.stderr}\n")
      end
      status.stdout
    end

    def validate_runner(runner_info)
      cmd = "./runner_ctl validate_runner #{enterprise} #{runner_info[:hostname.to_s]}"
      status = escript_run(cmd)
      if status.error?
        raise Runner::Exceptions::RunnerCtlFailed.new("\n\n #{status.stdout}\n #{status.stderr}\n")
      end
      status.stdout
    end

    def delete_runner(runner_info)
      cmd = "./runner_ctl delete #{enterprise} #{runner_info[:hostname.to_s]}"
      status = escript_run(cmd)
      if status.error?
        raise Runner::Exceptions::RunnerCtlFailed.new("\nThe runner could not be deleted because of the following error:\n\n #{status.stdout}\n #{status.stderr}\n")
      else
        puts "Runner #{runner_info[:hostname.to_s]} successfully deleted"
      end
      status.stdout
    end

    def escript_run(cmd)
      Mixlib::ShellOut.new(
        cmd,
        cwd: CtlHelpers::Erl.escript_path,
        env: { "ERL_COOKIE" => CtlHelpers::Erl.erl_cookie }
      ).run_command
    end
  end
end
