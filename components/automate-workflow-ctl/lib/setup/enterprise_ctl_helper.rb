require "ctl-helpers/erl"
require 'setup/exceptions'
require 'mixlib/shellout'

module Setup
  class EnterpriseCtlHelper

    # Creates an enterprise with the specified name
    def create_enterprise(name)
      credentials = ctl_command(['create', 'enterprise', name, '--ssh-pub-key-file', '/etc/delivery/builder_key.pub'])
      File.write("/etc/delivery/#{name}-admin-credentials", credentials)
      credentials
    rescue Setup::Exceptions::EnterpriseCtlCommandFailed => ex
      raise Setup::Exceptions::EnterpriseCreationFailed.from_ctl_command_failed(ex)
    end

    # returns true if at least one enterprise exists
    def any_enterprise_exists?
      return @enterprises unless @enterprises.nil?
      enterprises = ctl_command(["list", "enterprises"])
      @enterprises = (enterprises.chomp.length > 0)
    rescue Setup::Exceptions::EnterpriseCtlCommandFailed
      # Most likely to occur if the server is not yet configured or is shut down
      # Future iterations will add more granularity so that we can report
      # meaningful next steps (complete setup first, start services, etc)
      false
    end

    def ctl_command(args)
      r = run_command(["/opt/delivery/embedded/service/delivery/bin/enterprise_ctl"] + args)
      if r[:status] == 0
        r[:response]
      else
        raise Setup::Exceptions::EnterpriseCtlCommandFailed.new(r[:response], r[:status], r[:command])
      end
    end

    private

    def run_command(cmd)
      so = Mixlib::ShellOut.new(cmd, env: { "ERL_COOKIE" => CtlHelpers::Erl.erl_cookie })
      so.run_command
      result = {}
      result[:command] = cmd
      result[:status] = so.status.exitstatus
      result[:response] = so.stdout
      result
    end
  end
end
