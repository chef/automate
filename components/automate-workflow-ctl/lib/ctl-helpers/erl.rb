#
# Copyright:: Copyright (c) 2016 Chef Software, Inc.
#

module CtlHelpers
  class Erl

    def self.erl_cookie
      begin
        # The delivery cookbook will create a magic cookie and store the value here
        secrets_file = "/etc/delivery/delivery-secrets.json"
        if ENV['ERL_COOKIE'] != nil
          ENV['ERL_COOKIE']
        elsif File.exist?(secrets_file)
          JSON.parse(File.read(secrets_file))["delivery"]["erl_cookie"]
        else
          # Fallback to the old 'delivery' which is only used in dev environments
          "delivery"
        end
      end
    end

    def self.escript_path
      if ENV["DELIV_DEV"]
        "/mnt/sync/delivery/server/escript"
      elsif ENV['ESCRIPT_PATH'] != nil
        ENV['ESCRIPT_PATH']
      else
        "/opt/delivery/embedded/service/delivery/bin"
      end
    end
  end
end
