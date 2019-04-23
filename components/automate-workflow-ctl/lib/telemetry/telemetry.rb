require 'fileutils'

module Telemetry
  class << self

    def telemetry_disabled_file
      "/var/opt/delivery/.telemetry.disabled"
    end

    def telemetry_enabled?
      !telemetry_disabled?
    end

    def telemetry_disabled?
      File.exist?(telemetry_disabled_file)
    end

    def enable_for_server
      FileUtils.rm_f(telemetry_disabled_file)
    end

    def disable_for_server
      FileUtils.mkdir_p(File.dirname(telemetry_disabled_file))
      FileUtils.touch(telemetry_disabled_file)
    end

    def legal_notice
      if telemetry_disabled?
        <<-NOTICE
Users of this Automate server cannot choose to share anonymized usage data
with Chef Software, Inc. Chef uses this shared data to improve Automate.
Please consider allowing your users to share this data by running
`workflow-ctl telemetry enable`.
NOTICE
      else
        <<-NOTICE
Users of this Automate server may elect to share anonymized usage data with
Chef Software, Inc. Chef uses this shared data to improve Automate.
Please visit https://chef.io/privacy-policy for more information about the
information Chef collects, and how that information is used.
NOTICE
      end
    end
  end
end
