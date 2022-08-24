require "digest/sha1" unless defined?(Digest::SHA1)
require "securerandom" unless defined?(SecureRandom)
require "json" unless defined?(JSON)

module Train::Platforms::Detect
  class UUID
    include Train::Platforms::Detect::Helpers::OSCommon

    def initialize(platform)
      @platform = platform
      @backend = @platform.backend
    end

    def find_or_create_uuid
      # for api transports uuid is defined on the connection
      if defined?(@backend.unique_identifier)
        uuid_from_string(@backend.unique_identifier)
      elsif @platform.unix?
        unix_uuid
      elsif @platform.windows?
        windows_uuid
      else
        # Checking "unknown" :uuid_command which is set for mock transport.
        if @platform[:uuid_command] && !@platform[:uuid_command] == "unknown"
          result = @backend.run_command(@platform[:uuid_command])
          return uuid_from_string(result.stdout.chomp) if result.exit_status == 0 && !result.stdout.empty?
        end

        raise Train::PlatformUuidDetectionFailed.new("Could not find platform uuid! Please set a uuid_command for your platform.")
      end
    end
  end
end
