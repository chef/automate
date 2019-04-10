module Setup
  module Exceptions
    class BadArgumentError < RuntimeError ; end

    # The things that can go wrong based on the inputs the user
    # gives us:
    class ArgumentExistsAsConfigValueError < BadArgumentError
     def initialize(description, config_path, key_name, value)
       message =
%Q{#{description} is already set in #{config_path} as:
  #{key_name} = '#{value}'
Please change it there, or remove that setting to use this option.}
       super(message)
     end
    end

    class EnterpriseAlreadyExists  < BadArgumentError ; end

    # reconfigure/chef client run failures:
    class ChefClientRunFailed < StandardError
      attr_reader :exitstatus
      def initialize(exitstatus)
        @exitstatus = exitstatus
      end
    end

    class ReconfigureFailed < StandardError
      attr_reader :exitstatus
      def initialize(exitstatus)
        @exitstatus = exitstatus
      end
    end

    # The things that can go wrong when we interface with
    # EnterpriseCtl:
    class EnterpriseCtlError < StandardError ; end

    class EnterpriseCtlCommandFailed < EnterpriseCtlError
      attr_reader :exitstatus, :command
      def initialize(description, exitstatus, command)
        @exitstatus = exitstatus
        @command = Array(command).join(" ")
        super(description)
      end
    end

    class EnterpriseCreationFailed < EnterpriseCtlCommandFailed
      def self.from_ctl_command_failed(ex)
        EnterpriseCreationFailed.new(ex.message, ex.exitstatus, ex.command)
      end
    end

    # Config loading failures
    class FileIOError < RuntimeError ; end
  end
end
