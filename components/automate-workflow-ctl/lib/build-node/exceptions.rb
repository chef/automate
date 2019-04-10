module BuildNode
  module Exceptions
    class BaseError < StandardError
      attr_accessor :remediation_steps
      def initialize(message = nil, remediation = nil)
        super(message)
        @remediation_steps = remediation
      end
    end

    class PackageTargetMismatch < BaseError
      def initialize(target_os, got_package, expected_package)
        super("You provided a '#{got_package}' installer, but the build node is running #{target_os} which expects a #{expected_package} installer.",
              "Please provide a #{expected_package} installer for this build node.")
      end
    end

    class UnsupportedTargetOS < BaseError
      def initialize(os)
        super("Your build node operating system #{os} is not yet supported",
              "Please use an Ubuntu, Debian, CentOS, or RHEL build node with this command.")
      end
    end

    class BadArgumentError < BaseError ; end

    class CertFetchFailed < BaseError ; end

    class KnifeCommandFailed  < BaseError ; end

    class DeliveryAPIRequestFailed  < BaseError ; end

    class ConfigurationError < BaseError ; end

    class NoConnection < BaseError ; end

    class AlreadyRegistered < BaseError ; end
    class NodeExists < AlreadyRegistered ; end
    class ClientExists < AlreadyRegistered ; end
    class LegacyNode < AlreadyRegistered ; end

    class RemoteCopyFailed < BaseError ; end

    class RemoteConnectionFailed < BaseError ; end

    class RemoteExecutionFailed < BaseError ; end
  end
end
