module Runner
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
        super("You provided a '#{got_package}' installer, but the runner is running #{target_os} which expects a #{expected_package} installer.",
              "Please provide a #{expected_package} installer for this runner.")
      end
    end

    class UnsupportedTargetOS < BaseError
      def initialize(os)
        super("Your runner operating system #{os} is not yet supported.",
              "Please use a CentOS, Debian, Oracle Linux, SUSE Enterprise Linux, or Ubuntu runner with this command.")
      end
    end

    class CustomCertfileNotFound < BaseError
      def initialize(fips_custom_cert_filename)
        base_error = "You passed the option --fips-custom-cert-filename #{fips_custom_cert_filename} but the file does not exist."
        remediation = "This option is only needed if you wish to run in FIPS mode with a self-signed certificate. "\
                      "To get started with self-signed certificates in FIPS mode, please see the FIPS Automate documentation for more information."
        super(base_error, remediation)
      end
    end

    class BadArgumentError < BaseError ; end

    class CertFetchFailed < BaseError ; end

    class KnifeCommandFailed  < BaseError ; end

    class DeliveryAPIRequestFailed  < BaseError ; end
    class RunnerCtlFailed  < BaseError ; end

    class ConfigurationError < BaseError ; end

    class NoConnection < BaseError ; end

    class AlreadyRegistered < BaseError ; end
    class NodeExists < AlreadyRegistered ; end
    class ClientExists < AlreadyRegistered ; end
    class LegacyNode < AlreadyRegistered ; end

    class RemoteCopyFailed < BaseError ; end

    class RemoteConnectionFailed < BaseError ; end

    class RemoteExecutionFailed < BaseError ; end
    class ChefDK404 < BaseError ; end
    class ChefDKHTTPError < BaseError ; end
  end
end
