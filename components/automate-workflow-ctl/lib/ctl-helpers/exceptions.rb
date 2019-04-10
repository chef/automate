module CtlHelpers
  module Exceptions
    class BaseError < StandardError
      attr_accessor :remediation_steps
      def initialize(message = nil, remediation = nil)
        super(message)
        @remediation_steps = remediation
      end
    end

    class ConfigurationError < BaseError ; end

    class ActionCanceled < StandardError ; end
  end
end
