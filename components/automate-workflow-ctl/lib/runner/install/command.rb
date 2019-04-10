require 'ctl-helpers/exceptions'
require 'runner/exceptions'
require 'runner/manage_runner'
require 'ctl-helpers/color_printer'

module Runner
  module Install
    class Command

      include CtlHelpers::ColorPrinter

      DESCRIPTION = "Configure a new runner in Automate for job dispatch."
      def initialize(args)
        @args = args
      end

      def run
        begin
          installer = Runner::ManageRunner.new(@args, DESCRIPTION)
          installer.configure!
          installer.install
        rescue Runner::Exceptions::BadArgumentError => e
          puts_error "\n#{e.message}"
          exit 1
        rescue CtlHelpers::Exceptions::ActionCanceled
          puts_error "\nNo action taken, exiting."
          exit 1
        end
      end
    end
  end
end
