require 'ctl-helpers/exceptions'
require 'runner/exceptions'
require 'runner/manage_runner'
require 'ctl-helpers/color_printer'

module Runner
  module Delete
    class Command

      include CtlHelpers::ColorPrinter

      DESCRIPTION = "Delete a runner in Automate."
      def initialize(args)
        @args = args
      end

      def run
        begin
          delete_runner = Runner::ManageRunner.new(@args, DESCRIPTION)
          delete_runner.delete
        rescue Runner::Exceptions::BadArgumentError => e
          puts_error "\n#{e.message}"
          exit 1
        rescue CtlHelpers::Exceptions::ActionCanceled
          puts_error "\nNo action taken, exiting."
        end
      end
    end
  end
end
