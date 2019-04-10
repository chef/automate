require "telemetry/options"
require "telemetry/telemetry"
require "ctl-helpers/exit_handler"
require "ctl-helpers/color_printer"

module Telemetry
  class Command

    include CtlHelpers::ColorPrinter

    DESCRIPTION = "Configure the Chef telemetry preferences"
    def initialize(args=[])
      @args = args
      @options = Telemetry::Options.new(args, DESCRIPTION)
    end

    def run
      CtlHelpers::ExitHandler.handle_exit do
        enable_telemetry = @args[0]
        case enable_telemetry
        when "enable"
          Telemetry.enable_for_server
        when "disable"
          Telemetry.disable_for_server
        when "status"
          # We are printing the status below - so do nothing.
        else
          puts_error("You passed the argument '#{enable_telemetry}'. Valid options are 'enable, 'disable' or 'status'.\n\nHelp:", "  #{@options.usage}")
          exit 1
        end

        puts Telemetry.legal_notice
      end
    end
  end
end
