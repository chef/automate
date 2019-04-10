require 'ctl-helpers/color_printer'

module CtlHelpers
  # Very basic usage parser for omnibus-ctl commands.
  # Written to address several delivery-ctl commands that didn't respond to --help.
  #
  # When using with omnibus-ctl, should remove leading args before passing in ARGV.
  # For example, your command "my-command" should receive ARGV like:
  #   ARGV: [delivery /opt/delivery/embedded/service/omnibus-ctl my-command some args]
  # But you should simply pass:
  #   args: [some args]
  # So, you probably wanna initalize this class like:
  #   CtlHelpers::BasicUsage.new(ARGV[3..-1], "my-command", "Optional description.")
  class BasicUsage

    include CtlHelpers::ColorPrinter

    def initialize(args, usage_line, description=nil)
      @args = args
      @usage_line = usage_line
      @description = description
    end

    def parse_help_and_display_usage
      @args.each do |arg|
        if arg == "--help"
          puts_usage(@usage_line, @description)
          exit 0
        end
      end
    end

  end
end
