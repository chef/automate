require 'io/console'
require 'ctl-helpers/prompt'
require 'ctl-helpers/color_printer'

require 'runner/delete/options'

module Runner
  module Delete
    class Config

      # overrides puts / print / puts_indented
      include CtlHelpers::ColorPrinter

      attr_reader :fqdn, :enterprise

      def initialize(args, description)
        @args = args
        runner_options = Options.new(args, description)
        @option_parser = runner_options.option_parser
        @options = runner_options.options
      rescue OptionParser::MissingArgument,
             OptionParser::InvalidArgument,
             OptionParser::InvalidOption,
             OptionParser::MissingArgument => e
        raise Runner::Exceptions::BadArgumentError.new e.message
      end

      def confirm_config
        unless @yes
          display_config
          prompt_for_config
        end
      end

      def display_config
        puts "Is this the runner you would like to " \
               "delete? (you can skip this step by passing -y):\n"
        puts_indented(2, "Runner FQDN: #{@fqdn}")
      end

      def prompt_for_config
        prompt = "(y/n)"
        unless CtlHelpers::Prompt.yes_no_prompt(prompt)
          puts "You chose no. Please re-run the command with the updated settings you want. Exiting..."
          exit 0
        end
        puts ""
      end

      def display_usage
        puts @option_parser
      end

      def validate!
        # set arguments
        @fqdn = @args[0]
        if @fqdn.nil?
          raise Runner::Exceptions::BadArgumentError.new("Please provide the FQDN of the runner to be deleted as an argument")
        end
        # set options
        @enterprise = @options[:enterprise]
        @yes = @options[:yes]
      end

    end
  end
end
