require 'optparse'

module ChefCliUnity
  # Class that sets up optparse with help and a nice standard for usage.
  #
  # See lib/runner/install/options.rb for a good
  # child class example.
  class Options
    attr_reader :options, :option_parser

    def initialize(args, usage_line, arguments: nil, description: nil)
      @options = {}
      @args = args
      @usage_line = usage_line
      @arguments = arguments
      @description = description

      OptionParser.new do |opts|
        @option_parser = opts
        opts.banner = usage
        opts.on("-h", "--help", "Prints this help") do
          puts opts
          exit
        end

        additional_options(opts)
      end.parse!(args)
    end

    def additional_options(opts)
      # Override this method if you want to add options.
      # Here's an example but see optparse for all the possibilities:
      # opts.on("-m", "--my-options MY_OPTION", "My opt") do |my_option|
      #   options[:my_option] = my_option
      #   ...whatever else you want to happen here...
      # end
    end

    # Gives you nice usage output from --help based on what you
    # initialized with. Skips outputting sections you didn't pass.
    def usage
      output = ""
      output << "#{@description}\n\n" if @description
      output << "Usage:\n"
      output << "  workflow-ctl " + @usage_line + "\n\n"

      if @arguments
        output << "Arguments:\n"
        @arguments.each do |argument, desc|
          output << "  #{argument} #{desc}\n"
        end
        output << "\n"
      end

      if options.length > 0
        output << "Options:\n"
      end
      output
    end

  end
end
