require 'highline'

module CtlHelpers
  # Module for printing things in color and encapsulating output in CLI commands.
  # Simply `include CtlHelpers::ColorPrinter` to replace IO.puts and IO.print with this.
  # Will also let you write things like `puts_blue("a string that will print blue")` in your code.
  module ColorPrinter
    # Colors that have been tested to be colorblind friendly.
    # Recommended to be used over untested colors.
    ERROR_WARNING_COLOR = :yellow
    INFORMATION_COLOR = :blue

    HighLine.colorize_strings

    def puts(print_str="", color=:default)
      puts_color_to_stream(colorize_string(print_str, color))
    end

    def print(print_str="", color=:default)
      print_color_to_stream(colorize_string(print_str, color))
    end

    HighLine::COLORS.each do |color, _|
      color = color.to_s.downcase
      define_method("puts_#{color}") do |print_str|
        puts(print_str, color.to_sym)
      end

      define_method("print_#{color}") do |print_str|
        print(print_str, color.to_sym)
      end
    end

    def puts_default(print_str)
      puts(print_str, :default)
    end

    def print_default(print_str)
      print(print_str, :default)
    end

    def puts_error(error_str, extra_info_str=nil)
      print("ERROR: ", ERROR_WARNING_COLOR)
      puts(error_str + "\n")
      puts(extra_info_str) if extra_info_str
    end

    def puts_indented(spaces, string)
      to_print = (" "*spaces) + string
      puts(to_print)
    end

    def print_indented(spaces, string)
      to_print = (" "*spaces) + string
      print(to_print)
    end

    def puts_information(print_str)
      puts(print_str, INFORMATION_COLOR)
    end

    def print_information(print_str)
      print(print_str, INFORMATION_COLOR)
    end

    def puts_warning(print_str)
      puts(print_str, ERROR_WARNING_COLOR)
    end

    def print_warning(print_str)
      print(print_str, ERROR_WARNING_COLOR)
    end

    # Example:
    #  string:
    #    "This is some important top level info"
    #  outputs in info color:
    #    "This is some important top level info..."
    def puts_top_level_step(string)
      puts_information(string + "...")
    end

    # Example:
    #  string:
    #    "This is some substep info that goes below top level info properly tabbed."
    #  outputs in default color:
    #     "  This is some substep info that goes below top level info properly tabbed."
    def puts_substep(string)
      puts_default("  " + string)
    end

    # Outputs usage in a format similar to OptParse.
    # Should only be used on very similar commands that
    # aren't doing much validation. Was written to
    # get --help output for every commmand.
    #
    # Example:
    #  usage_line: "my_command [options...]"
    #  description: "This is some high level description that
    #                I maybe reused from add_command."
    def puts_usage(usage_line, description=nil)
      puts "#{description}\n\n" if description
      puts "Usage: "
      puts "   workflow-ctl " + usage_line
    end

    private

    # TODO: could also output to logs optionally if we wanted to here
    def puts_color_to_stream(print_str)
      $stdout.puts print_str
    end

    def print_color_to_stream(print_str)
      $stdout.print print_str
    end

    def colorize_string(string, color)
      if color == :default
        string
      else
        string.respond_to?(color) ? string.send(color) : string
      end
    end

  end
end
