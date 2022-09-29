# frozen_string_literal: true

require "assert/config"
require "assert/config_helpers"
require "assert/suite"
require "assert/view_helpers"

module Assert
  class View
    include Assert::ConfigHelpers
    include Assert::ViewHelpers

    # this method is used to bring in custom user-specific views
    # require views by passing either a full path to the view ruby file
    # or passing the name of a view installed in ~/.assert/views

    def self.require_user_view(view_name)
      views_file = File.expand_path(
        File.join("#{ENV["HOME"]}/.assert/views", view_name, "lib", view_name),
      )

      if File.exist?(view_name) || File.exist?(view_name + ".rb")
        require view_name
      elsif File.exist?(views_file + ".rb")
        require views_file
      else
        msg = +"[WARN] Can't find or require #{view_name.inspect} view."
        unless view_name.match(%r{\A/})
          msg << " Did you install it in `~/.assert/views`?"
        end
        warn msg
      end
    end

    # setup options and their default values

    option "styled",        false
    option "pass_styles"    # none
    option "fail_styles"    # none
    option "error_styles"   # none
    option "skip_styles"    # none
    option "ignore_styles"  # none

    option "pass_abbrev",   "."
    option "fail_abbrev",   "F"
    option "ignore_abbrev", "I"
    option "skip_abbrev",   "S"
    option "error_abbrev",  "E"

    attr_reader :config

    def initialize(config, output_io)
      @config, @output_io, = config, output_io
      @output_io.sync = true if @output_io.respond_to?(:sync=)
    end

    def view
      self
    end

    def is_tty?
      !!@output_io.isatty
    end

    # Callbacks

    # define callback handlers to output information.  These will be called
    # by the test runner.

    # available callbacks from the runner:
    # * `before_load`:  called at the beginning, before the suite is loaded
    # * `after_load`:   called after the suite is loaded, just before `on_start`
    #                   functionally equivalent to `on_start`
    # * `on_start`:     called when a loaded test suite starts running
    # * `before_test`:  called before a test starts running
    #                   the test is passed as an arg
    # * `on_result`:    called when a running tests generates a result
    #                   the result is passed as an arg
    # * `after_test`:   called after a test finishes running
    #                   the test is passed as an arg
    # * `on_finish`:    called when the test suite is finished running
    # * `on_info`:      called when the INFO signal is triggered whil runninng
    #                   the test suite
    # * `on_interrupt`: called when the test suite is interrupted while running
    #                   the interrupt exception is passed as an arg
    def before_load(test_files)
    end

    def after_load
    end

    def on_start
    end

    def before_test(test)
    end

    def on_result(result)
    end

    def after_test(test)
    end

    def on_finish
    end

    def on_info(test)
    end

    def on_interrupt(err)
    end

    # IO capture

    def puts(*args)
      @output_io.puts(*args)
    end

    def print(*args)
      @output_io.print(*args)
    end
  end
end
