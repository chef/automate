# frozen_string_literal: true

require "assert/config_helpers"
require "assert/test"

module Assert
  # This is the base suite. It loads the tests to run in memory and provides
  # methods for these tests that the runner/view uses for handling and
  # presentation purposes. It also stores suite-level setups and teardowns.
  # Override the test/result count methods and the callbacks as needed.  See
  # the default suite for example usage.
  class Suite
    include Assert::ConfigHelpers

    # A suite is a set of tests to run.  When a test class subclasses
    # the Context class, that test class is pushed to the suite.

    attr_reader :config, :setups, :teardowns
    attr_accessor :start_time, :end_time

    def initialize(config)
      @config     = config
      @tests      = []
      @setups     = []
      @teardowns  = []
      @start_time = Time.now
      @end_time   = @start_time
    end

    def suite
      self
    end

    def setup(&block)
      setups << (block || proc{})
    end
    alias_method :startup, :setup

    def teardown(&block)
      teardowns << (block || proc{})
    end
    alias_method :shutdown, :teardown

    def tests_to_run?
      @tests.any?
    end

    def tests_to_run_count
      @tests.size
    end

    def clear_tests_to_run
      @tests.clear
    end

    def find_test_to_run(file_line)
      @tests.find{ |t| t.file_line == file_line }
    end

    def sorted_tests_to_run(&sort_by_proc)
      @tests.sort.sort_by(&sort_by_proc)
    end

    def test_count
    end

    def result_count
    end

    def pass_result_count
    end

    def fail_result_count
    end

    def error_result_count
    end

    def skip_result_count
    end

    def ignore_result_count
    end

    def run_time
      @end_time - @start_time
    end

    def test_rate
      get_rate(test_count, run_time)
    end

    def result_rate
      get_rate(result_count, run_time)
    end

    # Callbacks

    # define callback handlers to do special behavior during the test run.
    # These will be called by the test runner.

    def before_load(test_files)
    end

    # this is required to load tests into the suite, be sure to `super` if you
    # override this method
    def on_test(test)
      @tests << test
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

    def inspect
      "#<#{self.class}:#{"0x0%x" % (object_id << 1)}"\
      " test_count=#{test_count.inspect}"\
      " result_count=#{result_count.inspect}>"
    end
  end
end
