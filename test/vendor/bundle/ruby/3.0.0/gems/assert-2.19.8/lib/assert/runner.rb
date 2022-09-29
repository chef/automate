# frozen_string_literal: true

require "assert/config_helpers"
require "assert/suite"
require "assert/view"

module Assert
  class Runner
    include Assert::ConfigHelpers

    attr_reader :config

    def initialize(config)
      @config = config
    end

    def runner
      self
    end

    def run
      on_start
      suite.on_start
      view.on_start

      if single_test?
        view.print "Running test: #{single_test_file_line}"
      elsif tests_to_run?
        view.print "Running tests in random order"
      end
      view.puts ", seeded with \"#{runner_seed}\"" if tests_to_run?

      @current_running_test = nil

      # if SIGINFO available (ie on OSX, not on BSD) and if SIGINFO requested
      # (Ctrl+T on Macs), process it
      if Signal.list.keys.include?("INFO")
        Signal.trap("INFO") do
          on_info(@current_running_test)
          suite.on_info(@current_running_test)
          view.on_info(@current_running_test)
        end
      end

      begin
        suite.start_time = Time.now
        suite.setups.each(&:call)
        tests_to_run.tap{ suite.clear_tests_to_run }.delete_if do |test|
          @current_running_test = test

          before_test(test)
          suite.before_test(test)
          view.before_test(test)
          test.run do |result|
            on_result(result)
            suite.on_result(result)
            view.on_result(result)
          end
          after_test(test)
          suite.after_test(test)
          view.after_test(test)

          # always delete `test` from `tests_to_run` since it has been run
          true
        end
        suite.teardowns.each(&:call)
        suite.end_time = Time.now
      rescue Interrupt => ex
        on_interrupt(ex)
        suite.on_interrupt(ex)
        view.on_interrupt(ex)
        raise(ex)
      end

      (fail_result_count + error_result_count).tap do
        view.on_finish
        suite.on_finish
        on_finish
      end
    end

    # Callbacks

    # define callback handlers to do special behavior during the test run. These
    # will be called by the test runner

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

    private

    def tests_to_run
      srand runner_seed
      if single_test?
        [suite.find_test_to_run(single_test_file_line)].compact
      else
        suite.sorted_tests_to_run{ rand tests_to_run_count }
      end
    end
  end
end
