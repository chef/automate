# frozen_string_literal: true

require "assert/suite"

module Assert
  # This is the default suite used by assert. In addition to the base suite
  # behavior, it accumulates test/result counts in memory.  This data is used
  # by the runner/view for handling and presentation purposes.
  class DefaultSuite < Assert::Suite
    attr_reader :test_count, :result_count, :pass_result_count
    attr_reader :fail_result_count, :error_result_count
    attr_reader :skip_result_count, :ignore_result_count

    def initialize(config)
      super
      reset_run_data
    end

    # Callbacks

    def on_start
      reset_run_data
    end

    def before_test(_test)
      @test_count += 1
    end

    def on_result(result)
      @result_count += 1
      send("increment_#{result.type}_result_count")
    end

    private

    def increment_pass_result_count
      @pass_result_count += 1
    end

    def increment_fail_result_count
      @fail_result_count += 1
    end

    def increment_error_result_count
      @error_result_count += 1
    end

    def increment_skip_result_count
      @skip_result_count += 1
    end

    def increment_ignore_result_count
      @ignore_result_count += 1
    end

    def reset_run_data
      @test_count          = 0
      @result_count        = 0
      @pass_result_count   = 0
      @fail_result_count   = 0
      @error_result_count  = 0
      @skip_result_count   = 0
      @ignore_result_count = 0
    end
  end
end
