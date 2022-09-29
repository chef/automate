# frozen_string_literal: true

require "assert/actual_value"
require "assert/assertions"
require "assert/context/let_dsl"
require "assert/context/method_missing"
require "assert/context/setup_dsl"
require "assert/context/subject_dsl"
require "assert/context/suite_dsl"
require "assert/context/test_dsl"
require "assert/context_info"
require "assert/macros/methods"
require "assert/result"
require "assert/suite"
require "assert/utils"

module Assert
  # A Context is a scope for tests to run in. Contexts have setup and teardown
  # blocks, subjects, and descriptions. Tests are run in the scope of a Context
  # instance. Therefore, a Context should have minimal base
  # logic/methods/instance_vars. The instance should remain pure to not pollute
  # test scopes.
  class Context
    # Put all logic in DSL methods to keep context instances pure.
    extend SetupDSL
    extend SubjectDSL
    extend SuiteDSL
    extend TestDSL
    extend LetDSL
    include MethodMissing
    include Assert::Assertions
    include Assert::Macros::Methods

    def initialize(running_test, config, result_callback)
      @__assert_running_test__ = running_test
      @__assert_config__       = config
      @__assert_with_bt__      = []
      @__assert_pending__      = 0

      @__assert_result_callback__ =
        proc do |result|
          unless @__assert_with_bt__.empty?
            result.set_with_bt(@__assert_with_bt__.dup)
          end
          result_callback.call(result)
          result
        end
    end

    # Check if the result is true. If so, create a new pass result,  Otherwise
    # create a new fail result with the desc and fail msg.
    def assert(assertion, desc = nil)
      if assertion
        pass
      else
        what =
          if block_given?
            yield
          else
            "Failed assert: assertion was "\
            "`#{Assert::U.show(assertion, __assert_config__)}`."
          end
        fail(fail_message(desc, what))
      end
    end

    # The opposite of assert. Check if the result is false. If so, create a new
    # pass result. Otherwise create a new fail result with the desc and
    # fail msg.
    def assert_not(assertion, fail_desc = nil)
      assert(!assertion, fail_desc) do
        "Failed assert_not: assertion was "\
        "`#{Assert::U.show(assertion, __assert_config__)}`."
      end
    end
    alias_method :refute, :assert_not

    def assert_that(
          actual_value = Assert::ActualValue.not_given,
          &actual_value_block)
      Assert::ActualValue.new(actual_value, context: self, &actual_value_block)
    end

    # adds a Pass result to the end of the test's results
    # does not break test execution
    def pass(pass_msg = nil)
      if @__assert_pending__ == 0
        capture_result(Assert::Result::Pass, pass_msg)
      else
        capture_result(
          Assert::Result::Fail,
          "Pending pass (make it not pending)",
        )
      end
    end

    # adds an Ignore result to the end of the test's results
    # does not break test execution
    def ignore(ignore_msg = nil)
      capture_result(Assert::Result::Ignore, ignore_msg)
    end

    # adds a Fail result to the end of the test's results
    # break test execution if assert is configured to halt on failures
    def fail(message = nil)
      if @__assert_pending__ == 0
        if halt_on_fail?
          raise Result::TestFailure, message || ""
        else
          capture_result(Assert::Result::Fail, message || "")
        end
      elsif halt_on_fail?
        raise Result::TestSkipped, "Pending fail: #{message || ""}"
      else
        capture_result(Assert::Result::Skip, "Pending fail: #{message || ""}")
      end
    end
    alias_method :flunk, :fail

    # adds a Skip result to the end of the test's results
    # breaks test execution
    def skip(skip_msg = nil, called_from = nil)
      raise Result::TestSkipped, (skip_msg || ""), called_from
    end

    # runs block and any fails are skips and any passes are fails
    def pending(&block)
      begin
        @__assert_pending__ += 1
        instance_eval(&block)
      ensure
        @__assert_pending__ -= 1
      end
    end

    # alter the backtraces of fail/skip results generated in the given block
    def with_backtrace(bt, &block)
      bt ||= []
      begin
        @__assert_with_bt__.push(bt.first)
        instance_eval(&block)
      rescue Result::TestSkipped, Result::TestFailure => ex
        if ex.assert_with_bt.nil? && !@__assert_with_bt__.empty?
          ex.assert_with_bt = @__assert_with_bt__.dup
        end
        raise(ex)
      ensure
        @__assert_with_bt__.pop
      end
    end

    def subject
      unless instance_variable_defined?("@__assert_subject__")
        @__assert_subject__ =
          instance_eval(&self.class.subject) if self.class.subject
      end

      @__assert_subject__
    end

    def inspect
      "#<#{self.class}>"
    end

    protected

    # Returns a Proc that will output a custom message along with the default
    # fail message.
    def fail_message(fail_desc = nil, what_failed_msg = nil)
      [fail_desc, what_failed_msg].compact.join("\n")
    end

    private

    def halt_on_fail?
      __assert_config__.halt_on_fail
    end

    def capture_result(result_klass, msg)
      @__assert_result_callback__.call(
        result_klass.for_test(@__assert_running_test__, msg, caller_locations),
      )
    end

    attr_reader :__assert_config__
  end
end
