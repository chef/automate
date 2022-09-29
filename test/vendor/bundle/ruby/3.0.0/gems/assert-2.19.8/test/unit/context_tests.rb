# frozen_string_literal: true

require "assert"
require "assert/context"

require "assert/config"
require "assert/result"
require "assert/utils"

class Assert::Context
  class UnitTests < Assert::Context
    desc "Assert::Context"
    subject{ unit_class }

    let(:unit_class){ Assert::Context }

    # DSL methods
    should have_imeths :description, :desc, :describe, :subject, :suite, :let
    should have_imeths :setup_once, :before_once, :startup
    should have_imeths :teardown_once, :after_once, :shutdown
    should have_imeths :setup, :before, :setups, :run_setups
    should have_imeths :teardown, :after, :teardowns, :run_teardowns
    should have_imeths :around, :arounds, :run_arounds
    should have_imeths :test, :test_eventually, :test_skip
    should have_imeths :should, :should_eventually, :should_skip
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ context_class1.new(test1, test1.config, result_callback1) }

    setup do
      @callback_result = nil
    end

    let(:test1){ Factory.test }
    let(:context_class1){ test1.context_class }
    let(:test_results1){ [] }
    let(:result_callback1) do
      proc do |result|
        @callback_result = result
        test_results1 << result
      end
    end
    let(:halt_config1){ Assert::Config.new(halt_on_fail: true) }
    let(:msg1){ Factory.string }

    should have_imeths :assert, :assert_not, :refute, :assert_that
    should have_imeths :pass, :ignore, :fail, :flunk, :skip
    should have_imeths :pending, :with_backtrace, :subject

    should "collect context info" do
      test = @__assert_running_test__
      assert_that(test.context_info.file)
        .matches(%r{test/unit/context_tests.rb$})
      assert_that(test.context_info.klass).equals(self.class)
    end

    private

    ASSERT_TEST_PATH_REGEX = /\A#{File.join(ROOT_PATH, "test", "")}/

    def assert_with_bt_set(exp_with_bt, result)
      with_backtrace(caller) do
        assert_that(result.with_bt_set?).is_true

        exp =
          Assert::Result::Backtrace.to_s(exp_with_bt +
          [(result.backtrace.filtered.first)])
        assert_that(result.trace).equals(exp)
        assert_that(result.src_line).equals(exp_with_bt.first)
      end
    end

    def assert_not_with_bt_set(result)
      with_backtrace(caller) do
        assert_that(result.with_bt_set?).is_false

        assert_that(result.trace).equals(result.src_line)
        assert_that(result.src_line)
          .equals(result.backtrace.filtered.first.to_s)
      end
    end
  end

  class SkipTests < InitTests
    desc "skip method"

    setup do
      @exception =
        begin
          subject.skip(msg1)
        rescue => ex
          ex
        end
      @result = Factory.skip_result(@exception)
    end

    should "raise a test skipped exception and set its message" do
      assert_that(@exception).is_kind_of(Assert::Result::TestSkipped)
      assert_that(@exception.message).equals(msg1)
      assert_that(@result.message).equals(msg1)
    end

    should "not call the result callback" do
      assert_that(@callback_result).is_nil
    end

    should "use any given called from arg as the exception backtrace" do
      assert_that(@exception.backtrace.size).does_not_equal(1)

      called_from = Factory.string
      exception =
        begin
          subject.skip(msg1, called_from)
        rescue => ex
          ex
        end
      assert_that(exception.backtrace.size).equals(1)
      assert_that(exception.backtrace.first).equals(called_from)
    end
  end

  class IgnoreTests < InitTests
    desc "ignore method"

    setup do
      @result = subject.ignore(msg1)
    end

    should "create an ignore result and set its message" do
      assert_that(@result).is_kind_of(Assert::Result::Ignore)
      assert_that(@result.message).equals(msg1)
    end

    should "call the result callback" do
      assert_that(@callback_result).equals(@result)
    end
  end

  class PassTests < InitTests
    desc "pass method"

    setup do
      @result = subject.pass(msg1)
    end

    should "create a pass result and set its message" do
      assert_that(@result).is_kind_of(Assert::Result::Pass)
      assert_that(@result.message).equals(msg1)
    end

    should "call the result callback" do
      assert_that(@callback_result).equals(@result)
    end
  end

  class FlunkTests < InitTests
    desc "flunk method"

    setup do
      @result = subject.flunk(msg1)
    end

    should "create a fail result and set its message" do
      assert_that(@result).is_kind_of(Assert::Result::Fail)
      assert_that(@result.message).equals(msg1)
    end

    should "call the result callback" do
      assert_that(@callback_result).equals(@result)
    end
  end

  class FailTests < InitTests
    desc "fail method"

    setup do
      @result = subject.fail
    end

    should "create a fail result and set its backtrace" do
      assert_that(@result).is_kind_of(Assert::Result::Fail)
      assert_that(@result.trace).equals(@result.backtrace.filtered.first.to_s)
      assert_that(@result.backtrace).is_kind_of(Array)
    end

    should "set any given result message" do
      result = subject.fail(msg1)
      assert_that(result.message).equals(msg1)
    end

    should "call the result callback" do
      assert_that(@callback_result).equals(@result)
    end
  end

  class HaltOnFailTests < InitTests
    desc "failing when halting on fails"
    subject{ context_class1.new(test1, halt_config1, result_callback1) }

    should "raise an exception with the failure's message" do
      exception =
        begin
          subject.fail(msg1)
        rescue => ex
          ex
        end
      assert_that(exception).is_kind_of(Assert::Result::TestFailure)
      assert_that(exception.message).equals(msg1)

      result =
        Assert::Result::Fail.for_test(Factory.test("something"), exception)
      assert_that(result.message).equals(msg1)
    end

    should "not call the result callback" do
      assert_that(@callback_result).is_nil
    end
  end

  class AssertTests < InitTests
    desc "assert method"

    let(:what_failed){ Factory.string }

    should "return a pass result given a `true` assertion" do
      result = subject.assert(true, msg1){ what_failed }
      assert_that(result).is_kind_of(Assert::Result::Pass)
      assert_that(result.message).equals("")
    end

    should "return a fail result given a `false` assertion" do
      result = subject.assert(false, msg1){ what_failed }
      assert_that(result).is_kind_of(Assert::Result::Fail)
    end

    should "pp the assertion value in the fail message by default" do
      exp_def_what =
        "Failed assert: assertion was `#{Assert::U.show(false, test1.config)}`."
      result = subject.assert(false, msg1)

      assert_that(result.message).equals([msg1, exp_def_what].join("\n"))
    end

    should "use a custom fail message if one is given" do
      result = subject.assert(false, msg1){ what_failed }
      assert_that(result.message).equals([msg1, what_failed].join("\n"))
    end

    should "return a pass result given a \"truthy\" assertion" do
      assert_that(subject.assert(34)).is_kind_of(Assert::Result::Pass)
    end

    should "return a fail result gievn a `nil` assertion" do
      assert_that(subject.assert(nil)).is_kind_of(Assert::Result::Fail)
    end
  end

  class AssertNotTests < InitTests
    desc "assert_not method"

    should "return a pass result given a `false` assertion" do
      result = subject.assert_not(false, msg1)
      assert_that(result).is_kind_of(Assert::Result::Pass)
      assert_that(result.message).equals("")
    end

    should "return a fail result given a `true` assertion" do
      result = subject.assert_not(true, msg1)
      assert_that(result).is_kind_of(Assert::Result::Fail)
    end

    should "pp the assertion value in the fail message by default" do
      exp_def_what =
        "Failed assert_not: "\
        "assertion was `#{Assert::U.show(true, test1.config)}`."
      result = subject.assert_not(true, msg1)

      assert_that(result.message).equals([msg1, exp_def_what].join("\n"))
    end

    should "return a fail result given a \"truthy\" assertion" do
      assert_that(subject.assert_not(34)).is_kind_of(Assert::Result::Fail)
    end

    should "return a pass result given a `nil` assertion" do
      assert_that(subject.assert_not(nil)).is_kind_of(Assert::Result::Pass)
    end
  end

  class AssertThatTests < InitTests
    desc "`assert_that` method"

    setup do
      Assert.stub_tap_on_call(Assert::ActualValue, :new) do |_, call|
        @actual_value_new_call = call
      end
    end

    let(:actual_value){ Factory.string }

    should "build an Assert::ActualValue" do
      assert_instance_of Assert::ActualValue, subject.assert_that(actual_value)
      assert_equal [actual_value], @actual_value_new_call.pargs
      assert_equal({ context: subject }, @actual_value_new_call.kargs)
    end
  end

  class SubjectTests < InitTests
    desc "subject method"
    subject{ context_class1.new(test1, test1.config, proc{ |result| }) }

    setup do
      expected = expected1
      context_class1.subject{ @something = expected }
      @subject = subject.subject
    end

    let(:context_class1){ Factory.modes_off_context_class }
    let(:expected1){ Factory.string }

    should "instance evaluate the block set with the class setup method" do
      assert_that(@subject).equals(expected1)
    end
  end

  class PendingTests < InitTests
    desc "`pending` method"

    let(:block2) do
      proc do
        fail # rubocop:disable Style/SignalException
        pass # rubocop:disable Lint/UnreachableCode
      end
    end
    # test nesting
    let(:block1) do
      block = block2
      proc{ pending(&block) }
    end

    should "make fails skips and make passes fails" do
      subject.fail "not affected"
      subject.pass
      subject.pending(&block1)

      assert_that(test_results1.size).equals(4)
      norm_fail, norm_pass, pending_fail, pending_pass = test_results1

      assert_that(norm_fail).is_kind_of(Assert::Result::Fail)
      assert_that(norm_pass).is_kind_of(Assert::Result::Pass)

      assert_that(pending_fail).is_kind_of(Assert::Result::Skip)
      assert_that(pending_fail.message).includes("Pending fail")

      assert_that(pending_pass).is_kind_of(Assert::Result::Fail)
      assert_that(pending_pass.message).includes("Pending pass")
    end
  end

  class PendingWithHaltOnFailTests < PendingTests
    desc "when halting on fails"
    subject{ context_class1.new(test1, halt_config1, result_callback1) }

    should "make fails skips and stop the test" do
      exception =
        begin
          subject.pending(&block1)
        rescue => ex
          ex
        end
      assert_that(exception).is_kind_of(Assert::Result::TestSkipped)
      assert_that(exception.message).includes("Pending fail")

      # it halted before the pending pass
      assert_that(test_results1.size).equals(0)
    end
  end

  class WithBacktraceTests < InitTests
    desc "`with_backtrace` method"

    let(:from_bt1){ ["called_from_here", Factory.string] }
    let(:from_block1) do
      proc do
        ignore
        fail # rubocop:disable Style/SignalException
        pass # rubocop:disable Lint/UnreachableCode
        skip "todo"
      end
    end

    should "alter non-error block results' bt with given bt's first line" do
      subject.fail "not affected"
      begin
        subject.with_backtrace(from_bt1, &from_block1)
      rescue Assert::Result::TestSkipped => ex
        test_results1 << Assert::Result::Skip.for_test(test1, ex)
      end

      assert_that(test_results1.size).equals(5)
      norm_fail, with_ignore, with_fail, with_pass, _with_skip = test_results1

      assert_not_with_bt_set norm_fail

      exp = [from_bt1.first]
      assert_with_bt_set exp, with_ignore
      assert_with_bt_set exp, with_fail
      assert_with_bt_set exp, with_pass
      assert_with_bt_set exp, with_ignore
    end
  end

  class WithNestedBacktraceTests < InitTests
    desc "`with_backtrace` method nested"

    let(:from_bt1){ ["called_from_here 1", Factory.string] }
    let(:from_bt2){ ["called_from_here 2", Factory.string] }
    let(:from_block2) do
      proc do
        ignore
        fail # rubocop:disable Style/SignalException
        pass # rubocop:disable Lint/UnreachableCode
        skip "todo"
      end
    end
    let(:from_block1) do
      from_bt    = from_bt2
      from_block = from_block2
      proc{ with_backtrace(from_bt, &from_block) }
    end

    should "alter non-error block results' bt with nested wbt accrued "\
           "first lines" do
      subject.fail "not affected"
      begin
        subject.with_backtrace(from_bt1, &from_block1)
      rescue Assert::Result::TestSkipped => ex
        test_results1 << Assert::Result::Skip.for_test(test1, ex)
      end

      assert_that(test_results1.size).equals(5)
      norm_fail, with_ignore, with_fail, with_pass, _with_skip = test_results1

      assert_not_with_bt_set norm_fail

      exp = [from_bt1.first, from_bt2.first]
      assert_with_bt_set exp, with_ignore
      assert_with_bt_set exp, with_fail
      assert_with_bt_set exp, with_pass
      assert_with_bt_set exp, with_ignore
    end
  end

  class InspectTests < InitTests
    desc "inspect method"

    should "just show the name of the class" do
      assert_that(subject.inspect).equals("#<#{subject.class}>")
    end
  end
end
