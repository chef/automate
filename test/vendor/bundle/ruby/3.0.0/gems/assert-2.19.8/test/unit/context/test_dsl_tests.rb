# frozen_string_literal: true

require "assert"
require "assert/context/test_dsl"

module Assert::Context::TestDSL
  class UnitTests < Assert::Context
    desc "Assert::Context::TestDSL"

    let(:test_desc1){ "be true" }
    let(:test_block1){ Proc.new{ assert(true) } }

    should "build a test using `test` with a desc and code block" do
      d, b = test_desc1, test_block1
      context, test = build_eval_context{ test(d, &b) }

      assert_that(context.class.suite.tests_to_run_count).equals(1)

      assert_that(test).is_kind_of(Assert::Test)
      assert_that(test.name).equals(test_desc1)
      assert_that(test.code).equals(test_block1)
    end

    should "build a test using `should` with a desc and code block" do
      d, b = test_desc1, test_block1
      context, test = build_eval_context{ should(d, &b) }

      assert_that(context.class.suite.tests_to_run_count).equals(1)

      assert_that(test).is_kind_of(Assert::Test)
      assert_that(test.name).equals("should #{test_desc1}")
      assert_that(test.code).equals(test_block1)
    end

    should "build a test that skips with no msg when `test_eventually` "\
           "called" do
      d, b = test_desc1, test_block1
      context, test = build_eval_context{ test_eventually(d, &b) }
      err =
        capture_err(Assert::Result::TestSkipped) do
          context.instance_eval(&test.code)
        end

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that(err.message).equals("TODO")
      assert_that(err.backtrace.size).equals(1)
    end

    should "build a test that skips with no msg  when `should_eventually` "\
           "called" do
      d, b = test_desc1, test_block1
      context, test = build_eval_context{ should_eventually(d, &b) }
      err =
        capture_err(Assert::Result::TestSkipped) do
          context.instance_eval(&test.code)
        end

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that(err.message).equals("TODO")
      assert_that(err.backtrace.size).equals(1)
    end

    should "skip with the msg \"TODO\" when `test` called with no block" do
      d = test_desc1
      context, test = build_eval_context{ test(d) } # no block passed
      err =
        capture_err(Assert::Result::TestSkipped) do
          context.instance_eval(&test.code)
        end

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that(err.message).equals("TODO")
      assert_that(err.backtrace.size).equals(1)
    end

    should "skip with the msg \"TODO\" when `should` called with no block" do
      d = test_desc1
      context, test = build_eval_context{ should(d) } # no block passed
      err =
        capture_err(Assert::Result::TestSkipped) do
          context.instance_eval(&test.code)
        end

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that(err.message).equals("TODO")
      assert_that(err.backtrace.size).equals(1)
    end

    should "skip with the msg \"TODO\" when `test_eventually` called with "\
           "no block" do
      d = test_desc1
      context, test = build_eval_context{ test_eventually(d) } # no block given
      err =
        capture_err(Assert::Result::TestSkipped) do
          context.instance_eval(&test.code)
        end

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that(err.message).equals("TODO")
      assert_that(err.backtrace.size).equals(1)
    end

    should "skip with the msg \"TODO\" when `should_eventually` called with "\
           "no block" do
      d = test_desc1
      context, test = build_eval_context{ should_eventually(d) }
      err =
        capture_err(Assert::Result::TestSkipped) do
          context.instance_eval(&test.code)
        end

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that(err.message).equals("TODO")
      assert_that(err.backtrace.size).equals(1)
    end

    should "build a test from a macro using `test`" do
      d, b = test_desc1, test_block1
      m =
        Assert::Macro.new do
          test(d, &b)
          test(d, &b)
        end
      context_class = Factory.modes_off_context_class{ test(m) }

      assert_that(context_class.suite.tests_to_run_count).equals(2)
    end

    should "build a test from a macro using `should`" do
      d, b = test_desc1, test_block1
      m =
        Assert::Macro.new do
          should(d, &b)
          should(d, &b)
        end
      context_class = Factory.modes_off_context_class{ should(m) }

      assert_that(context_class.suite.tests_to_run_count).equals(2)
    end

    should "build a test that skips from a macro using `test_eventually`" do
      d, b = test_desc1, test_block1
      m =
        Assert::Macro.new do
          test(d, &b)
          test(d, &b)
        end
      context, test = build_eval_context{ test_eventually(m) }

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that{
        context.instance_eval(&test.code)
      }.raises(Assert::Result::TestSkipped)
    end

    should "build a test that skips from a macro using `should_eventually`" do
      d, b = test_desc1, test_block1
      m =
        Assert::Macro.new do
          should(d, &b)
          should(d, &b)
        end
      context, test = build_eval_context{ should_eventually(m) }

      assert_that(context.class.suite.tests_to_run_count).equals(1)
      assert_that{
        context.instance_eval(&test.code)
      }.raises(Assert::Result::TestSkipped)
    end

    private

    def build_eval_context(&build_block)
      context_class = Factory.modes_off_context_class(&build_block)
      test = context_class.suite.sorted_tests_to_run.to_a.last
      [context_class.new(test, test.config, proc{ |r| }), test]
    end

    def capture_err(err_class, &block)
      begin
        block.call
      rescue err_class => ex
        ex
      end
    end
  end
end
