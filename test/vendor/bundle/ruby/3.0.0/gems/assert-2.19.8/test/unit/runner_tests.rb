# frozen_string_literal: true

require "assert"
require "assert/runner"

require "stringio"
require "assert/config_helpers"
require "assert/default_suite"
require "assert/result"
require "assert/view"

class Assert::Runner
  class UnitTests < Assert::Context
    desc "Assert::Runner"
    subject{ unit_class }

    let(:unit_class){ Assert::Runner }

    should "include the config helpers" do
      assert_that(subject).includes(Assert::ConfigHelpers)
    end
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new(config1) }

    setup do
      config1.suite Assert::DefaultSuite.new(config1)
      config1.view  Assert::View.new(config1, StringIO.new(+"", "w+"))
    end

    let(:config1){ Factory.modes_off_config }

    should have_readers :config
    should have_imeths :runner, :run
    should have_imeths :before_load, :after_load
    should have_imeths :on_start, :on_finish, :on_info, :on_interrupt
    should have_imeths :before_test, :after_test, :on_result

    should "know its config" do
      assert_that(subject.config).equals(config1)
    end

    should "override the config helper's runner value with itself" do
      assert_that(subject.runner).equals(subject)
    end
  end

  class RunTests < InitTests
    desc "and run"
    subject{ runner_class1.new(config1) }

    setup do
      @view_output = +""

      suite_class = Class.new(Assert::DefaultSuite){ include CallbackMixin }
      view_class  = Class.new(Assert::View){ include CallbackMixin }

      config1.suite suite_class.new(config1)
      config1.view  view_class.new(config1, StringIO.new(@view_output, "w+"))
      config1.suite.on_test(test1)

      @result_count = subject.run
    end

    let(:runner_class1) do
      Class.new(unit_class){ include CallbackMixin }
    end
    let(:ci1){ Factory.context_info(Factory.modes_off_context_class) }
    # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
    let(:test1){ Factory.test("should pass", ci1){ assert(1 == 1) } }
    # rubocop:enable Lint/BinaryOperatorWithIdenticalOperands

    should "return the fail+error result count as an integer exit code" do
      assert_that(@result_count).equals(0)

      fail_count  = Factory.integer
      error_count = Factory.integer
      Assert.stub(subject, :fail_result_count){ fail_count }
      Assert.stub(subject, :error_result_count){ error_count }
      Assert.stub(test1, :run){} # no-op
      result_count = subject.run

      assert_that(result_count).equals(fail_count + error_count)
    end

    should "run all callbacks on itself, the suite and the view" do
      # itself
      assert_that(subject.on_start_called).is_true
      assert_that(subject.before_test_called).equals([test1])
      assert_that(subject.on_result_called.last)
        .is_instance_of(Assert::Result::Pass)
      assert_that(subject.after_test_called).equals([test1])
      assert_that(subject.on_finish_called).is_true

      # suite
      suite = config1.suite
      assert_that(suite.on_start_called).is_true
      assert_that(suite.before_test_called).equals([test1])
      assert_that(suite.on_result_called.last)
        .is_instance_of(Assert::Result::Pass)
      assert_that(suite.after_test_called).equals([test1])
      assert_that(suite.on_finish_called).is_true

      # view
      view = config1.view
      assert_that(view.on_start_called).is_true
      assert_that(view.before_test_called).equals([test1])
      assert_that(view.on_result_called.last)
        .is_instance_of(Assert::Result::Pass)
      assert_that(view.after_test_called).equals([test1])
      assert_that(view.on_finish_called).is_true
    end

    should "describe running the tests in random order if there are tests" do
      exp = "Running tests in random order, " \
            "seeded with \"#{subject.runner_seed}\"\n"
      assert_that(@view_output).includes(exp)

      @view_output.gsub!(/./, "")
      config1.suite.clear_tests_to_run
      subject.run
      assert_that(@view_output).does_not_include(exp)
    end

    should "run only a single test if a single test is configured" do
      # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
      test = Factory.test("should pass", ci1){ assert(1 == 1) }
      # rubocop:enable Lint/BinaryOperatorWithIdenticalOperands
      config1.suite.clear_tests_to_run
      config1.suite.on_test(test)
      config1.single_test test.file_line.to_s

      runner = runner_class1.new(config1).tap(&:run)
      assert_that(runner.before_test_called).equals([test])
    end

    should "not run any tests if a single test is configured but "\
           "can't be found" do
      # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
      test = Factory.test("should pass", ci1){ assert(1 == 1) }
      # rubocop:enable Lint/BinaryOperatorWithIdenticalOperands
      config1.suite.clear_tests_to_run
      config1.suite.on_test(test)
      config1.single_test Factory.string

      runner = runner_class1.new(config1).tap(&:run)
      assert_that(runner.before_test_called).is_nil
    end

    should "describe running only a single test if a single test is "\
           "configured" do
      config1.suite.clear_tests_to_run
      config1.suite.on_test(test1)
      config1.single_test test1.file_line.to_s
      @view_output.gsub!(/./, "")
      subject.run

      exp = "Running test: #{subject.single_test_file_line}, " \
            "seeded with \"#{subject.runner_seed}\"\n"
      assert_that(@view_output).includes(exp)
    end
  end

  module CallbackMixin
    attr_reader :on_start_called, :on_finish_called
    attr_reader :before_test_called, :after_test_called, :on_result_called

    def on_start
      @on_start_called = true
    end

    def before_test(test)
      @before_test_called ||= []
      @before_test_called << test
    end

    def on_result(result)
      @on_result_called ||= []
      @on_result_called << result
    end

    def after_test(test)
      @after_test_called ||= []
      @after_test_called << test
    end

    def on_finish
      @on_finish_called = true
    end
  end
end
