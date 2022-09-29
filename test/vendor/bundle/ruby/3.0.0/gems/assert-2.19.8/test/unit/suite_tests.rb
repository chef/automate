# frozen_string_literal: true

require "assert"
require "assert/suite"

require "assert/config_helpers"
require "assert/test"
require "test/support/inherited_stuff"

class Assert::Suite
  class UnitTests < Assert::Context
    desc "Assert::Suite"
    subject{ unit_class }

    let(:unit_class){ Assert::Suite }

    should "include the config helpers" do
      assert_that(subject).includes(Assert::ConfigHelpers)
    end
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new(config1) }

    let(:config1){ Factory.modes_off_config }

    should have_readers :config, :setups, :teardowns
    should have_accessors :start_time, :end_time
    should have_imeths :suite, :setup, :startup, :teardown, :shutdown
    should have_imeths :tests_to_run?, :tests_to_run_count, :clear_tests_to_run
    should have_imeths :find_test_to_run, :sorted_tests_to_run
    should have_imeths :run_time, :test_rate, :result_rate
    should have_imeths :test_count, :result_count, :pass_result_count
    should have_imeths :fail_result_count, :error_result_count
    should have_imeths :skip_result_count, :ignore_result_count
    should have_imeths :before_load, :on_test, :after_load
    should have_imeths :on_start, :on_finish, :on_info, :on_interrupt
    should have_imeths :before_test, :after_test, :on_result

    should "know its config" do
      assert_that(subject.config).equals(config1)
    end

    should "default its attrs" do
      assert_that(subject.setups).equals([])
      assert_that(subject.teardowns).equals([])

      assert_that(subject.end_time).equals(subject.start_time)
    end

    should "override the config helper's suite value with itself" do
      assert_that(subject.suite).equals(subject)
    end

    should "not provide any test/result count implementations" do
      assert_that(subject.test_count).is_nil
      assert_that(subject.pass_result_count).is_nil
      assert_that(subject.fail_result_count).is_nil
      assert_that(subject.error_result_count).is_nil
      assert_that(subject.skip_result_count).is_nil
      assert_that(subject.ignore_result_count).is_nil
    end

    should "know its run time and rates" do
      assert_that(subject.run_time).equals(0)
      assert_that(subject.test_rate).equals(0)
      assert_that(subject.result_rate).equals(0)

      time = Factory.integer(3).to_f
      subject.end_time = subject.start_time + time
      count = Factory.integer(10)
      Assert.stub(subject, :test_count){ count }
      Assert.stub(subject, :result_count){ count }

      assert_that(subject.run_time).equals(time)
      assert_that(subject.test_rate)
        .equals((subject.test_count / subject.run_time))
      assert_that(subject.result_rate)
        .equals((subject.result_count / subject.run_time))
    end

    should "add setup procs" do
      status = nil
      subject.setup{ status = "setups" }
      subject.startup{ status += " have been run" }

      assert_that(subject.setups.count).equals(2)
      subject.setups.each(&:call)
      assert_that(status).equals("setups have been run")
    end

    should "add teardown procs" do
      status = nil
      subject.teardown{ status = "teardowns" }
      subject.shutdown{ status += " have been run" }

      assert_that(subject.teardowns.count).equals(2)
      subject.teardowns.each(&:call)
      assert_that(status).equals("teardowns have been run")
    end
  end

  class WithTestsLoadedTests < InitTests
    desc "with tests loaded"

    setup do
      tests1.each{ |test| subject.on_test(test) }
    end

    let(:ci1){ proc{ Factory.context_info(Factory.modes_off_context_class) } }
    let(:tests1) do
      # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
      [
        Factory.test("should nothing", ci1.call) do
        end,
        Factory.test("should pass", ci1.call) do
          assert(1 == 1)
          refute(1 == 0)
        end,
        Factory.test("should fail", ci1.call) do
          ignore
          assert(1 == 0)
          refute(1 == 1)
        end,
        Factory.test("should ignore", ci1.call) do
          ignore
        end,
        Factory.test("should skip", ci1.call) do
          skip
          ignore
          assert(1 == 1)
        end,
        Factory.test("should error", ci1.call) do
          raise Exception
          ignore # rubocop:disable Lint/UnreachableCode
          assert(1 == 1)
        end,
      ]
      # rubocop:enable Lint/BinaryOperatorWithIdenticalOperands
    end

    should "know its tests-to-run attrs" do
      assert_that(subject.tests_to_run_count).equals(tests1.size)
      assert_that(subject.tests_to_run?).is_true

      subject.clear_tests_to_run

      assert_that(subject.tests_to_run_count).equals(0)
      assert_that(subject.tests_to_run?).is_false
    end

    should "find a test to run given a file line" do
      test = tests1.sample
      assert_that(subject.find_test_to_run(test.file_line)).is(test)
    end

    should "know its sorted tests to run" do
      sorted_tests = subject.sorted_tests_to_run{ 1 }
      assert_that(sorted_tests.size).equals(tests1.size)
      assert_that(sorted_tests.first).is_kind_of(Assert::Test)
      assert_that(subject.sorted_tests_to_run{ 1 }.first).is(sorted_tests.first)
    end
  end
end
