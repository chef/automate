# frozen_string_literal: true

require "assert"
require "assert/config_helpers"

require "assert/config"

module Assert::ConfigHelpers
  class UnitTests < Assert::Context
    desc "Assert::ConfigHelpers"
    subject{ unit_class }

    let(:unit_class) do
      Class.new do
        include Assert::ConfigHelpers

        def config
          # use the assert config since it has tests, contexts, etc
          # also use a fresh config that is empty
          @config ||= [Assert.config, Assert::Config.new].sample
        end
      end
    end
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new }

    should have_imeths :runner, :suite, :view
    should have_imeths :runner_seed, :single_test?, :single_test_file_line
    should have_imeths :tests_to_run?, :tests_to_run_count
    should have_imeths :test_count, :result_count, :pass_result_count
    should have_imeths :fail_result_count, :error_result_count
    should have_imeths :skip_result_count, :ignore_result_count
    should have_imeths :all_pass?, :formatted_run_time
    should have_imeths :formatted_test_rate, :formatted_result_rate
    should have_imeths :formatted_suite_run_time
    should have_imeths :formatted_suite_test_rate, :formatted_suite_result_rate
    should have_imeths :show_test_profile_info?, :show_test_verbose_info?
    should have_imeths :ocurring_result_types

    should "know the config's runner, suite and view" do
      assert_that(subject.runner).equals(subject.config.runner)
      assert_that(subject.suite).equals(subject.config.suite)
      assert_that(subject.view).equals(subject.config.view)
    end

    should "know its runner seed" do
      assert_that(subject.runner_seed).equals(subject.config.runner_seed)
    end

    should "know if it is in single test mode" do
      Assert.stub(subject.config, :single_test?){ true }
      assert_that(subject.single_test?).is_true

      Assert.stub(subject.config, :single_test?){ false }
      assert_that(subject.single_test?).is_false
    end

    should "know its single test file line" do
      exp = subject.config.single_test_file_line
      assert_that(subject.single_test_file_line).equals(exp)
    end

    should "know its tests-to-run attrs" do
      exp = subject.config.suite.tests_to_run?
      assert_that(subject.tests_to_run?).equals(exp)

      exp = subject.config.suite.tests_to_run_count
      assert_that(subject.tests_to_run_count).equals(exp)
    end

    should "know its test/result counts" do
      exp = subject.config.suite.test_count
      assert_that(subject.test_count).equals(exp)

      exp = subject.config.suite.result_count
      assert_that(subject.result_count).equals(exp)

      exp = subject.config.suite.pass_result_count
      assert_that(subject.pass_result_count).equals(exp)

      exp = subject.config.suite.fail_result_count
      assert_that(subject.fail_result_count).equals(exp)

      exp = subject.config.suite.error_result_count
      assert_that(subject.error_result_count).equals(exp)

      exp = subject.config.suite.skip_result_count
      assert_that(subject.skip_result_count).equals(exp)

      exp = subject.config.suite.ignore_result_count
      assert_that(subject.ignore_result_count).equals(exp)
    end

    should "know if all tests are passing or not" do
      result_count = Factory.integer
      Assert.stub(subject, :result_count){ result_count }
      Assert.stub(subject, :pass_result_count){ result_count }
      assert_that(subject.all_pass?).is_true

      Assert.stub(subject, :pass_result_count){ Factory.integer }
      assert_that(subject.all_pass?).is_false
    end

    should "know its formatted run time, test rate and result rate" do
      format = "%.6f"

      run_time = Factory.float
      exp = format % run_time
      assert_that(subject.formatted_run_time(run_time, format)).equals(exp)
      assert_that(subject.formatted_run_time(run_time)).equals(exp)

      test_rate = Factory.float
      exp = format % test_rate
      assert_that(subject.formatted_result_rate(test_rate, format)).equals(exp)
      assert_that(subject.formatted_result_rate(test_rate)).equals(exp)

      result_rate = Factory.float
      exp = format % result_rate
      assert_that(subject.formatted_result_rate(result_rate, format))
        .equals(exp)
      assert_that(subject.formatted_result_rate(result_rate)).equals(exp)
    end

    should "know its formatted suite run time, test rate and result rate" do
      format = "%.6f"

      exp = format % subject.config.suite.run_time
      assert_that(subject.formatted_suite_run_time(format)).equals(exp)

      exp = format % subject.config.suite.test_rate
      assert_that(subject.formatted_suite_test_rate(format)).equals(exp)

      exp = format % subject.config.suite.result_rate
      assert_that(subject.formatted_suite_result_rate(format)).equals(exp)
    end

    should "know whether to show test profile info" do
      assert_that(subject.show_test_profile_info?)
        .equals(!!subject.config.profile)
    end

    should "know whether to show verbose info" do
      assert_that(subject.show_test_verbose_info?)
        .equals(!!subject.config.verbose)
    end

    should "know what result types occur in a suite's results" do
      result_types = [:pass, :fail, :ignore, :skip, :error]
      result_count = Factory.integer
      Assert.stub(subject, "#{result_types.sample}_result_count") do
        result_count
      end

      exp =
        result_types.select do |type_sym|
          subject.send("#{type_sym}_result_count") > 0
        end
      assert_that(subject.ocurring_result_types).equals(exp)
    end
  end
end
