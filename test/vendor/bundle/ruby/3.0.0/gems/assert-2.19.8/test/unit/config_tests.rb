# frozen_string_literal: true

require "assert"
require "assert/config"

require "assert/default_runner"
require "assert/default_suite"
require "assert/default_view"
require "assert/file_line"
require "assert/runner"

class Assert::Config
  class UnitTests < Assert::Context
    desc "Assert::Config"
    subject{ unit_class }

    let(:unit_class){ Assert::Config }
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new }

    should have_imeths :view, :suite, :runner
    should have_imeths :test_dir, :test_helper, :test_file_suffixes
    should have_imeths :changed_proc, :pp_proc, :use_diff_proc, :run_diff_proc
    should have_imeths :runner_seed, :changed_only, :changed_ref, :single_test
    should have_imeths :pp_objects, :capture_output, :halt_on_fail, :profile
    should have_imeths :verbose, :list, :debug
    should have_imeths :apply, :single_test?
    should have_imeths :single_test_file_line, :single_test_file_path
    should have_imeths :env_runner_seed, :random_runner_seed

    should "default the view, suite, and runner" do
      assert_that(subject.view).is_kind_of(Assert::DefaultView)
      assert_that(subject.suite).is_kind_of(Assert::DefaultSuite)
      assert_that(subject.runner).is_kind_of(Assert::DefaultRunner)
    end

    should "default the test dir/helper/suffixes" do
      assert_that(subject.test_dir).equals("test")
      assert_that(subject.test_helper).equals("helper.rb")
      assert_that(subject.test_file_suffixes).equals(["_tests.rb", "_test.rb"])
    end

    should "default the procs" do
      assert_that(subject.changed_proc).is_not_nil
      assert_that(subject.pp_proc).is_not_nil
      assert_that(subject.use_diff_proc).is_not_nil
      assert_that(subject.run_diff_proc).is_not_nil
    end

    should "default the option settings" do
      assert_that(subject.runner_seed).is_not_nil
      assert_that(subject.changed_only).is_false
      assert_that(subject.changed_ref).is_empty
      assert_that(subject.single_test).is_empty
      assert_that(subject.pp_objects).is_false
      assert_that(subject.capture_output).is_false
      assert_that(subject.halt_on_fail).is_true
      assert_that(subject.profile).is_false
      assert_that(subject.verbose).is_false
      assert_that(subject.list).is_false
      assert_that(subject.debug).is_false
    end

    should "prefer a SEED env var, if present, for the runner seed value" do
      orig_env_seed = ENV["SEED"]
      new_env_seed = Factory.integer
      ENV["SEED"] = new_env_seed.to_s

      config = unit_class.new
      assert_that(config.env_runner_seed).equals(new_env_seed.to_s)
      assert_that(config.runner_seed).equals(config.env_runner_seed.to_i)

      ENV["SEED"] = orig_env_seed
    end

    should "fallback to a random runner seed value if no SEED env var" do
      orig_env_seed = ENV["SEED"]
      ENV["SEED"] = nil

      config = unit_class.new
      assert_that(config.random_runner_seed).is_not_nil
      assert_that(config.runner_seed).equals(config.random_runner_seed.to_i)

      ENV["SEED"] = orig_env_seed
    end

    should "apply settings given from a hash" do
      assert subject.halt_on_fail
      subject.apply(halt_on_fail: false)
      assert_that(subject.halt_on_fail).is_false

      assert_that(Assert::Config.new.halt_on_fail).is_true
      assert_that(Assert::Config.new(halt_on_fail: false).halt_on_fail).is_false
    end

    should "know if it is in single test mode" do
      assert_that(subject.single_test?).is_false

      subject.apply(single_test: Factory.string)
      assert_that(subject.single_test?).is_true
    end

    should "know its single test file line" do
      exp = Assert::FileLine.parse(File.expand_path("", Dir.pwd))
      assert_that(subject.single_test_file_line).equals(exp)

      file_line_path = "#{Factory.path}_tests.rb:#{Factory.integer}"
      subject.apply(single_test: file_line_path)

      exp = Assert::FileLine.parse(File.expand_path(file_line_path, Dir.pwd))
      assert_that(subject.single_test_file_line).equals(exp)
    end

    should "know its single test file path" do
      exp = Assert::FileLine.parse(File.expand_path("", Dir.pwd)).file
      assert_that(subject.single_test_file_path).equals(exp)

      path = "#{Factory.path}_tests.rb"
      file_line_path = "#{path}:#{Factory.integer}"
      subject.apply(single_test: file_line_path)
      assert_that(subject.single_test_file_path)
        .equals(File.expand_path(path, Dir.pwd))
    end
  end
end
