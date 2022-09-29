# frozen_string_literal: true

require "assert/config"
require "assert/default_suite"
require "assert/factory"
require "assert/result"
require "assert/test"

module Factory
  extend Assert::Factory

  def self.context_info_called_from
    File.expand_path(
      "#{Factory.path}_tests.rb:#{Factory.integer}",
      Dir.pwd,
    )
  end

  def self.context_info(context_klass = nil)
    Assert::ContextInfo.new(
      context_klass || context_class,
      context_info_called_from,
    )
  end

  # Generate an anonymous `Context` inherited from `Assert::Context` by default.
  # This provides a common interface for all contexts used in testing.

  def self.context_class(inherit_from = nil, &block)
    klass = Class.new(inherit_from || Assert::Context, &block)
    default = const_name = "FactoryAssertContext"

    const_name =
      "#{default}#{rand(Time.now.to_i)}" while Object.const_defined?(const_name)
    Object.const_set(const_name, klass)
    klass
  end

  # Generate a no-op test for use in testing.

  def self.test(*args, &block)
    config, context_info, name = [
      args.last.is_a?(Assert::Config) ? args.pop : modes_off_config,
      args.last.is_a?(Assert::ContextInfo) ? args.pop : self.context_info,
      args.last.is_a?(::String) ? args.pop : "a test",
    ]
    Assert::Test.for_block(name, context_info, config, &block)
  end

  # Generate results for use in testing.

  def self.pass_result(msg = nil)
    Assert::Result::Pass.for_test(
      Factory.test(Factory.string),
      msg || Factory.string,
      [],
    )
  end

  def self.ignore_result(msg = nil)
    Assert::Result::Ignore.for_test(
      Factory.test(Factory.string),
      msg || Factory.string,
      [],
    )
  end

  def self.fail_result(msg = nil)
    Assert::Result::Fail.for_test(
      Factory.test(Factory.string),
      msg || Factory.string,
      [],
    )
  end

  def self.skip_result(exception = nil)
    exception ||= Assert::Result::TestSkipped.new
    Assert::Result::Skip.for_test(Factory.test(Factory.string), exception)
  end

  def self.error_result(exception = nil)
    exception ||= StandardError.new
    Assert::Result::Error.for_test(Factory.test(Factory.string), exception)
  end

  def self.modes_off_config
    Assert::Config.new({
      capture_output: false,
      halt_on_fail: false,
      changed_only: false,
      pp_objects: false,
      debug: false,
    })
  end

  def self.modes_off_suite
    Assert::DefaultSuite.new(modes_off_config)
  end

  def self.modes_off_context_class(*args, &block)
    suite_obj = modes_off_suite
    context_class(*args) do
      suite(suite_obj)
      instance_eval(&block) unless block.nil?
    end
  end

  def self.modes_off_context(&result_block)
    test = Factory.test
    Factory.modes_off_context_class.new(
      test,
      test.config,
      result_block || proc{ |r| },
    )
  end

  def self.backtrace
    assert_lib_path =
      File.join(ROOT_PATH, "lib/#{Factory.string}:#{Factory.integer}")
    (Factory.integer(3).times.map{ Factory.string } + [assert_lib_path]).shuffle
  end
end
