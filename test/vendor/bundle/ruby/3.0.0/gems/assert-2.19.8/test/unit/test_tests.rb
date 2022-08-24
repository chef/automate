# frozen_string_literal: true

require "assert"
require "assert/test"

require "assert/config"
require "assert/file_line"
require "assert/result"

class Assert::Test
  class UnitTests < Assert::Context
    desc "Assert::Test"
    subject{ unit_class }

    let(:unit_class){ Assert::Test }

    let(:context_class1) do
      Factory.modes_off_context_class{ desc "context class" }
    end
    let(:context_info1){ Factory.context_info(context_class1) }
    let(:config1){ Factory.modes_off_config }
    let(:test_code1){ proc{ assert(true) } }

    should have_imeths :name_file_line_context_data, :for_block

    should "know how to build the name and file line given context" do
      test_name = Factory.string
      data = subject.name_file_line_context_data(context_info1, test_name)

      exp = context_info1.test_name(test_name)
      assert_that(data[:name]).equals(exp)

      exp = context_info1.called_from
      assert_that(data[:file_line]).equals(exp)
    end

    should "build tests for a block" do
      name = Factory.string
      test = subject.for_block(name, context_info1, config1, &test_code1)

      exp = Assert::FileLine.parse(context_info1.called_from)
      assert_that(test.file_line).equals(exp)

      exp = context_info1.test_name(name)
      assert_that(test.name).equals(exp)

      assert_that(test.context_info).equals(context_info1)
      assert_that(test.config).equals(config1)
      assert_that(test.code).equals(test_code1)
    end
  end

  class InitWithDataTests < UnitTests
    desc "when init with data"
    subject{ unit_class.new(meta_data1.merge(run_data1)) }

    let(:file_line1) do
      Assert::FileLine.new(Factory.string, Factory.integer.to_s)
    end
    let(:meta_data1) do
      {
        file_line: file_line1.to_s,
        name: Factory.string,
        output: Factory.string,
        run_time: Factory.float(1.0),
      }
    end
    let(:run_data1) do
      {
        context_info: context_info1,
        config: config1,
        code: test_code1,
      }
    end

    should have_imeths :file_line, :file_name, :line_num
    should have_imeths :name, :output, :run_time
    should have_imeths :context_info, :context_class, :config, :code, :run

    should "use any given attrs" do
      assert_that(subject.file_line).equals(file_line1)
      assert_that(subject.name).equals(meta_data1[:name])
      assert_that(subject.output).equals(meta_data1[:output])
      assert_that(subject.run_time).equals(meta_data1[:run_time])

      assert_that(subject.context_info).equals(context_info1)
      assert_that(subject.config).equals(config1)
      assert_that(subject.code).equals(test_code1)
    end

    should "default its attrs" do
      test = unit_class.new

      assert_that(test.file_line).equals(Assert::FileLine.parse(""))
      assert_that(test.name).equals("")
      assert_that(test.output).equals("")
      assert_that(test.run_time).equals(0)

      assert_that(test.context_info).is_nil
      assert_that(test.config).is_nil
      assert_that(test.code).is_nil
    end

    should "know its context class" do
      assert_that(subject.context_class).equals(context_class1)
    end

    should "know its file line attrs" do
      assert_that(subject.file_name).equals(subject.file_line.file)
      assert_that(subject.line_num).equals(subject.file_line.line.to_i)
    end

    should "have a custom inspect that only shows limited attributes" do
      attrs =
        [:name, :context_info]
          .map{ |method|
            "@#{method}=#{subject.send(method).inspect}"
          }
          .join(" ")
      exp = "#<#{subject.class}:#{"0x0%x" % (subject.object_id << 1)} #{attrs}>"
      assert_that(subject.inspect).equals(exp)
    end
  end

  class PassFailIgnoreHandlingTests < UnitTests
    include Assert::Test::TestHelpers

    subject do
      Factory.test("pass fail ignore test", context_info1) do
        ignore("something")
        assert(true)
        assert(false)
      end
    end

    setup do
      subject.context_class.setup do
        ignore("something")
        assert(true)
        assert(false)
      end
      subject.context_class.teardown do
        ignore("something")
        assert(true)
        assert(false)
      end
      subject.run(&test_run_callback)
    end

    should "capture results in the test and any setups/teardowns" do
      assert_that(test_run_results.size).equals(9)
      test_run_results.each do |result|
        assert_that(result).is_kind_of(Assert::Result::Base)
      end
    end

    should "capture pass results in the test and any setups/teardowns" do
      assert_that(test_run_results(:pass).size).equals(3)
      test_run_results(:pass).each do |result|
        assert_that(result).is_kind_of(Assert::Result::Pass)
      end
    end

    should "capture fail results in the test and any setups/teardowns" do
      assert_that(test_run_results(:fail).size).equals(3)
      test_run_results(:fail).each do |result|
        assert_that(result).is_kind_of(Assert::Result::Fail)
      end
    end

    should "capture ignore results in the test and any setups/teardowns" do
      assert_that(test_run_results(:ignore).size).equals(3)
      test_run_results(:ignore).each do |result|
        assert_that(result).is_kind_of(Assert::Result::Ignore)
      end
    end
  end

  class FailHandlingTests < UnitTests
    include Assert::Test::TestHelpers

    desc "when in halt-on-fail mode"

    should "capture fail results" do
      test =
        Factory.test("halt-on-fail test", context_info1) do
          raise Assert::Result::TestFailure
        end
      test.run(&test_run_callback)

      assert_failed(test)
    end

    should "capture fails in the context setup" do
      test = Factory.test("setup halt-on-fail test", context_info1){}
      test.context_class.setup{ raise Assert::Result::TestFailure }
      test.run(&test_run_callback)

      assert_failed(test)
    end

    should "capture fails in the context teardown" do
      test = Factory.test("teardown halt-on-fail test", context_info1){}
      test.context_class.teardown{ raise Assert::Result::TestFailure }
      test.run(&test_run_callback)

      assert_failed(test)
    end

    private

    def assert_failed(_test)
      with_backtrace(caller) do
        assert_that(test_run_result_count)
          .equals(1, "too many/few fail results")
        test_run_results.each do |result|
          assert_that(result)
            .is_kind_of(Assert::Result::Fail, "not a fail result")
        end
      end
    end
  end

  class SkipHandlingTests < UnitTests
    include Assert::Test::TestHelpers

    should "capture skip results" do
      test = Factory.test("skip test", context_info1){ skip }
      test.run(&test_run_callback)

      assert_skipped(test)
    end

    should "capture skips in the context setup" do
      test = Factory.test("setup skip test", context_info1){}
      test.context_class.setup{ skip }
      test.run(&test_run_callback)

      assert_skipped(test)
    end

    should "capture skips in the context teardown" do
      test = Factory.test("teardown skip test", context_info1){}
      test.context_class.teardown{ skip }
      test.run(&test_run_callback)

      assert_skipped(test)
    end

    private

    def assert_skipped(_test)
      with_backtrace(caller) do
        assert_that(test_run_result_count)
          .equals(1, "too many/few skip results")
        test_run_results.each do |result|
          assert_that(result)
            .is_kind_of(Assert::Result::Skip, "not a skip result")
        end
      end
    end
  end

  class ErrorHandlingTests < UnitTests
    include Assert::Test::TestHelpers

    should "capture error results" do
      test =
        Factory.test("error test", context_info1) do
          raise StandardError, "WHAT"
        end
      test.run(&test_run_callback)

      assert_errored(test)
    end

    should "capture errors in the context setup" do
      test = Factory.test("setup error test", context_info1){}
      test.context_class.setup{ raise "an error" }
      test.run(&test_run_callback)

      assert_errored(test)
    end

    should "capture errors in the context teardown" do
      test = Factory.test("teardown error test", context_info1){}
      test.context_class.teardown{ raise "an error" }
      test.run(&test_run_callback)

      assert_errored(test)
    end

    private

    def assert_errored(_test)
      with_backtrace(caller) do
        assert_that(test_run_result_count)
          .equals(1, "too many/few error results")
        test_run_results.each do |result|
          assert_that(result)
            .is_kind_of(Assert::Result::Error, "not an error result")
        end
      end
    end
  end

  class SignalExceptionHandlingTests < UnitTests
    should "raise any signal exceptions and not capture as an error" do
      test =
        Factory.test("signal test", context_info1) do
          raise SignalException, "USR1"
        end

      assert_that{ test.run }.raises(SignalException)
    end

    should "raises signal exceptions in the context setup" do
      test = Factory.test("setup signal test", context_info1){}
      test.context_class.setup{ raise SignalException, "INT" }

      assert_that{ test.run }.raises(SignalException)
    end

    should "raises signal exceptions in the context teardown" do
      test = Factory.test("teardown signal test", context_info1){}
      test.context_class.teardown{ raise SignalException, "TERM" }

      assert_that{ test.run }.raises(SignalException)
    end
  end

  class ComparingTests < UnitTests
    desc "<=> another test"
    subject{ Factory.test("mmm") }

    should "return 1 with a test named 'aaa' (greater than it)" do
      assert_that(subject <=> Factory.test("aaa")).equals(1)
    end

    should "return 0 with a test named the same" do
      assert_that(subject <=> Factory.test(subject.name)).equals(0)
    end

    should "return -1 with a test named 'zzz' (less than it)" do
      assert_that(subject <=> Factory.test("zzz")).equals(-1)
    end
  end

  class CaptureOutTests < UnitTests
    desc "when capturing std out"
    subject do
      Factory.test("stdout", capture_config1) do
        puts "std out from the test"
        assert true
      end
    end

    let(:capture_config1){ Assert::Config.new(capture_output: true) }

    should "capture any io from the test" do
      subject.run
      assert_that(subject.output).equals("std out from the test\n")
    end
  end

  class FullCaptureOutTests < CaptureOutTests
    desc "across setup, teardown, and meth calls"
    subject do
      Factory.test("fullstdouttest", capture_config1) do
        puts "std out from the test"
        assert a_method_an_assert_calls
      end
    end

    setup do
      subject.context_class.setup{ puts "std out from the setup" }
      subject.context_class.teardown{ puts "std out from the teardown" }
      subject.context_class.send(:define_method, "a_method_an_assert_calls") do
        puts "std out from a method an assert called"
      end
    end

    should "collect all stdout in the output accessor" do
      subject.run

      exp_out =
        "std out from the setup\n"\
        "std out from the test\n"\
        "std out from a method an assert called\n"\
        "std out from the teardown\n"
      assert_that(subject.output).equals(exp_out)
    end
  end
end
