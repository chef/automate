# frozen_string_literal: true

require "assert"
require "assert/result"

require "assert/file_line"

module Assert::Result
  class UnitTests < Assert::Context
    desc "Assert::Result"
    subject{ unit_class }

    let(:unit_class){ Assert::Result }

    let(:test1){ Factory.test("a test name") }

    should have_imeths :types, :new

    should "know its types" do
      exp =
        {
          pass: Pass,
          fail: Fail,
          ignore: Ignore,
          skip: Skip,
          error: Error,
        }
      assert_that(subject.types).equals(exp)

      assert_that(subject.types[Factory.string]).equals(Base)
    end

    should "create results from data hashes" do
      type = Assert::Result.types.keys.sample
      exp  = Assert::Result.types[type].new(type: type)
      assert_that(Assert::Result.new(type: type)).equals(exp)
    end
  end

  class InitBaseTests < UnitTests
    desc "Base when init"
    subject{ Base.new(given_data1) }

    let(:given_data1) do
      {
        type: Factory.string,
        name: Factory.string,
        test_name: Factory.string,
        test_file_line: Assert::FileLine.new(Factory.string, Factory.integer),
        message: Factory.string,
        output: Factory.text,
        backtrace: Backtrace.new(Factory.backtrace),
      }
    end

    should have_cmeths :type, :name, :for_test
    should have_imeths :type, :name, :test_name, :test_file_line
    should have_imeths :test_file_name, :test_line_num, :test_id
    should have_imeths :message, :output
    should have_imeths :backtrace, :trace
    should have_imeths :set_backtrace, :set_with_bt, :with_bt_set?
    should have_imeths :src_line, :file_line, :file_name, :line_num
    should have_imeths(*Assert::Result.types.keys.map{ |k| "#{k}?" })
    should have_imeths :to_sym, :to_s

    should "know its class-level type/name" do
      assert_that(subject.class.type).equals(:unknown)
      assert_that(subject.class.name).equals("")
    end

    should "know how to build a result for a given test" do
      message = Factory.text
      bt      = Factory.integer(3).times.map{ Factory.string }
      result  = Base.for_test(test1, message, bt)

      exp_backtrace = Backtrace.new(bt)
      exp_trace     = exp_backtrace.filtered.first.to_s

      assert_that(result.test_name).equals(test1.name)
      assert_that(result.test_id).equals(test1.file_line.to_s)

      assert_that(result.message).equals(message)
      assert_that(result.backtrace).equals(exp_backtrace)
      assert_that(result.trace).equals(exp_trace)

      assert_that(result.with_bt_set?).is_false
    end

    should "use any given attrs" do
      assert_that(subject.type).equals(given_data1[:type].to_sym)
      assert_that(subject.name).equals(given_data1[:name])
      assert_that(subject.test_name).equals(given_data1[:test_name])
      assert_that(subject.test_file_line).equals(given_data1[:test_file_line])
      assert_that(subject.message).equals(given_data1[:message])
      assert_that(subject.output).equals(given_data1[:output])
      assert_that(subject.backtrace).equals(given_data1[:backtrace])
    end

    should "default its attrs" do
      result = Base.new({})

      assert_that(result.type).equals(:unknown)
      assert_that(result.name).equals("")
      assert_that(result.test_name).equals("")
      assert_that(result.test_file_line).equals(Assert::FileLine.parse(""))
      assert_that(result.message).equals("")
      assert_that(result.output).equals("")
      assert_that(result.backtrace).equals(Backtrace.new([]))
      assert_that(result.trace).equals("")
    end

    should "know its test file line attrs" do
      exp = given_data1[:test_file_line]
      assert_that(subject.test_file_name).equals(exp.file)
      assert_that(subject.test_line_num).equals(exp.line.to_i)
      assert_that(subject.test_id).equals(exp.to_s)
    end

    should "allow setting a new backtrace" do
      new_bt        = Factory.backtrace
      exp_backtrace = Backtrace.new(new_bt)
      exp_trace     = exp_backtrace.filtered.first.to_s
      subject.set_backtrace(new_bt)
      assert_that(subject.backtrace).equals(exp_backtrace)
      assert_that(subject.trace).equals(exp_trace)

      # test that the first bt line is used if filtered is empty
      assert_lib_path =
        File.join(ROOT_PATH, "lib/#{Factory.string}:#{Factory.integer}")
      new_bt        = (Factory.integer(3) + 1).times.map{ assert_lib_path }
      exp_backtrace = Backtrace.new(new_bt)
      exp_trace     = exp_backtrace.first.to_s
      subject.set_backtrace(new_bt)
      assert_that(subject.backtrace).equals(exp_backtrace)
      assert_that(subject.trace).equals(exp_trace)
    end

    should "allow setting a with bt backtrace and know if one has been set" do
      assert_that(subject.with_bt_set?).is_false

      orig_backtrace = subject.backtrace
      with_bt        = Factory.backtrace

      subject.set_with_bt(with_bt)

      assert_that(subject.with_bt_set?).is_true
      assert_that(subject.backtrace).equals(orig_backtrace)
      assert_that(subject.src_line).equals(with_bt.first)

      exp = Backtrace.to_s(with_bt + [orig_backtrace.filtered.first])
      assert_that(subject.trace).equals(exp)
    end

    should "know its src/file line attrs" do
      new_bt = Factory.backtrace
      subject.set_backtrace(new_bt)

      exp = Backtrace.new(new_bt).filtered.first.to_s
      assert_that(subject.src_line).equals(exp)

      exp = Assert::FileLine.parse(subject.src_line)
      assert_that(subject.file_line).equals(exp)
      assert_that(subject.file_name).equals(exp.file)
      assert_that(subject.line_num).equals(exp.line.to_i)

      # test you get the same file line attrs using `set_with_bt`
      subject.set_with_bt(new_bt)
      assert_that(subject.src_line).equals(new_bt.first.to_s)

      exp = Assert::FileLine.parse(subject.src_line)
      assert_that(subject.file_line).equals(exp)
      assert_that(subject.file_name).equals(exp.file)
      assert_that(subject.line_num).equals(exp.line.to_i)

      # test that the first bt line is used if filtered is empty
      assert_lib_path =
        File.join(ROOT_PATH, "lib/#{Factory.string}:#{Factory.integer}")
      new_bt = (Factory.integer(3) + 1).times.map{ assert_lib_path }
      subject.set_backtrace(new_bt)

      exp = new_bt.first.to_s
      assert_that(subject.src_line).equals(exp)

      exp = Assert::FileLine.parse(subject.src_line)
      assert_that(subject.file_line).equals(exp)
      assert_that(subject.file_name).equals(exp.file)
      assert_that(subject.line_num).equals(exp.line.to_i)
    end

    should "know if it is a certain type of result" do
      Assert::Result.types.keys.each do |type|
        assert_that(subject.send("#{type}?")).is_false
        Assert.stub(subject, :type){ type }
        assert_that(subject.send("#{type}?")).is_true
      end
    end

    should "know its symbol representation" do
      assert_that(subject.to_sym).equals(subject.type)
    end

    should "know its string representation" do
      str = subject.to_s

      assert_that(str).includes(subject.name.upcase)
      assert_that(str).includes(subject.test_name)
      assert_that(str).includes(subject.message)
      assert_that(str).includes(subject.trace)

      assert_that(str.split("\n").count).equals(3)

      Assert.stub(subject, :message){ "" }
      Assert.stub(subject, :trace){ "" }

      assert_that(subject.to_s.split("\n").count).equals(1)
    end

    should "know if it is equal to another result" do
      other = Assert::Result::Base.new(given_data1)
      assert_that(subject).equals(other)

      Assert.stub(other, [:type, :message].sample){ Factory.string }
      assert_not_equal other, subject
    end

    should "show only its class and message when inspected" do
      exp = "#<#{subject.class}:#{"0x0%x" % (subject.object_id << 1)} "\
            "@message=#{subject.message.inspect} "\
            "@file_line=#{subject.file_line.to_s.inspect} "\
            "@test_file_line=#{subject.test_file_line.to_s.inspect}>"
      assert_that(subject.inspect).equals(exp)
    end
  end

  class InitPassTests < UnitTests
    desc "Pass when init"
    subject{ Pass.new({}) }

    should "know its type/name" do
      assert_that(subject.type).equals(:pass)
      assert_that(subject.class.type).equals(:pass)
      assert_that(subject.class.name).equals("Pass")
    end
  end

  class InitIgnoreTests < UnitTests
    desc "Ignore when init"
    subject{ Ignore.new({}) }

    should "know its type/name" do
      assert_that(subject.type).equals(:ignore)
      assert_that(subject.class.type).equals(:ignore)
      assert_that(subject.class.name).equals("Ignore")
    end
  end

  class InitHaltingTestResultErrorTests < UnitTests
    desc "HaltingTestResultError when init"
    subject{ HaltingTestResultError.new }

    should have_accessors :assert_with_bt

    should "be a runtime error" do
      assert_that(subject).is_kind_of(RuntimeError)
    end
  end

  class InitTestFailureTests < UnitTests
    desc "TestFailure when init"
    subject{ TestFailure.new }

    should "be a halting test result error" do
      assert_that(subject).is_kind_of(HaltingTestResultError)
    end
  end

  class InitFailTests < UnitTests
    desc "Fail when init"
    subject{ Fail.new({}) }

    should "know its type/name" do
      assert_that(subject.type).equals(:fail)
      assert_that(subject.class.type).equals(:fail)
      assert_that(subject.class.name).equals("Fail")
    end

    should "allow creating for a test with TestFailure exceptions" do
      err = TestFailure.new
      err.set_backtrace(Factory.backtrace)
      result = Fail.for_test(test1, err)

      assert_that(result.message).equals(err.message)

      err_backtrace = Backtrace.new(err.backtrace)
      assert_that(result.backtrace).equals(err_backtrace)

      # test assert with bt errors
      err.assert_with_bt = Factory.backtrace
      result = Fail.for_test(test1, err)

      assert_that(result.message).equals(err.message)
      assert_that(result.backtrace).equals(err.backtrace)
      assert_that(result.src_line).equals(err.assert_with_bt.first)

      exp = Backtrace.to_s(err.assert_with_bt + [err_backtrace.filtered.first])
      assert_that(result.trace).equals(exp)
    end

    should "not allow creating for a test with non-TestFailure exceptions" do
      assert_that{ Fail.for_test(test1, RuntimeError.new) }
        .raises(ArgumentError)
    end
  end

  class InitTestSkippedTests < UnitTests
    desc "TestSkipped when init"
    subject{ TestSkipped.new }

    should "be a halting test result error" do
      assert_that(subject).is_kind_of(HaltingTestResultError)
    end
  end

  class InitSkipTests < UnitTests
    desc "Skip when init"
    subject{ Skip.new({}) }

    should "know its type/name" do
      assert_that(subject.type).equals(:skip)
      assert_that(subject.class.type).equals(:skip)
      assert_that(subject.class.name).equals("Skip")
    end

    should "allow creating for a test with TestSkipped exceptions" do
      err = TestSkipped.new
      err.set_backtrace(Factory.backtrace)
      result = Skip.for_test(test1, err)

      assert_that(result.message).equals(err.message)

      err_backtrace = Backtrace.new(err.backtrace)
      assert_that(result.backtrace).equals(err_backtrace)

      # test assert with bt errors
      err.assert_with_bt = Factory.backtrace
      result = Skip.for_test(test1, err)

      assert_that(result.message).equals(err.message)
      assert_that(result.backtrace).equals(err.backtrace)
      assert_that(result.src_line).equals(err.assert_with_bt.first)

      exp = Backtrace.to_s(err.assert_with_bt + [err_backtrace.filtered.first])
      assert_that(result.trace).equals(exp)
    end

    should "not allow creating for a test with non-TestSkipped exceptions" do
      assert_that{ Skip.for_test(test1, RuntimeError.new) }
        .raises(ArgumentError)
    end
  end

  class InitErrorTests < UnitTests
    desc "Error when init"
    subject{ Error.new({}) }

    should "know its class-level type/name" do
      assert_that(subject.class.type).equals(:error)
      assert_that(subject.class.name).equals("Error")
    end

    should "allow creating for a test with exceptions" do
      err = Exception.new
      err.set_backtrace(Factory.backtrace)
      result = Error.for_test(test1, err)

      exp_msg = "#{err.message} (#{err.class.name})"
      assert_that(result.message).equals(exp_msg)

      exp_bt = Backtrace.new(err.backtrace)
      assert_that(result.backtrace).equals(exp_bt)
      assert_that(result.trace).equals(Backtrace.to_s(exp_bt))
    end

    should "not allow creating for a test without an exception" do
      assert_that{ Error.for_test(test1, Factory.string) }.raises(ArgumentError)
    end
  end

  class InitBacktraceTests < UnitTests
    desc "Backtrace when init"
    subject{ Backtrace.new(Factory.backtrace) }

    should have_cmeths :parse, :to_s
    should have_imeths :filtered

    should "be parseable from its string representation" do
      assert_that(Backtrace.parse(Backtrace.to_s(subject))).equals(subject)
    end

    should "render as a string by joining on the newline" do
      assert_that(Backtrace.to_s(subject))
        .equals(subject.join(Backtrace::DELIM))
    end

    should "be an Array" do
      assert_that(subject).is_kind_of(::Array)
    end

    should "know its DELIM" do
      assert_that(Backtrace::DELIM).equals("\n")
    end

    should "another backtrace when filtered" do
      assert_that(subject).is_kind_of(Backtrace)
    end

    should "default itself when created from nil" do
      assert_that(Backtrace.new).equals(["No backtrace"])
    end
  end
end
