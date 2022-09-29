# frozen_string_literal: true

require "assert/file_line"

module Assert; end

module Assert::Result
  class Base; end
  class Pass   < Base; end
  class Ignore < Base; end
  class Fail   < Base; end
  class Error  < Base; end
  class Skip   < Base; end

  def self.types
    @types ||= Hash.new{ |_h, _k| Base }.tap{ |hash|
      hash[:pass]   = Pass
      hash[:fail]   = Fail
      hash[:ignore] = Ignore
      hash[:skip]   = Skip
      hash[:error]  = Error
    }.freeze
  end

  def self.new(data = nil)
    data ||= {}
    types[data[:type]].new(data)
  end

  class Base
    def self.type
      :unknown
    end

    def self.name
      ""
    end

    def self.for_test(test, message, bt)
      new({
        test_name: test.name,
        test_file_line: test.file_line,
        message: message,
        output: test.output,
        backtrace: Backtrace.new(bt),
      })
    end

    def initialize(build_data)
      @build_data = build_data
      @with_bt    = nil
    end

    def type
      @type ||= (@build_data[:type] || self.class.type).to_sym
    end

    def name
      @name ||= (@build_data[:name] || self.class.name.to_s)
    end

    def test_name
      @test_name ||= (@build_data[:test_name] || "")
    end

    def test_file_line
      @test_file_line ||=
        (@build_data[:test_file_line] || Assert::FileLine.parse(""))
    end

    def test_file_name
      test_file_line.file
    end

    def test_line_num
      test_file_line.line.to_i
    end

    def test_id
      test_file_line.to_s
    end

    def message
      @message ||= (@build_data[:message] || "")
    end

    def output
      @output ||= (@build_data[:output] || "")
    end

    def backtrace
      @backtrace ||= (@build_data[:backtrace] || Backtrace.new([]))
    end

    def trace
      @trace ||= build_trace
    end

    # we choose to implement this way instead of using an `attr_writer` to be
    # consistant with how you override exception backtraces.
    def set_backtrace(bt)
      @backtrace = Backtrace.new(bt)
      @src_line  = nil
      @file_line = nil
      @trace     = nil
    end

    # set the given with bt and the src line for with bt
    def set_with_bt(with_bt)
      return if with_bt.nil?
      @with_bt   = with_bt
      @src_line  = with_bt.first
      @file_line = nil
      @trace     = nil
    end

    def with_bt_set?
      !@with_bt.nil?
    end

    def src_line
      @src_line ||= first_filtered_bt_line(backtrace)
    end

    def file_line
      @file_line ||= Assert::FileLine.parse(src_line)
    end

    def file_name
      file_line.file
    end

    def line_num
      file_line.line.to_i
    end

    Assert::Result.types.keys.each do |type|
      define_method("#{type}?"){ self.type == type }
    end

    def to_sym
      type
    end

    def to_s
      ["#{name.upcase}: #{test_name}", message, trace]
        .reject(&:empty?)
        .join("\n")
    end

    def ==(other)
      if other.is_a?(self.class)
        type == other.type && message == other.message
      else
        super
      end
    end

    def inspect
      "#<#{self.class}:#{"0x0%x" % (object_id << 1)} "\
      "@message=#{message.inspect} "\
      "@file_line=#{file_line.to_s.inspect} "\
      "@test_file_line=#{test_file_line.to_s.inspect}>"
    end

    private

    # By default, a result's trace is its `src_line`.  If a with bt has been
    # set, display it in full along with the "original src line" (the first
    # filtered line of the backtrace).  This is overridden for error results
    # as they always show their full backtrace.
    def build_trace
      if with_bt_set?
        Backtrace.to_s(@with_bt + [first_filtered_bt_line(backtrace)])
      else
        src_line
      end
    end

    # if the filtered backtrace is empty, just use the backtrace itself (this
    # should only occur if the result is an error from a line in Assert's
    # non-test code).
    def first_filtered_bt_line(backtrace)
      ((fbt = backtrace.filtered).empty? ? backtrace : fbt).first.to_s
    end
  end

  class Pass < Base
    def self.type
      :pass
    end

    def self.name
      "Pass"
    end
  end

  class Ignore < Base
    def self.type
      :ignore
    end

    def self.name
      "Ignore"
    end
  end

  class HaltingTestResultError < RuntimeError
    attr_accessor :assert_with_bt
  end

  # raised by the "fail" context helper to break test execution
  TestFailure = Class.new(HaltingTestResultError)

  class Fail < Base
    def self.type
      :fail
    end

    def self.name
      "Fail"
    end

    # fail results can be generated manually or by raising
    # Assert::Result::TestFailure
    def self.for_test(test, msg_or_err, bt = nil)
      if msg_or_err.is_a?(TestFailure)
        super(test, msg_or_err.message, msg_or_err.backtrace).tap do |result|
          result.set_with_bt(msg_or_err.assert_with_bt)
        end
      elsif msg_or_err.is_a?(Exception)
        raise(
          ArgumentError,
          "generate fail results by raising Assert::Result::TestFailure",
        )
      else
        super(test, msg_or_err, bt)
      end
    end
  end

  # raised by the "skip" context helper to break test execution
  TestSkipped = Class.new(HaltingTestResultError)

  class Skip < Base
    def self.type
      :skip
    end

    def self.name
      "Skip"
    end

    # skip results are generated by raising Assert::Result::TestSkipped
    def self.for_test(test, msg_or_err, bt = nil)
      if msg_or_err.is_a?(TestSkipped)
        super(test, msg_or_err.message, msg_or_err.backtrace).tap do |result|
          result.set_with_bt(msg_or_err.assert_with_bt)
        end
      elsif msg_or_err.is_a?(Exception)
        raise(
          ArgumentError,
          "generate skip results by raising Assert::Result::TestSkipped",
        )
      else
        super(test, msg_or_err, bt)
      end
    end
  end

  class Error < Base
    def self.type
      :error
    end

    def self.name
      "Error"
    end

    # error results are generated by raising exceptions in tests
    def self.for_test(test, err)
      if err.is_a?(Exception)
        super(test, "#{err.message} (#{err.class.name})", err.backtrace)
      else
        raise ArgumentError, "generate error results by raising an exception"
      end
    end

    private

    # override of the base, always show the full unfiltered backtrace for errors
    def build_trace
      Backtrace.to_s(backtrace)
    end
  end

  class Backtrace < ::Array
    DELIM = "\n"

    def self.parse(bt)
      new(bt.to_s.split(DELIM))
    end

    def self.to_s(bt_array)
      bt_array.join(DELIM)
    end

    def initialize(value = nil)
      super([*(value || "No backtrace")])
    end

    def filtered
      self.class.new(reject{ |line| filter_out?(line.to_s) })
    end

    protected

    # filter a line out if it's an Assert lib/bin line
    def filter_out?(line)
      # "./lib" in project dir, or "/usr/local/blahblah" if installed
      assert_lib_path    = File.expand_path("../..", __FILE__)
      assert_macros_path = File.join(assert_lib_path, "assert/macros")
      assert_bin_regex = %r{bin/assert\:}
      (line.rindex(assert_lib_path, 0) &&
        !line.rindex(assert_macros_path, 0)
      ) ||
      line =~ assert_bin_regex
    end
  end
end
