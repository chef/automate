# frozen_string_literal: true

require "stringio"
require "assert/file_line"
require "assert/result"

module Assert
  class Test
    # Test is some code/method to run in the scope of a Context that may
    # produce results.
    def self.name_file_line_context_data(ci, name)
      { name: ci.test_name(name),
        file_line: ci.called_from,
      }
    end

    def self.for_block(name, context_info, config, &block)
      new(name_file_line_context_data(context_info, name).merge({
        context_info: context_info,
        config: config,
        code: block,
      }))
    end

    def initialize(build_data = nil)
      @build_data      = build_data || {}
      @result_callback = nil
    end

    def file_line
      @file_line ||= FileLine.parse((@build_data[:file_line] || "").to_s)
    end

    def file_name
      file_line.file
    end

    def line_num
      file_line.line.to_i
    end

    def name
      @name ||= (@build_data[:name] || "")
    end

    def output
      @output ||= (@build_data[:output] || +"")
    end

    def run_time
      @run_time ||= (@build_data[:run_time] || 0)
    end

    def context_info
      @context_info ||= @build_data[:context_info]
    end

    def context_class
      context_info.klass
    end

    def config
      @config ||= @build_data[:config]
    end

    def code
      @code ||= @build_data[:code]
    end

    def run(&result_callback)
      @result_callback = result_callback || proc{ |result| } # noop by default
      scope = context_class.new(self, config, @result_callback)
      start_time = Time.now
      capture_output do
        context_class.run_arounds(scope){ run_test(scope) }
      end
      @result_callback = nil
      @run_time = Time.now - start_time
    end

    def <=>(other)
      name <=> other.name
    end

    def inspect
      attributes_string = ([:name, :context_info].map do |attr|
        "@#{attr}=#{send(attr).inspect}"
      end).join(" ")
      "#<#{self.class}:#{"0x0%x" % (object_id << 1)} #{attributes_string}>"
    end

    private

    def run_test(scope)
      begin
        # run any assert style "setup do" setups
        context_class.run_setups(scope)
        # run any test/unit style "def setup" setups
        scope.setup if scope.respond_to?(:setup)
        # run the code block
        scope.instance_eval(&(code || proc{}))
      rescue Result::TestFailure => ex
        capture_result(Result::Fail, ex)
      rescue Result::TestSkipped => ex
        capture_result(Result::Skip, ex)
      rescue SignalException => ex
        raise(ex)
      rescue => ex
        capture_result(Result::Error, ex)
      ensure
        begin
          # run any assert style "teardown do" teardowns
          context_class.run_teardowns(scope)
          # run any test/unit style "def teardown" teardowns
          scope.teardown if scope.respond_to?(:teardown)
        rescue Result::TestFailure => ex
          capture_result(Result::Fail, ex)
        rescue Result::TestSkipped => ex
          capture_result(Result::Skip, ex)
        rescue SignalException => ex
          raise(ex)
        rescue => ex
          capture_result(Result::Error, ex)
        end
      end
    end

    def capture_result(result_klass, err)
      @result_callback.call(result_klass.for_test(self, err))
    end

    def capture_output(&block)
      if config.capture_output == true
        orig_stdout = $stdout.clone
        $stdout = capture_io
        block.call
        $stdout = orig_stdout
      else
        block.call
      end
    end

    def capture_io
      StringIO.new(output, "a+")
    end
  end
end
