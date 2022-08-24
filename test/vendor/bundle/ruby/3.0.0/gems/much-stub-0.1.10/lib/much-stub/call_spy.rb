# frozen_string_literal: true

require "much-stub/call"

module MuchStub
  class CallSpy < ::BasicObject
    METHOD_NAME_REPLACEMENTS = {
      "!" => "_bang",
      "?" => "_predicate",
    }.freeze

    def initialize(**return_values)
      @call_spy_return_values = return_values.transform_keys(&:to_s)
      @call_spy_method_calls = ::Hash.new{ |hash, key| hash[key] = [] }
      @call_spy_method_return_values =
        ::Hash.new{ |hash, key| hash[key] = call_spy_return_value_proc(key) }
    end

    def call_spy_tap
      yield self
      self
    end

    def ==(other)
      equal?(other)
    end

    def ===(other)
      equal?(other)
    end

    def eql?(other)
      equal?(other)
    end

    def equal?(other)
      __id__ == other.__id__
    end

    def hash
      __id__
    end

    def respond_to?(*)
      true
    end

    def inspect
      "#<MuchStub::CallSpy:#{"0x0%x" % (__id__ << 1)}>"
    end

    private

    def call_spy_method_return_value(method_name, much_stub_call)
      @call_spy_method_return_values[method_name.to_s].call(much_stub_call)
    end

    def call_spy_return_value_proc(method_name)
      value = @call_spy_return_values.fetch(method_name, ::Proc.new{ self })
      value.respond_to?(:call) ? value : ::Proc.new{ value }
    end

    def call_spy_normalize_method_name(name)
      METHOD_NAME_REPLACEMENTS.reduce(name.to_s) do |acc, (source, replacement)|
        acc.gsub(source, replacement)
      end
    end

    def call_spy_define_spied_method(name)
      method_name = call_spy_normalize_method_name(name)
      call_spy_define_metaclass_method(name) do |*args, &block|
        call = ::MuchStub::Call.new(*args, &block)
        @call_spy_method_calls[method_name] << call
        call_spy_method_return_value(name, call)
      end
    end

    def call_spy_define_query_method(query_method_match)
      spied_method_name = query_method_match[1]
      query_method_suffix = query_method_match[2]
      method_name = call_spy_normalize_method_name(spied_method_name)
      call_spy_define_metaclass_method(
        "#{method_name}#{query_method_suffix}",
      ) do
        yield(method_name) if ::Kernel.block_given?
      end
    end

    def call_spy_define_metaclass_method(name, &block)
      metaclass = class << self; self; end
      metaclass.define_method(name, &block)
    end

    def respond_to_missing?(_name, *_args)
      false
    end

    def method_missing(name, *args, &block)
      if (match = name.match(/(\w+)(_calls)\z/))
        call_spy_define_query_method(match) do |method_name|
          @call_spy_method_calls[method_name]
        end
      elsif (match = name.match(/(\w+)(_last_called_with)\z/))
        call_spy_define_query_method(match) do |method_name|
          __send__("#{method_name}_calls").last
        end
      elsif (match = name.match(/(\w+)(_called_with)\z/))
        call_spy_define_query_method(match) do |method_name|
          __send__("#{method_name}_last_called_with")
        end
      elsif (match = name.match(/(\w+)(_call_count)\z/))
        call_spy_define_query_method(match) do |method_name|
          __send__("#{method_name}_calls").size
        end
      elsif (match = name.match(/(\w+)(_called\?)\z/))
        call_spy_define_query_method(match) do |method_name|
          __send__("#{method_name}_call_count") > 0
        end
      else
        call_spy_define_spied_method(name)
      end
      __send__(name, *args, &block)
    end
  end
end
