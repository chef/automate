# frozen_string_literal: true

require "much-stub/version"

require "much-stub/call"
require "much-stub/call_spy"

module MuchStub
  def self.stubs
    @stubs ||= {}
  end

  def self.stub_key(obj, meth)
    MuchStub::Stub.key(obj, meth)
  end

  def self.arity_matches?(method, args)
    # mandatory args
    return true if method.arity == args.size
    # variable args
    return true if method.arity < 0 && args.size >= (method.arity + 1).abs

    false
  end

  def self.stub(obj, meth, &block)
    key = stub_key(obj, meth)
    stubs[key] ||= MuchStub::Stub.new(obj, meth, caller_locations)
    stubs[key].tap{ |s| s.do = block }
  end

  def self.call(*args, &block)
    stub(*args, &block)
  end

  def self.stub_on_call(*args, &on_call_block)
    stub(*args).on_call(&on_call_block)
  end

  def self.on_call(*args, &on_call_block)
    stub_on_call(*args, &on_call_block)
  end

  def self.unstub(obj, meth)
    key = stub_key(obj, meth)
    (stubs.delete(key) || MuchStub::NullStub.new).teardown
  end

  def self.unstub!
    stubs.keys.each{ |key| stubs.delete(key).teardown }
  end

  def self.stub_send(obj, meth, *pargs, **kargs, &block)
    orig_caller = caller_locations
    stub =
      stubs.fetch(MuchStub::Stub.key(obj, meth)) do
        raise NotStubbedError, "`#{meth}` not stubbed.", orig_caller.map(&:to_s)
      end
    stub.call_method(*pargs, **kargs, &block)
  end

  def self.tap(obj, meth, &tap_block)
    stub(obj, meth) do |*pargs, **kargs, &block|
      stub_send(obj, meth, *pargs, **kargs, &block).tap do |value|
        tap_block&.call(value, *pargs, **kargs, &block)
      end
    end
  end

  def self.tap_on_call(obj, meth, &on_call_block)
    tap(obj, meth) do |value, *pargs, **kargs, &block|
      on_call_block&.call(value, MuchStub::Call.new(*pargs, **kargs, &block))
    end
  end

  def self.spy(obj, *meths, **return_values)
    MuchStub::CallSpy.new(**return_values).call_spy_tap do |spy|
      meths.each do |meth|
        stub(obj, meth) do |*pargs, **kargs, &block|
          spy.__send__(meth, *pargs, **kargs, &block)
        end
      end
    end
  end

  class Stub
    def self.key(object, method_name)
      "--#{object.object_id}--#{method_name}--"
    end

    attr_reader :method_name, :name, :ivar_name, :do

    def initialize(object, method_name, orig_caller = nil, &block)
      orig_caller ||= caller_locations
      @metaclass   = class << object; self; end
      @method_name = method_name.to_s
      @name        = "__muchstub_stub__#{object.object_id}_#{@method_name}"
      @ivar_name   = "@__muchstub_stub_#{object.object_id}_" \
                     "#{@method_name.to_sym.object_id}"

      setup(object, orig_caller)

      @do     = block
      @lookup = {}
    end

    def do=(block)
      @do = block || @do
    end

    def call_method(*pargs, **kargs, &block)
      @method.call(*pargs, **kargs, &block)
    end

    def call(*pargs, orig_caller: nil, **kargs, &block)
      orig_caller ||= caller_locations
      args = combined_args(pargs, kargs)
      unless MuchStub.arity_matches?(@method, args)
        raise(
          StubArityError.new(
            @method,
            args,
            method_name: @method_name,
            backtrace: orig_caller,
          ),
        )
      end
      lookup(pargs, kargs, orig_caller).call(*pargs, **kargs, &block)
    rescue NotStubbedError
      @lookup.rehash
      lookup(pargs, kargs, orig_caller).call(*pargs, **kargs, &block)
    end

    def with(*pargs, **kargs, &block)
      orig_caller = caller_locations
      args = combined_args(pargs, kargs)
      unless MuchStub.arity_matches?(@method, args)
        raise(
          StubArityError.new(
            @method,
            args,
            method_name: @method_name,
            backtrace: orig_caller,
          ),
        )
      end
      @lookup[args] = block
      self
    end

    def on_call(&on_call_block)
      stub_block =
        ->(*pargs, **kargs, &block){
          on_call_block&.call(MuchStub::Call.new(*pargs, **kargs, &block))
        }
      if @lookup.empty?
        @do = stub_block
      elsif @lookup.value?(nil)
        @lookup.transform_values!{ |value| value.nil? ? stub_block : value }
      end
      self
    end

    def teardown
      @metaclass.send(:undef_method, @method_name)
      MuchStub.send(:remove_instance_variable, @ivar_name)
      @metaclass.send(:alias_method, @method_name, @name)
      @metaclass.send(:undef_method, @name)
    end

    def inspect
      "#<#{self.class}:#{format("0x0%x", (object_id << 1))}" \
      " @method_name=#{@method_name.inspect}" \
      ">"
    end

    private

    def setup(object, orig_caller)
      unless object.respond_to?(@method_name)
        msg = "#{object.inspect} does not respond to `#{@method_name}`"
        raise StubError, msg, orig_caller.map(&:to_s)
      end
      is_constant          = object.is_a?(Module)
      local_object_methods = object.methods(false).map(&:to_s)
      all_object_methods   = object.methods.map(&:to_s)
      if (is_constant && !local_object_methods.include?(@method_name)) ||
         (!is_constant && !all_object_methods.include?(@method_name))
        params_list = ParameterList.new(object, @method_name)
        @metaclass.class_eval <<-method
          def #{@method_name}(#{params_list}); super; end
        method
      end

      # already stubbed
      unless local_object_methods.include?(@name)
        @metaclass.send(:alias_method, @name, @method_name)
      end
      @method = object.method(@name)

      MuchStub.instance_variable_set(@ivar_name, self)
      @metaclass.class_eval <<-stub_method
        def #{@method_name}(*pargs, **kargs, &block)
          MuchStub
            .instance_variable_get("#{@ivar_name}")
            .call(*pargs, orig_caller: caller_locations, **kargs, &block)
        end
      stub_method
    end

    def lookup(pargs, kargs, orig_caller)
      args = combined_args(pargs, kargs)
      @lookup.fetch(args) do
        self.do ||
        begin
          msg = "#{inspect_call(args)} not stubbed."
          inspect_lookup_stubs.tap do |stubs|
            msg += "\nStubs:\n#{stubs}" unless stubs.empty?
          end
          raise NotStubbedError, msg, orig_caller.map(&:to_s)
        end
      end ||
      raise(
        StubError,
        "#{inspect_call(args)} stubbed with no block.",
        orig_caller.map(&:to_s),
      )
    end

    def inspect_lookup_stubs
      @lookup.keys.map{ |args| "    - #{inspect_call(args)}" }.join("\n")
    end

    def inspect_call(args)
      "`#{@method_name}(#{args.map(&:inspect).join(",")})`"
    end

    def combined_args(pargs, kargs)
      pargs + [(kargs.empty? ? nil : kargs)].compact
    end
  end

  StubError       = Class.new(ArgumentError)
  NotStubbedError = Class.new(StubError)
  StubArityError  =
    Class.new(StubError) do
      def initialize(method, args, method_name:, backtrace:)
        msg = "arity mismatch on `#{method_name}`: " \
              "expected #{number_of_args(method.arity)}, " \
              "called with #{args.size}"

        super(msg)
        set_backtrace(Array(backtrace).map(&:to_s))
      end

      private

      def number_of_args(arity)
        if arity < 0
          "at least #{(arity + 1).abs}"
        else
          arity
        end
      end
    end

  NullStub =
    Class.new do
      def teardown
        # no-op
      end
    end

  module ParameterList
    LETTERS = ("a".."z").to_a.freeze

    def self.new(object, method_name)
      arity = get_arity(object, method_name)
      params = build_params_from_arity(arity)
      params << "*pargs, **kargs" if arity < 0
      params << "&block"
      params.join(", ")
    end

    def self.get_arity(object, method_name)
      object.method(method_name).arity
    rescue NameError
      -1
    end

    def self.build_params_from_arity(arity)
      number = arity < 0 ? (arity + 1).abs : arity
      (0..(number - 1)).map{ |param_index| get_param_name(param_index) }
    end

    def self.get_param_name(param_index)
      param_index += LETTERS.size # avoid getting 0 for the number of letters
      number_of_letters, letter_index = param_index.divmod(LETTERS.size)
      LETTERS[letter_index] * number_of_letters
    end
  end
end

# Kernel#caller_locations polyfill for pre ruby 2.0.0
if RUBY_VERSION =~ /\A1\..+/ && !Kernel.respond_to?(:caller_locations)
  module Kernel
    def caller_locations(start = 1, length = nil)
      length ? caller[start, length] : caller[start..-1]
    end
  end
end
