# frozen_string_literal: true

require "assert/utils"

module Assert
  module Assertions
    IGNORED_ASSERTION_HELPERS =
      [
        :assert_throws,
        :assert_nothing_thrown,
        :assert_operator,
        :refute_operator,
        :assert_in_epsilon,
        :refute_in_epsilon,
        :assert_in_delta,
        :refute_in_delta,
        :assert_send,
      ]

    def assert_block(desc = nil)
      assert(yield, desc){ "Expected block to return a true value." }
    end

    def assert_not_block(desc = nil)
      assert(!yield, desc){ "Expected block to not return a true value." }
    end
    alias_method :refute_block, :assert_not_block

    def assert_empty(collection, desc = nil)
      assert(collection.empty?, desc) do
        "Expected #{Assert::U.show(collection, __assert_config__)} to "\
        "be empty."
      end
    end

    def assert_not_empty(collection, desc = nil)
      assert(!collection.empty?, desc) do
        "Expected #{Assert::U.show(collection, __assert_config__)} to "\
        "not be empty."
      end
    end
    alias_method :refute_empty, :assert_not_empty

    def assert_equal(exp, act, desc = nil)
      assert(act == exp, desc) do
        c = __assert_config__
        exp_show = Assert::U.show_for_diff(exp, c)
        act_show = Assert::U.show_for_diff(act, c)

        if c.use_diff_proc.call(exp_show, act_show)
          "Expected does not equal actual, diff:\n"\
          "#{c.run_diff_proc.call(exp_show, act_show)}"
        else
          "Expected #{Assert::U.show(act, c)} to "\
          "be equal to #{Assert::U.show(exp, c)}."
        end
      end
    end

    def assert_not_equal(exp, act, desc = nil)
      assert(act != exp, desc) do
        c = __assert_config__
        exp_show = Assert::U.show_for_diff(exp, c)
        act_show = Assert::U.show_for_diff(act, c)

        if c.use_diff_proc.call(exp_show, act_show)
          "Expected equals actual, diff:\n"\
          "#{c.run_diff_proc.call(exp_show, act_show)}"
        else
          "Expected #{Assert::U.show(act, c)} to "\
          "not be equal to #{Assert::U.show(exp, c)}."
        end
      end
    end
    alias_method :refute_equal, :assert_not_equal

    def assert_file_exists(file_path, desc = nil)
      assert(File.exist?(File.expand_path(file_path)), desc) do
        "Expected #{Assert::U.show(file_path, __assert_config__)} to exist."
      end
    end

    def assert_not_file_exists(file_path, desc = nil)
      assert(!File.exist?(File.expand_path(file_path)), desc) do
        "Expected #{Assert::U.show(file_path, __assert_config__)} to not exist."
      end
    end
    alias_method :refute_file_exists, :assert_not_file_exists

    def assert_includes(object, collection, desc = nil)
      assert(collection.include?(object), desc) do
        "Expected #{Assert::U.show(collection, __assert_config__)}"\
        " to include #{Assert::U.show(object, __assert_config__)}."
      end
    end
    alias_method :assert_included, :assert_includes

    def assert_not_includes(object, collection, desc = nil)
      assert(!collection.include?(object), desc) do
        "Expected #{Assert::U.show(collection, __assert_config__)}"\
        " to not include #{Assert::U.show(object, __assert_config__)}."
      end
    end
    alias_method :assert_not_included, :assert_not_includes
    alias_method :refute_includes, :assert_not_includes
    alias_method :refute_included, :assert_not_includes

    def assert_instance_of(klass, instance, desc = nil)
      assert(instance.instance_of?(klass), desc) do
        "Expected #{Assert::U.show(instance, __assert_config__)} "\
        "(#{instance.class}) to be an instance of #{klass}."
      end
    end

    def assert_not_instance_of(klass, instance, desc = nil)
      assert(!instance.instance_of?(klass), desc) do
        "Expected #{Assert::U.show(instance, __assert_config__)} "\
        "(#{instance.class}) to not be an instance of #{klass}."
      end
    end
    alias_method :refute_instance_of, :assert_not_instance_of

    def assert_is_a(klass, instance, desc = nil)
      assert(instance.is_a?(klass), desc) do
        "Expected #{Assert::U.show(instance, __assert_config__)} "\
        "(#{instance.class}) to be a `#{klass}`."
      end
    end
    alias_method :assert_kind_of, :assert_is_a

    def assert_is_not_a(klass, instance, desc = nil)
      assert(!instance.is_a?(klass), desc) do
        "Expected #{Assert::U.show(instance, __assert_config__)} "\
        "(#{instance.class}) to not be a `#{klass}`."
      end
    end
    alias_method :assert_not_a, :assert_is_not_a
    alias_method :assert_not_kind_of, :assert_is_not_a
    alias_method :refute_is_a, :assert_is_not_a
    alias_method :refute_kind_of, :assert_is_not_a

    def assert_match(exp, act, desc = nil)
      exp_regex =
        String === exp && String === act ? /#{Regexp.escape(exp)}/ : exp
      assert(act =~ exp_regex, desc) do
        "Expected #{Assert::U.show(act, __assert_config__)}"\
        " to match #{Assert::U.show(exp, __assert_config__)}."
      end
    end

    def assert_not_match(exp, act, desc = nil)
      exp = String === exp && String === act ? /#{Regexp.escape(exp)}/ : exp
      assert(act !~ exp, desc) do
        "Expected #{Assert::U.show(act, __assert_config__)}"\
        " to not match #{Assert::U.show(exp, __assert_config__)}."
      end
    end
    alias_method :refute_match, :assert_not_match
    alias_method :assert_no_match, :assert_not_match

    def assert_nil(object, desc = nil)
      assert(object.nil?, desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} to be nil."
      end
    end

    def assert_not_nil(object, desc = nil)
      assert(!object.nil?, desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} to not be nil."
      end
    end
    alias_method :refute_nil, :assert_not_nil

    def assert_true(object, desc = nil)
      assert(object == true, desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} to be true."
      end
    end

    def assert_not_true(object, desc = nil)
      assert(object != true, desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} to not be true."
      end
    end
    alias_method :refute_true, :assert_not_true

    def assert_false(object, desc = nil)
      assert(object == false, desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} to be false."
      end
    end

    def assert_not_false(object, desc = nil)
      assert(object != false, desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} to not be false."
      end
    end
    alias_method :refute_false, :assert_not_false

    def assert_raises(*exceptions, &block)
      desc = exceptions.last.is_a?(String) ? exceptions.pop : nil
      err = RaisedException.new(exceptions, &block)
      assert(err.raised?, desc){ err.msg }
      err.exception
    end
    alias_method :assert_raise, :assert_raises

    def assert_nothing_raised(*exceptions, &block)
      desc = exceptions.last.is_a?(String) ? exceptions.pop : nil
      err = NoRaisedException.new(exceptions, &block)
      assert(!err.raised?, desc){ err.msg }
    end
    alias_method :assert_not_raises, :assert_nothing_raised
    alias_method :assert_not_raise, :assert_nothing_raised

    def assert_changes(
          ruby_string_to_eval,
          desc: nil,
          from: Assert::ActualValue.not_given,
          to: Assert::ActualValue.not_given,
          &block)
      desc_msg = desc ? "#{desc}\n" : ""
      from_eval = instance_eval(ruby_string_to_eval)
      if Assert::ActualValue.given?(from)
        assert_equal(
          from,
          from_eval,
          "#{desc_msg}Expected `#{ruby_string_to_eval}` to "\
          "change from `#{from.inspect}`.",
        )
      end

      block.call

      to_eval = instance_eval(ruby_string_to_eval)
      if Assert::ActualValue.given?(to)
        assert_equal(
          to,
          to_eval,
          "#{desc_msg}Expected `#{ruby_string_to_eval}` to "\
          "change to `#{to.inspect}`.",
        )
      end

      if (
        Assert::ActualValue.not_given?(from) &&
        Assert::ActualValue.not_given?(to)
      )
        assert_not_equal(
          from_eval,
          to_eval,
          "#{desc_msg}Expected `#{ruby_string_to_eval}` to change; "\
          "it was `#{from_eval.inspect}` and didn't change.",
        )
      end
    end

    def assert_not_changes(
          ruby_string_to_eval,
          desc: nil,
          from: Assert::ActualValue.not_given,
          &block)
      desc_msg = desc ? "#{desc}\n" : ""
      from_eval = instance_eval(ruby_string_to_eval)
      if Assert::ActualValue.given?(from)
        assert_equal(
          from,
          from_eval,
          "#{desc_msg}Expected `#{ruby_string_to_eval}` to "\
          "not change from `#{from.inspect}`.",
        )
      end

      block.call

      to_eval = instance_eval(ruby_string_to_eval)
      assert_equal(
        from_eval,
        to_eval,
        "#{desc_msg}Expected `#{ruby_string_to_eval}` to not change; "\
        "it was `#{from_eval.inspect}` and changed to `#{to_eval.inspect}`.",
      )
    end
    alias_method :refute_changes, :assert_not_changes

    def assert_respond_to(method, object, desc = nil)
      assert(object.respond_to?(method), desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} "\
        "(#{object.class}) to respond to `#{method}`."
      end
    end
    alias_method :assert_responds_to, :assert_respond_to

    def assert_not_respond_to(method, object, desc = nil)
      assert(!object.respond_to?(method), desc) do
        "Expected #{Assert::U.show(object, __assert_config__)} "\
        "(#{object.class}) to not respond to `#{method}`."
      end
    end
    alias_method :assert_not_responds_to, :assert_not_respond_to
    alias_method :refute_respond_to, :assert_not_respond_to
    alias_method :refute_responds_to, :assert_not_respond_to

    def assert_same(exp, act, desc = nil)
      assert(act.equal?(exp), desc) do
        c = __assert_config__
        exp_show = Assert::U.show_for_diff(exp, c)
        act_show = Assert::U.show_for_diff(act, c)
        exp_id = "#<#{exp.class}:#{"0x0%x" % (exp.object_id << 1)}>"
        act_id = "#<#{act.class}:#{"0x0%x" % (act.object_id << 1)}>"

        if c.use_diff_proc.call(exp_show, act_show)
          "Expected #{act_id} to be the same as #{exp_id}, diff:\n"\
          "#{c.run_diff_proc.call(exp_show, act_show)}"
        else
          "Expected #{Assert::U.show(act, c)} (#{act_id}) to "\
          "be the same as #{Assert::U.show(exp, c)} (#{exp_id})."
        end
      end
    end

    def assert_not_same(exp, act, desc = nil)
      assert(!act.equal?(exp), desc) do
        c = __assert_config__
        exp_show = Assert::U.show_for_diff(exp, c)
        act_show = Assert::U.show_for_diff(act, c)
        exp_id = "#<#{exp.class}:#{"0x0%x" % (exp.object_id << 1)}>"
        act_id = "#<#{act.class}:#{"0x0%x" % (act.object_id << 1)}>"

        if c.use_diff_proc.call(exp_show, act_show)
          "Expected #{act_id} to not be the same as #{exp_id}, diff:\n"\
          "#{c.run_diff_proc.call(exp_show, act_show)}"
        else
          "Expected #{Assert::U.show(act, c)} (#{act_id}) to "\
          "not be the same as #{Assert::U.show(exp, c)} (#{exp_id})."
        end
      end
    end
    alias_method :refute_same, :assert_not_same

    private

    def __assert_config__
      raise NotImplementedError # should be defined by the config mixing this in
    end

    # exception raised utility classes

    class CheckException
      attr_reader :msg, :exception

      def initialize(exceptions, &block)
        @exceptions = exceptions
        # rubocop:disable Lint/SuppressedException
        # rubocop:disable Lint/RescueException
        # rubocop:disable Naming/RescuedExceptionsVariableName
        begin
          block.call
        rescue Exception => @exception
        end
        # rubocop:enable Lint/SuppressedException
        # rubocop:enable Lint/RescueException
        # rubocop:enable Naming/RescuedExceptionsVariableName
        @msg = "#{exceptions_sentence(@exceptions)} #{exception_details}"
      end

      def raised?
        !@exception.nil? && is_one_of?(@exception, @exceptions)
      end

      private

      def is_one_of?(exception, exceptions)
        exceptions.empty? || exceptions.any? do |exp|
          if exp.instance_of?(Module)
            exception.is_a?(exp)
          else
            exception.class == exp
          end
        end
      end

      def exceptions_sentence(exceptions)
        if exceptions.size <= 1
          (exceptions.first || "An").to_s
        else
          "#{exceptions[0..-2].join(", ")} or #{exceptions[-1]}"
        end
      end

      def exception_details(raised_msg = nil, no_raised_msg = nil)
        if @exception
          backtrace = Assert::Result::Backtrace.new(@exception.backtrace)
          [
            raised_msg,
            "Class: `#{@exception.class}`",
            "Message: `#{@exception.message.inspect}`",
            "---Backtrace---",
            backtrace.filtered.to_s,
            "---------------",
          ]
            .compact
            .join("\n")
        else
          no_raised_msg
        end
      end
    end

    class RaisedException < CheckException
      def exception_details
        super(
          "exception expected, not:",
          "exception expected but nothing raised."
        )
      end
    end

    class NoRaisedException < CheckException
      def exception_details
        super("exception not expected, but raised:")
      end
    end
  end
end
