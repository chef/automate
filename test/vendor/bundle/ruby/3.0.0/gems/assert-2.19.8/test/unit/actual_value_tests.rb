# frozen_string_literal: true

require "assert"
require "assert/actual_value"

class Assert::ActualValue
  class UnitTests < Assert::Context
    desc "Assert::ActualValue"
    subject{ unit_class }

    let(:unit_class){ Assert::ActualValue }
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new(actual_value1, context: context1) }

    let(:actual_value1){ Factory.string }
    let(:context1){ Factory.modes_off_context }

    should have_imeths :returns_true, :does_not_return_true
    should have_imeths :raises, :does_not_raise
    should have_imeths :changes, :does_not_change
    should have_imeths :is_a, :is_not_a
    should have_imeths :is_a_kind_of, :is_not_a_kind_of
    should have_imeths :is_kind_of, :is_not_kind_of
    should have_imeths :is_an_instance_of, :is_not_an_instance_of
    should have_imeths :is_instance_of, :is_not_instance_of
    should have_imeths :responds_to, :does_not_respond_to
    should have_imeths :is_the_same_as, :is_not_the_same_as
    should have_imeths :is, :is_not
    should have_imeths :equals, :does_not_equal
    should have_imeths :is_equal_to, :is_not_equal_to
    should have_imeths :matches, :does_not_match
    should have_imeths :is_empty, :is_not_empty
    should have_imeths :includes, :does_not_include
    should have_imeths :is_nil, :is_not_nil
    should have_imeths :is_true, :is_not_true
    should have_imeths :is_false, :is_not_false
    should have_imeths :is_a_file, :is_not_a_file
  end

  class MethodsTests < InitTests
    desc "methods"

    let(:args1){ Factory.integer(3).times.map{ Factory.string } }
    let(:capture_last_call_block){ ->(call){ @last_call = call } }

    should "call to their equivalent context methods" do
      assert_calls(
        :assert_block,
        when_calling: :returns_true,
        on_value: ->{},
      ) do |value, call|
        assert_equal args1, call.args
        assert_equal value, call.block
      end

      assert_calls(
        :assert_not_block,
        when_calling: :does_not_return_true,
        on_value: ->{},
      ) do |value, call|
        assert_equal args1, call.args
        assert_equal value, call.block
      end

      assert_calls(
        :assert_raises,
        when_calling: :raises,
        on_value: ->{},
      ) do |value, call|
        assert_equal args1, call.args
        assert_equal value, call.block
      end

      assert_calls(
        :assert_nothing_raised,
        when_calling: :does_not_raise,
        on_value: ->{},
      ) do |value, call|
        assert_equal args1, call.args
        assert_equal value, call.block
      end

      assert_calls(
        :assert_changes,
        when_calling: :changes,
        on_value: ->{},
      ) do |value, call|
        assert_equal args1, call.args
        assert_equal value, call.block
      end

      assert_calls(
        :assert_not_changes,
        when_calling: :does_not_change,
        on_value: ->{},
      ) do |value, call|
        assert_equal args1, call.args
        assert_equal value, call.block
      end

      assert_calls(
        :assert_is_a,
        when_calling: [:is_a, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_is_a,
        when_calling: [:is_a_kind_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_is_a,
        when_calling: [:is_kind_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_is_not_a,
        when_calling: [:is_not_a, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_is_not_a,
        when_calling: [:is_not_a_kind_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_is_not_a,
        when_calling: [:is_not_kind_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_instance_of,
        when_calling: [:is_an_instance_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_instance_of,
        when_calling: [:is_instance_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_not_instance_of,
        when_calling: [:is_not_an_instance_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_not_instance_of,
        when_calling: [:is_not_instance_of, String],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [String, value, *args1], call.args
      end

      assert_calls(
        :assert_responds_to,
        when_calling: [:responds_to, :method],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [:method, value, *args1], call.args
      end

      assert_calls(
        :assert_not_responds_to,
        when_calling: [:does_not_respond_to, :method],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [:method, value, *args1], call.args
      end

      assert_calls(
        :assert_same,
        when_calling: [:is_the_same_as, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_same,
        when_calling: [:is, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_not_same,
        when_calling: [:is_not_the_same_as, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_not_same,
        when_calling: [:is_not, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_equal,
        when_calling: [:equals, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_equal,
        when_calling: [:is_equal_to, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_not_equal,
        when_calling: [:does_not_equal, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_not_equal,
        when_calling: [:is_not_equal_to, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_match,
        when_calling: [:matches, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_not_match,
        when_calling: [:does_not_match, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_empty,
        when_calling: :is_empty,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_not_empty,
        when_calling: :is_not_empty,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_includes,
        when_calling: [:includes, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_not_includes,
        when_calling: [:does_not_include, "something"],
        on_value: actual_value1,
      ) do |value, call|
        assert_equal ["something", value, *args1], call.args
      end

      assert_calls(
        :assert_nil,
        when_calling: :is_nil,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_not_nil,
        when_calling: :is_not_nil,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_true,
        when_calling: :is_true,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_not_true,
        when_calling: :is_not_true,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_false,
        when_calling: :is_false,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_not_false,
        when_calling: :is_not_false,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_file_exists,
        when_calling: :is_a_file,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end

      assert_calls(
        :assert_not_file_exists,
        when_calling: :is_not_a_file,
        on_value: actual_value1,
      ) do |value, call|
        assert_equal [value, *args1], call.args
      end
    end

    private

    def assert_calls(context_method, when_calling:, on_value:)
      @last_call = nil
      Assert.stub_on_call(context1, context_method, &capture_last_call_block)

      unit_class
        .new(on_value, context: context1)
        .public_send(*when_calling, *args1)
      yield(on_value, @last_call)

      Assert.unstub(context1, context_method)
      @last_call = nil
    end
  end
end
