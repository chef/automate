# frozen_string_literal: true

require "assert"
require "much-stub/call_spy"

require "test/support/factory"

class MuchStub::CallSpy
  class UnitTests < ::Assert::Context
    desc "MuchStub::CallSpy"
    setup do
      @unit_class = MuchStub::CallSpy
    end
  end

  class InitTests < UnitTests
    desc "when init"
    before do
      @spy = @unit_class.new
    end
    subject{ @spy }

    should "spy on method calls and return itself" do
      assert_true subject.respond_to?(:get)

      assert_equal [], subject.get_calls
      assert_nil subject.get_last_called_with
      assert_nil subject.get_called_with
      assert_equal 0, subject.get_call_count
      assert_false subject.get_called?

      assert_same subject, subject.get

      assert_true subject.respond_to?(:get)

      assert_kind_of Array, subject.get_calls
      assert_kind_of MuchStub::Call, subject.get_last_called_with
      assert_equal [], subject.get_last_called_with.args
      assert_equal subject.get_last_called_with, subject.get_called_with
      assert_equal 1, subject.get_call_count
      assert_true subject.get_called?

      assert_same subject, subject.set!("value1")
      assert_same subject, subject.set!("value2")

      assert_kind_of Array, subject.set_bang_calls
      assert_kind_of MuchStub::Call, subject.set_bang_last_called_with
      assert_equal ["value2"], subject.set_bang_last_called_with.args
      assert_equal 2, subject.set_bang_call_count
      assert_true subject.set_bang_called?
    end

    should "normalize method names in call query methods" do
      assert_same subject, subject.set!("value1")
      assert_kind_of Array, subject.set_bang_calls
      assert_kind_of MuchStub::Call, subject.set_bang_last_called_with
      assert_equal ["value1"], subject.set_bang_last_called_with.args
      assert_equal ["value1"], subject.set_bang_called_with.args
      assert_equal 1, subject.set_bang_call_count
      assert_true subject.set_bang_called?

      assert_same subject, subject.any?
      assert_kind_of Array, subject.any_predicate_calls
      assert_kind_of MuchStub::Call, subject.any_predicate_last_called_with
      assert_kind_of MuchStub::Call, subject.any_predicate_called_with
      assert_equal 1, subject.any_predicate_call_count
      assert_true subject.any_predicate_called?
    end
  end

  class InitWithReturnValuesTests < UnitTests
    desc "when init with return values"
    setup do
      @spy = @unit_class.new(any?: false)
    end
    subject{ @spy }

    should "return the given values instead of itself if that method is "\
           "called" do
      assert_false subject.get.set!("value1").any?
      assert_true subject.get_called?
      assert_equal ["value1"], subject.set_bang_called_with.args
      assert_true subject.any_predicate_called?
    end
  end

  class InitWithReturnValueProcsTests < UnitTests
    desc "when init with return value procs"
    setup do
      @result = Factory.boolean
      @spy = @unit_class.new(any?: ->(_call){ @result })
    end
    subject{ @spy }

    should "return the value of calling the procs instead of itself" do
      assert_equal @result, subject.get.set!("value1").any?
    end
  end

  class InitWithNilReturnValuesTests < UnitTests
    desc "when init with nil return values"
    setup do
      @spy = @unit_class.new(result: nil)
    end
    subject{ @spy }

    should "return nil" do
      assert_equal "nil", subject.get.set!("value1").result.inspect
    end
  end
end
