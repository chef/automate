# frozen_string_literal: true

require "assert"
require "much-stub/call"

require "test/support/factory"

class MuchStub::Call
  class UnitTests < Assert::Context
    desc "MuchStub::Call"
    setup do
      @unit_class = MuchStub::Call

      @pargs = [Factory.string, Factory.integer]
      @kargs = {
        one: 1,
        two: 2,
      }
      @block = ->{}
    end
  end

  class InitWithNoArgsTests < UnitTests
    desc "when init with no args"
    subject{ @unit_class.new }

    should "know its attrs" do
      assert_nil subject.pargs
      assert_nil subject.kargs
      assert_equal [], subject.args
      assert_nil subject.block
    end
  end

  class InitWithOnlyPositionalArgsTests < UnitTests
    desc "when init with only positional args"
    subject{ @unit_class.new(*@pargs) }

    should "know its attrs" do
      assert_equal @pargs, subject.pargs
      assert_nil subject.kargs
      assert_equal [*@pargs], subject.args
      assert_nil subject.block
    end
  end

  class InitWithOnlyKeywordArgsTests < UnitTests
    desc "when init with only keyword args"
    subject{ @unit_class.new(**@kargs) }

    should "know its attrs" do
      assert_nil subject.pargs
      assert_equal @kargs, subject.kargs
      assert_equal [@kargs], subject.args
      assert_nil subject.block
    end
  end

  class InitWithBothPositionalAndKeywordArgsTests < UnitTests
    desc "when init with only keyword args"
    subject{ @unit_class.new(*@pargs, **@kargs, &@block) }

    should "know its attrs" do
      assert_equal @pargs, subject.pargs
      assert_equal @kargs, subject.kargs
      assert_equal [*@pargs, @kargs], subject.args
      assert_equal @block, subject.block
    end
  end
end
