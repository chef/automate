# frozen_string_literal: true

require "assert"
require "much-mixin"

module MuchMixin
  class UnitTests < Assert::Context
    desc "MuchMixin"
    setup do
      @block1 = proc{ 1 }
      @block2 = proc{ 2 }

      @muchmixin = Module.new{ include MuchMixin }
    end
    subject{ @muchmixin }

    should have_imeths :much_mixin_included_detector
    should have_imeths :much_mixin_included_blocks
    should have_imeths :mixin_included, :after_mixin_included

    should "know its included detector" do
      mixin = subject.much_mixin_included_detector
      assert_instance_of Module, mixin
      assert_same mixin, subject.much_mixin_included_detector
      exp = subject::MuchMixinIncludedDetector
      assert_same exp, subject.much_mixin_included_detector
    end

    should "have no mix-in included blocks by default" do
      assert_empty subject.much_mixin_included_blocks
      assert_empty subject.much_mixin_after_included_blocks
    end

    should "append blocks" do
      subject.mixin_included(&@block1)
      subject.mixin_included(&@block2)

      assert_equal @block1, subject.much_mixin_included_blocks.first
      assert_equal @block2, subject.much_mixin_included_blocks.last
    end
  end

  class MixedInTests < UnitTests
    desc "when mixed in"
    setup do
      @receiver =
        Class.new do
          def self.inc_block1
            @block1_count ||= 0
            @block1_count += 1
          end

          def self.block1_count
            @block1_count ||= 0
          end

          def self.inc_block2
            @block2_count ||= 0
            @block2_count += 1
          end

          def self.block2_count
            @block2_count ||= 0
          end

          def self.do_something_count
            @do_something_count ||= 0
          end
        end
    end

    should "call the mix-in included blocks" do
      assert_equal 0, @receiver.block1_count
      assert_equal 0, @receiver.block2_count
      assert_equal 0, @receiver.do_something_count

      @receiver.send(:include, TestMuchMixin)

      assert_equal 1, @receiver.block1_count
      assert_equal 1, @receiver.block2_count
      assert_equal 1, @receiver.do_something_count
    end

    should "call blocks only once no matter even if previously mixed in" do
      @receiver.send(:include, TestMuchMixin)

      assert_equal 1, @receiver.block1_count
      assert_equal 1, @receiver.block2_count
      assert_equal 1, @receiver.do_something_count

      @receiver.send(:include, TestMuchMixin)

      assert_equal 1, @receiver.block1_count
      assert_equal 1, @receiver.block2_count
      assert_equal 1, @receiver.do_something_count
    end

    should "call blocks only once even if mixed in by a 3rd party" do
      third_party =
        Module.new do
          def self.included(receiver)
            receiver.send(:include, TestMuchMixin)
          end
        end
      @receiver.send(:include, third_party)

      assert_equal 1, @receiver.block1_count
      assert_equal 1, @receiver.block2_count
      assert_equal 1, @receiver.do_something_count

      @receiver.send(:include, TestMuchMixin)

      assert_equal 1, @receiver.block1_count
      assert_equal 1, @receiver.block2_count
      assert_equal 1, @receiver.do_something_count

      @receiver.send(:include, third_party)

      assert_equal 1, @receiver.block1_count
      assert_equal 1, @receiver.block2_count
      assert_equal 1, @receiver.do_something_count
    end
  end

  TestMuchMixin =
    Module.new do
      include MuchMixin

      mixin_included{ inc_block1 }
      after_mixin_included do
        inc_block2
        do_something
      end

      mixin_class_methods do
        def do_something
          @do_something_count ||= 0
          @do_something_count += 1
        end
      end
    end
end
