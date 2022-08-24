# frozen_string_literal: true

require "assert"
require "much-mixin"

module MuchMixin
  class SystemTests < Assert::Context
    desc "MuchMixin"
    setup do
      @my_class = MyClass.new
    end
    subject{ @my_class }

    should "class eval the mix-in included block on MyClass" do
      assert_equal "another", subject.another
    end

    should "add the mix-in class methods to MyClass" do
      assert_equal "a-class-method", MyClass.a_class_method
    end

    should "add the mix-in instance methods to MyClass" do
      assert_equal "an-instance-method", subject.an_instance_method
    end

    module AnotherMixin
      def another
        "another"
      end
    end

    module MyMuchMixin
      include MuchMixin

      mixin_included do
        include AnotherMixin
      end

      mixin_class_methods do
        def a_class_method
          "a-class-method"
        end
      end

      mixin_instance_methods do
        def an_instance_method
          "an-instance-method"
        end
      end
    end

    class MyClass
      include MyMuchMixin
    end
  end
end
