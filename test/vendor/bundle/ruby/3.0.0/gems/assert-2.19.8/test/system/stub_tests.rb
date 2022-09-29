# frozen_string_literal: true

require "assert"
require "assert/stub"

class Assert::Stub
  class SystemTests < Assert::Context
    desc "Assert::Stub"
  end

  class InstanceTests < SystemTests
    desc "for instance methods"
    subject{ TestClass.new }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class ClassTests < SystemTests
    desc "for singleton methods on a class"
    subject{ TestClass }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class ModuleTests < SystemTests
    desc "for singleton methods on a module"
    subject{ TestModule }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class ExtendedTests < SystemTests
    desc "for extended methods"
    subject{ Class.new{ extend TestMixin } }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class IncludedTests < SystemTests
    desc "for an included method"
    subject do
      Class.new{ include TestMixin }.new
    end

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class InheritedClassTests < SystemTests
    desc "for an inherited class method"
    subject{ Class.new(TestClass) }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class InheritedInstanceTests < SystemTests
    desc "for an inherited instance method"
    subject{ Class.new(TestClass).new }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "not allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withargs).with{} }.raises
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.raises

      assert_that{ Assert.stub(subject, :minargs).with{} }.raises
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.raises

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.raises
    end

    should "not allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.raises

      assert_that{ subject.withargs }.raises
      assert_that{ subject.withargs(1, 2) }.raises

      assert_that{ subject.minargs }.raises
      assert_that{ subject.minargs(1) }.raises

      assert_that{ subject.withblock(1) }.raises
    end
  end

  class DelegateClassTests < SystemTests
    desc "a class that delegates another object"
    subject{ DelegateClass }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.does_not_raise

      assert_that{ Assert.stub(subject, :withargs).with{} }.does_not_raise
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.does_not_raise

      assert_that{ Assert.stub(subject, :minargs).with{} }.does_not_raise
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.does_not_raise

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.does_not_raise
    end

    should "allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.does_not_raise

      assert_that{ subject.withargs }.does_not_raise
      assert_that{ subject.withargs(1, 2) }.does_not_raise

      assert_that{ subject.minargs }.does_not_raise
      assert_that{ subject.minargs(1) }.does_not_raise

      assert_that{ subject.withblock(1) }.does_not_raise
    end
  end

  class DelegateInstanceTests < SystemTests
    desc "an instance that delegates another object"
    subject{ DelegateClass.new }

    setup do
      Assert.stub(subject, :noargs){ "default" }
      Assert.stub(subject, :noargs).with{ "none" }

      Assert.stub(subject, :withargs){ "default" }
      Assert.stub(subject, :withargs).with(1){ "one" }

      Assert.stub(subject, :anyargs){ "default" }
      Assert.stub(subject, :anyargs).with(1, 2){ "one-two" }

      Assert.stub(subject, :minargs){ "default" }
      Assert.stub(subject, :minargs).with(1, 2){ "one-two" }
      Assert.stub(subject, :minargs).with(1, 2, 3){ "one-two-three" }

      Assert.stub(subject, :withblock){ "default" }
    end

    should "allow stubbing a method that doesn't take args" do
      assert_that(subject.noargs).equals("none")
    end

    should "allow stubbing a method that takes args" do
      assert_that(subject.withargs(1)).equals("one")
      assert_that(subject.withargs(2)).equals("default")
    end

    should "allow stubbing a method that takes any args" do
      assert_that(subject.anyargs).equals("default")
      assert_that(subject.anyargs(1)).equals("default")
      assert_that(subject.anyargs(1, 2)).equals("one-two")
    end

    should "allow stubbing a method that takes a minimum number of args" do
      assert_that(subject.minargs(1, 2)).equals("one-two")
      assert_that(subject.minargs(1, 2, 3)).equals("one-two-three")
      assert_that(subject.minargs(1, 2, 4)).equals("default")
      assert_that(subject.minargs(1, 2, 3, 4)).equals("default")
    end

    should "allow stubbing a method that takes a block" do
      assert_that(subject.withblock).equals("default")
      assert_that(subject.withblock{ "my-block" }).equals("default")
    end

    should "allow stubbing methods with invalid arity" do
      assert_that{ Assert.stub(subject, :noargs).with(1){} }.does_not_raise

      assert_that{ Assert.stub(subject, :withargs).with{} }.does_not_raise
      assert_that{ Assert.stub(subject, :withargs).with(1, 2){} }.does_not_raise

      assert_that{ Assert.stub(subject, :minargs).with{} }.does_not_raise
      assert_that{ Assert.stub(subject, :minargs).with(1){} }.does_not_raise

      assert_that{ Assert.stub(subject, :withblock).with(1){} }.does_not_raise
    end

    should "allow calling methods with invalid arity" do
      assert_that{ subject.noargs(1) }.does_not_raise

      assert_that{ subject.withargs }.does_not_raise
      assert_that{ subject.withargs(1, 2) }.does_not_raise

      assert_that{ subject.minargs }.does_not_raise
      assert_that{ subject.minargs(1) }.does_not_raise

      assert_that{ subject.withblock(1) }.does_not_raise
    end
  end

  class ParentAndChildClassTests < SystemTests
    desc "for a parent method stubbed on both the parent and child"
    setup do
      Assert.stub(parent_class, :new){ "parent" }
      Assert.stub(child_class, :new){ "child" }
    end

    let(:parent_class){ Class.new }
    let(:child_class){ Class.new(parent_class) }

    should "allow stubbing the methods individually" do
      assert_that(parent_class.new).equals("parent")
      assert_that(child_class.new).equals("child")
    end
  end

  class TestClass
    def self.noargs
    end

    def self.withargs(a)
    end

    def self.anyargs(*args)
    end

    def self.minargs(a, b, *args)
    end

    def self.withblock(&block)
    end

    def noargs
    end

    def withargs(a)
    end

    def anyargs(*args)
    end

    def minargs(a, b, *args)
    end

    def withblock(&block)
    end
  end

  module TestModule
    def self.noargs
    end

    def self.withargs(a)
    end

    def self.anyargs(*args)
    end

    def self.minargs(a, b, *args)
    end

    def self.withblock(&block)
    end
  end

  module TestMixin
    def noargs
    end

    def withargs(a)
    end

    def anyargs(*args)
    end

    def minargs(a, b, *args)
    end

    def withblock(&block)
    end
  end

  class DelegateClass
    def self.respond_to_missing?(*args)
      TestClass.respond_to?(*args) || super
    end

    def self.method_missing(name, *args, &block)
      TestClass.respond_to?(name) ? TestClass.send(name, *args, &block) : super
    end

    def initialize
      @delegate = TestClass.new
    end

    def respond_to_missing?(*args)
      @delegate.respond_to?(*args) || super
    end

    def method_missing(name, *args, &block)
      @delegate.respond_to?(name) ? @delegate.send(name, *args, &block) : super
    end
  end
end
