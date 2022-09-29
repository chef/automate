# frozen_string_literal: true

require "assert"
require "much-stub"

require "test/support/factory"

module MuchStub
  class UnitTests < Assert::Context
    desc "MuchStub"
  end

  class APITests < UnitTests
    desc "API"
    setup do
      @orig_value = Factory.string
      @stub_value = Factory.string

      @myclass =
        Class.new do
          def initialize(value)
            @value = value
          end

          def mymeth
            @value
          end
        end
      @myobj = @myclass.new(@orig_value)
    end

    should "build a stub" do
      stub1 = MuchStub.call(@myobj, :mymeth)
      assert_kind_of MuchStub::Stub, stub1

      stub2 = MuchStub.stub(@myobj, :mymeth)
      assert_kind_of MuchStub::Stub, stub2
    end

    should "build a stub with an on_call block" do
      my_meth_called_with = nil
      stub1 =
        MuchStub.on_call(@myobj, :mymeth) do |call|
          my_meth_called_with = call
        end

      @myobj.mymeth
      assert_kind_of MuchStub::Stub, stub1
      assert_equal [], my_meth_called_with.args

      my_meth_called_with = nil
      stub2 =
        MuchStub.stub_on_call(@myobj, :mymeth) do |call|
          my_meth_called_with = call
        end

      @myobj.mymeth
      assert_kind_of MuchStub::Stub, stub2
      assert_equal [], my_meth_called_with.args
    end

    should "lookup stubs that have been called before" do
      stub1 = MuchStub.call(@myobj, :mymeth)
      stub2 = MuchStub.call(@myobj, :mymeth)
      assert_same stub1, stub2
    end

    should "set the stub's do block if given a block" do
      MuchStub.call(@myobj, :mymeth)
      assert_raises(MuchStub::NotStubbedError){ @myobj.mymeth }
      MuchStub.call(@myobj, :mymeth){ @stub_value }
      assert_equal @stub_value, @myobj.mymeth
    end

    should "teardown stubs" do
      assert_equal @orig_value, @myobj.mymeth
      MuchStub.unstub(@myobj, :mymeth)
      assert_equal @orig_value, @myobj.mymeth

      assert_equal @orig_value, @myobj.mymeth
      MuchStub.call(@myobj, :mymeth){ @stub_value }
      assert_equal @stub_value, @myobj.mymeth
      MuchStub.unstub(@myobj, :mymeth)
      assert_equal @orig_value, @myobj.mymeth
    end

    should "know and teardown all stubs" do
      assert_equal @orig_value, @myobj.mymeth

      MuchStub.call(@myobj, :mymeth){ @stub_value }
      assert_equal @stub_value, @myobj.mymeth
      assert_equal 1, MuchStub.stubs.size

      MuchStub.unstub!
      assert_equal @orig_value, @myobj.mymeth
      assert_empty MuchStub.stubs
    end

    should "be able to call a stub's original method" do
      err =
        assert_raises(NotStubbedError) do
          MuchStub.stub_send(@myobj, :mymeth)
        end
      assert_includes "not stubbed.",                 err.message
      assert_includes "test/unit/much-stub_tests.rb", err.backtrace.first

      MuchStub.call(@myobj, :mymeth){ @stub_value }

      assert_equal @stub_value, @myobj.mymeth
      assert_equal @orig_value, MuchStub.stub_send(@myobj, :mymeth)
    end

    should "be able to add a stub tap" do
      my_meth_called_with = nil
      MuchStub.tap(@myobj, :mymeth) do |_value, *args|
        my_meth_called_with = args
      end

      assert_equal @orig_value, @myobj.mymeth
      assert_equal [], my_meth_called_with
    end

    should "be able to add a stub tap with an on_call block" do
      my_meth_called_with = nil
      MuchStub.tap_on_call(@myobj, :mymeth) do |_value, call|
        my_meth_called_with = call
      end

      assert_equal @orig_value, @myobj.mymeth
      assert_equal [], my_meth_called_with.args
    end

    should "be able to add a stubbed spy" do
      myclass =
        Class.new do
          def one
            self
          end

          def two(_val)
            self
          end

          def three
            self
          end

          def ready?
            false
          end
        end
      myobj = myclass.new

      spy =
        MuchStub.spy(
          myobj,
          :one,
          :two,
          :three,
          :to_s,
          ready?: true,
        )

      assert_equal spy, myobj.one
      assert_equal spy, myobj.two("a")
      assert_equal spy, myobj.three
      assert_equal spy, myobj.to_s

      assert_true myobj.one.two("b").three.ready?

      assert_kind_of MuchStub::CallSpy, spy
      assert_equal 2, spy.one_call_count
      assert_equal 2, spy.two_call_count
      assert_equal 2, spy.three_call_count
      assert_equal 1, spy.to_s_call_count
      assert_equal 1, spy.ready_predicate_call_count
      assert_equal ["b"], spy.two_last_called_with.args
      assert_true spy.ready_predicate_called?
    end
  end

  class StubTests < UnitTests
    desc "Stub"
    setup do
      @myclass =
        Class.new do
          def mymeth
            "meth"
          end

          def myval(val)
            val
          end

          def myargs(*args)
            args
          end

          def myvalargs(val1, val2, *args)
            [val1, val2, args]
          end

          def mykargs(val1:, **kargs)
            [val1, kargs]
          end

          def myblk(&block)
            block.call
          end
        end
      @myobj = @myclass.new

      @stub = MuchStub::Stub.new(@myobj, :mymeth)
    end
    subject{ @stub }

    should have_readers :method_name, :name, :ivar_name, :do
    should have_writers :do
    should have_cmeths :key
    should have_imeths :call_method, :call, :with, :teardown

    should "generate a key given an object and method name" do
      obj = @myobj
      meth = :mymeth
      assert_equal(
        "--#{obj.object_id}--#{meth}--",
        MuchStub::Stub.key(obj, meth),
      )
    end

    should "know its names" do
      assert_equal "mymeth", subject.method_name
      expected = "__muchstub_stub__#{@myobj.object_id}_#{subject.method_name}"
      assert_equal expected, subject.name
      expected = "@__muchstub_stub_#{@myobj.object_id}_" \
                 "#{subject.method_name.to_sym.object_id}"
      assert_equal expected, subject.ivar_name
    end

    should "complain when called if no do block was given" do
      err = assert_raises(MuchStub::NotStubbedError){ @myobj.mymeth }
      assert_includes "not stubbed.",                 err.message
      assert_includes "test/unit/much-stub_tests.rb", err.backtrace.first

      subject.do = proc{ "mymeth" }
      assert_nothing_raised do
        @myobj.mymeth
      end

      MuchStub::Stub.new(@myobj, :mymeth){ "mymeth" }
      assert_nothing_raised do
        @myobj.mymeth
      end
    end

    should "complain when called if no lookup block was given" do
      MuchStub::Stub.new(@myobj, :myval).with(1)

      err = assert_raises(MuchStub::StubError){ @myobj.myval(1) }
      assert_includes "stubbed with no block.",       err.message
      assert_includes "test/unit/much-stub_tests.rb", err.backtrace.first
    end

    should "complain if stubbing a method that the object doesn't respond to" do
      err =
        assert_raises(MuchStub::StubError) do
          MuchStub::Stub.new(@myobj, :some_other_meth)
        end
      assert_includes "does not respond to", err.message
      assert_includes "test/unit/much-stub_tests.rb", err.backtrace.first
    end

    should "complain if stubbed and called with mismatched arity" do
      MuchStub::Stub.new(@myobj, :myval){ "myval" }
      err = assert_raises(MuchStub::StubArityError){ @myobj.myval }
      assert_includes "arity mismatch on", err.message
      assert_includes "test/unit/much-stub_tests.rb", err.backtrace.first

      assert_nothing_raised{ @myobj.myval(1) }
      assert_raises(MuchStub::StubArityError){ @myobj.myval(1, 2) }

      MuchStub::Stub.new(@myobj, :myargs){ "myargs" }
      assert_nothing_raised{ @myobj.myargs }
      assert_nothing_raised{ @myobj.myargs(1) }
      assert_nothing_raised{ @myobj.myargs(1, 2) }

      MuchStub::Stub.new(@myobj, :myvalargs){ "myvalargs" }
      assert_raises(MuchStub::StubArityError){ @myobj.myvalargs }
      assert_raises(MuchStub::StubArityError){ @myobj.myvalargs(1) }
      assert_nothing_raised{ @myobj.myvalargs(1, 2) }
      assert_nothing_raised{ @myobj.myvalargs(1, nil) }
      assert_nothing_raised{ @myobj.myvalargs(1, 2, 3) }
    end

    should "complain if stubbed with mismatched arity" do
      err =
        assert_raises(MuchStub::StubArityError) do
          MuchStub::Stub.new(@myobj, :myval).with{ "myval" }
        end
      assert_includes "arity mismatch on", err.message
      assert_includes "test/unit/much-stub_tests.rb", err.backtrace.first

      assert_raises(MuchStub::StubArityError) do
        MuchStub::Stub.new(@myobj, :myval).with(1, 2){ "myval" }
      end
      assert_nothing_raised do
        MuchStub::Stub.new(@myobj, :myval).with(1){ "myval" }
      end

      assert_nothing_raised do
        MuchStub::Stub.new(@myobj, :myargs).with{ "myargs" }
      end
      assert_nothing_raised do
        MuchStub::Stub.new(@myobj, :myargs).with(1, 2){ "myargs" }
      end
      assert_nothing_raised do
        MuchStub::Stub.new(@myobj, :myargs).with(1){ "myargs" }
      end

      assert_raises(MuchStub::StubArityError) do
        MuchStub::Stub.new(@myobj, :myvalargs).with{ "myvalargs" }
      end
      assert_raises(MuchStub::StubArityError) do
        MuchStub::Stub.new(@myobj, :myvalargs).with(1){ "myvalargs" }
      end
      assert_nothing_raised do
        MuchStub::Stub.new(@myobj, :myvalargs).with(1, 2){ "myvalargs" }
      end
      assert_nothing_raised do
        MuchStub::Stub.new(@myobj, :myvalargs).with(1, 2, 3){ "myvalargs" }
      end
    end

    should "stub methods with no args" do
      subject.teardown

      assert_equal "meth", @myobj.mymeth
      MuchStub::Stub.new(@myobj, :mymeth){ "mymeth" }
      assert_equal "mymeth", @myobj.mymeth
    end

    should "stub methods with required arg" do
      assert_equal 1, @myobj.myval(1)
      stub = MuchStub::Stub.new(@myobj, :myval, &:to_s)
      assert_equal "1", @myobj.myval(1)
      assert_equal "2", @myobj.myval(2)
      stub.with(2){ "two" }
      assert_equal "two", @myobj.myval(2)
    end

    should "stub methods with variable args" do
      assert_equal [1, 2], @myobj.myargs(1, 2)

      stub = MuchStub::Stub.new(@myobj, :myargs){ |*args| args.join(",") }
      assert_equal "1,2,3", @myobj.myargs(1, 2, 3)
      stub.with(3, 4, 5){ |*args| args.join(":") }
      assert_equal "3:4:5", @myobj.myargs(3, 4, 5)

      stub =
        MuchStub::Stub.new(@myobj, :myargs).on_call do |call|
          call.args.join(",")
        end
      assert_equal "1,2,3", @myobj.myargs(1, 2, 3)
      stub.with(3, 4, 5).on_call{ |call| call.args.join(":") }
      assert_equal "3:4:5", @myobj.myargs(3, 4, 5)
    end

    should "stub methods with required args and variable args" do
      assert_equal [1, 2, [3]], @myobj.myvalargs(1, 2, 3)
      stub = MuchStub::Stub.new(@myobj, :myvalargs){ |*args| args.join(",") }
      assert_equal "1,2,3", @myobj.myvalargs(1, 2, 3)
      stub.with(3, 4, 5){ |*args| args.join(":") }
      assert_equal "3:4:5", @myobj.myvalargs(3, 4, 5)

      stub =
        MuchStub::Stub.new(@myobj, :myvalargs).on_call do |call|
          call.args.join(",")
        end
      assert_equal "1,2,3", @myobj.myvalargs(1, 2, 3)
      stub.with(3, 4, 5).on_call{ |call| call.args.join(":") }
      assert_equal "3:4:5", @myobj.myvalargs(3, 4, 5)
    end

    should "stub methods that yield blocks" do
      blkcalled = false
      blk = proc{ blkcalled = true }
      @myobj.myblk(&blk)
      assert_equal true, blkcalled

      blkcalled = false
      MuchStub::Stub.new(@myobj, :myblk){ blkcalled = "true" }
      @myobj.myblk(&blk)
      assert_equal "true", blkcalled
    end

    should "stub methods even if they are not local to the object" do
      mydelegatorclass =
        Class.new do
          def initialize(delegateclass)
            @delegate = delegateclass.new
          end

          def respond_to?(meth)
            @delegate.respond_to?(meth) || super
          end

          def method_missing(meth, *args, &block)
            respond_to?(meth) ? @delegate.send(meth, *args, &block) : super
          end

          def respond_to_missing?(meth, _)
            respond_to?(meth) || super
          end
        end
      mydelegator = mydelegatorclass.new(@myclass)

      assert_equal [1, 2, [3]], mydelegator.myvalargs(1, 2, 3)
      stub = MuchStub::Stub.new(mydelegator, :myvalargs){ |*args| args.inspect }
      assert_equal "[1, 2, 3]", mydelegator.myvalargs(1, 2, 3)
      assert_equal "[4, 5, 6]", mydelegator.myvalargs(4, 5, 6)
      stub.with(4, 5, 6){ "four-five-six" }
      assert_equal "four-five-six", mydelegator.myvalargs(4, 5, 6)
    end

    should "call to the original method" do
      subject.teardown

      # no args
      stub = MuchStub::Stub.new(@myobj, :mymeth){ "mymeth" }
      assert_equal "mymeth", stub.call
      assert_equal "meth",   stub.call_method

      # static args
      stub = MuchStub::Stub.new(@myobj, :myval, &:to_s)
      assert_equal "1", stub.call(1)
      assert_equal 1,   stub.call_method(1)
      assert_equal "2", stub.call(2)
      assert_equal 2,   stub.call_method(2)
      stub.with(2){ "two" }
      assert_equal "two", stub.call(2)
      assert_equal 2,     stub.call_method(2)

      # dynamic args
      stub = MuchStub::Stub.new(@myobj, :myargs){ |*args| args.join(",") }
      assert_equal "1,2", stub.call(1, 2)
      assert_equal [1, 2], stub.call_method(1, 2)
      assert_equal "3,4,5", stub.call(3, 4, 5)
      assert_equal [3, 4, 5], stub.call_method(3, 4, 5)
      stub.with(3, 4, 5){ "three-four-five" }
      assert_equal "three-four-five", stub.call(3, 4, 5)
      assert_equal [3, 4, 5], stub.call_method(3, 4, 5)

      # mixed static/dynamic args
      stub = MuchStub::Stub.new(@myobj, :myvalargs){ |*args| args.join(",") }
      assert_equal "1,2,3", stub.call(1, 2, 3)
      assert_equal [1, 2, [3]], stub.call_method(1, 2, 3)
      assert_equal "3,4,5", stub.call(3, 4, 5)
      assert_equal [3, 4, [5]], stub.call_method(3, 4, 5)
      stub.with(3, 4, 5){ "three-four-five" }
      assert_equal "three-four-five", stub.call(3, 4, 5)
      assert_equal [3, 4, [5]], stub.call_method(3, 4, 5)

      # keyword args
      stub = MuchStub::Stub.new(@myobj, :mykargs){ |**kargs| kargs.inspect }
      assert_equal "{:val1=>1, :val2=>2}", stub.call(val1: 1, val2: 2)
      assert_equal [1, { val2: 2 }], stub.call_method(val1: 1, val2: 2)
      stub.with(val1: 3, val2: 4){ "three-four" }
      assert_equal "three-four", stub.call(val1: 3, val2: 4)
      assert_equal [3, { val2: 4 }], stub.call_method(val1: 3, val2: 4)

      # blocks
      blkcalled = false
      blk = proc{ blkcalled = true }
      stub = MuchStub::Stub.new(@myobj, :myblk){ blkcalled = "true" }
      stub.call(&blk)
      assert_equal "true", blkcalled
      stub.call_method(&blk)
      assert_equal true, blkcalled
    end

    should "store and remove itself" do
      assert_equal subject, MuchStub.instance_variable_get(subject.ivar_name)
      subject.teardown
      assert_nil MuchStub.instance_variable_get(subject.ivar_name)
    end

    should "be removable" do
      assert_equal 1, @myobj.myval(1)
      stub = MuchStub::Stub.new(@myobj, :myval, &:to_s)
      assert_equal "1", @myobj.myval(1)
      stub.teardown
      assert_equal 1, @myobj.myval(1)
    end

    should "have a readable inspect" do
      exp =
        "#<#{subject.class}:#{format("0x0%x", (subject.object_id << 1))}" \
        " @method_name=#{subject.method_name.inspect}>"
      assert_equal exp, subject.inspect
    end
  end

  class MutatingArgsStubTests < StubTests
    desc "with args that are mutated after they've been used to stub"
    setup do
      @arg = ChangeHashObject.new

      @stub = MuchStub::Stub.new(@myobj, :myval)
      @stub.with(@arg){ true }

      @arg.change!
    end
    subject{ @stub }

    should "not raise a stub error when called" do
      assert_nothing_raised{ @stub.call(@arg) }
    end
  end

  class StubInheritedMethodAfterStubbedOnParentTests < StubTests
    desc "stubbing an inherited method after its stubbed on the parent class"
    setup do
      @parent_class = Class.new
      @child_class = Class.new(@parent_class)

      @parent_stub = MuchStub::Stub.new(@parent_class, :new)
      @child_stub = MuchStub::Stub.new(@child_class, :new)
    end

    should "allow stubbing them independently of each other" do
      assert_nothing_raised do
        @parent_stub.with(1, 2){ "parent" }
        @child_stub.with(1, 2){ "child" }
      end
      assert_equal "parent", @parent_class.new(1, 2)
      assert_equal "child", @child_class.new(1, 2)
    end

    should "not raise any errors when tearing down the parent before the "\
           "child" do
      assert_nothing_raised do
        @parent_stub.teardown
        @child_stub.teardown
      end
    end
  end

  class NullStubTests < UnitTests
    desc "NullStub"
    setup do
      @ns = NullStub.new
    end
    subject{ @ns }

    should have_imeths :teardown
  end

  class ParameterListTests < UnitTests
    desc "ParameterList"
    setup do
      many_args = (0..ParameterList::LETTERS.size).map{
        Assert::Factory.string
      }.join(", ")
      @object = Class.new
      # use `class_eval` with string to easily define methods with many params
      @object.class_eval <<-methods
        def self.noargs; end
        def self.anyargs(*args); end
        def self.manyargs(#{many_args}); end
        def self.minargs(#{many_args}, *args); end
      methods
    end
    subject{ ParameterList }

    should "build a parameter list for a method that takes no args" do
      assert_equal "&block", subject.new(@object, "noargs")
    end

    should "build a parameter list for a method that takes any args" do
      assert_equal "*pargs, **kargs, &block", subject.new(@object, "anyargs")
    end

    should "build a parameter list for a method that takes many args" do
      expected = "#{ParameterList::LETTERS.join(", ")}, aa, &block"
      assert_equal expected, subject.new(@object, "manyargs")
    end

    should "build a parameter list for a method that takes a minimum number "\
           "of args" do
      expected =
        "#{ParameterList::LETTERS.join(", ")}, aa, *pargs, **kargs, &block"
      assert_equal expected, subject.new(@object, "minargs")
    end
  end

  class ChangeHashObject
    def initialize
      @value = nil
    end

    def hash
      @value.hash
    end

    def change!
      @value = 1
    end
  end
end
