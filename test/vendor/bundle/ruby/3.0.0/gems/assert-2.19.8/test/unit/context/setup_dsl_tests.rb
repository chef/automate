# frozen_string_literal: true

require "assert"
require "assert/context/setup_dsl"

module Assert::Context::SetupDSL
  class UnitTests < Assert::Context
    desc "Assert::Context::SetupDSL"
    subject{ Factory.modes_off_context_class }

    let(:block1){ ::Proc.new{} }
  end

  class SetupTeardownOnceMethodsTests < UnitTests
    desc "once methods"

    should "add the block to the suite" do
      subject.setup_once(&block1)
      subject.teardown_once(&block1)

      assert_that(subject.suite.send(:setups)).includes(block1)
      assert_that(subject.suite.send(:teardowns)).includes(block1)
    end
  end

  class SetupTeardownMethodsTests < UnitTests
    desc "methods"

    should "add the block to the context" do
      subject.setup(&block1)
      subject.teardown(&block1)

      assert_that(subject.send(:setups)).includes(block1)
      assert_that(subject.send(:teardowns)).includes(block1)
    end
  end

  class SetupTeardownWithMethodNameTests < UnitTests
    desc "methods given a method name"

    let(:method_name1){ :something_amazing }

    should "add the method name to the context" do
      subject.setup(method_name1)
      subject.teardown(method_name1)

      assert_that(subject.send(:setups)).includes(method_name1)
      assert_that(subject.send(:teardowns)).includes(method_name1)
    end
  end

  class ParentContextClassTests < UnitTests
    subject{ Factory.modes_off_context_class(parent_class1) }

    let(:parent_class1){ Factory.modes_off_context_class }
  end

  class SetupTeardownMultipleTests < ParentContextClassTests
    desc "with multiple calls"

    let(:parent_setup_block1) do
      ::Proc.new{ self.setup_status = "the setup" }
    end
    let(:parent_teardown_block1) do
      ::Proc.new{ self.teardown_status += "the teardown" }
    end
    let(:context_setup_block1) do
      ::Proc.new{ self.setup_status += " has been run" }
    end
    let(:context_teardown_block1) do
      ::Proc.new{ self.teardown_status += "has been run " }
    end

    let(:test_status_class) do
      Class.new do
        attr_accessor :setup_status, :teardown_status
        define_method(:setup_something) do
          self.setup_status += " with something"
        end
        define_method(:teardown_something) do
          self.teardown_status = "with something "
        end
      end
    end

    should "run its parent and its own blocks in the correct order" do
      parent_class1.setup(&parent_setup_block1)
      parent_class1.teardown(&parent_teardown_block1)
      subject.setup(&context_setup_block1)
      subject.setup(:setup_something)
      subject.teardown(:teardown_something)
      subject.teardown(&context_teardown_block1)

      subject.send("run_setups", obj = test_status_class.new)
      assert_that(obj.setup_status)
        .equals("the setup has been run with something")

      subject.send("run_teardowns", obj = test_status_class.new)
      assert_that(obj.teardown_status)
        .equals("with something has been run the teardown")
    end
  end

  class AroundMethodTests < ParentContextClassTests
    desc "with multiple `around` calls"

    let(:test_status_class){ Class.new{ attr_accessor :out_status } }

    should "run its parent and its own blocks in the correct order" do
      parent_class1.around do |block|
        self.out_status ||= ""
        self.out_status += "p-around start, "
        block.call
        self.out_status += "p-around end."
      end

      subject.around do |block|
        self.out_status += "c-around1 start, "
        block.call
        self.out_status += "c-around1 end, "
      end
      subject.around do |block|
        self.out_status += "c-around2 start, "
        block.call
        self.out_status += "c-around2 end, "
      end

      obj = test_status_class.new
      subject.send("run_arounds", obj) do
        obj.instance_eval{ self.out_status += "TEST, " }
      end

      exp =
        "p-around start, c-around1 start, c-around2 start, "\
        "TEST, "\
        "c-around2 end, c-around1 end, p-around end."
      assert_that(obj.out_status).equals(exp)
    end
  end
end
