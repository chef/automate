# frozen_string_literal: true

require "assert"
require "assert/macro"

class Assert::Macro
  class UnitTests < Assert::Context
    desc "Assert::Macro"
    subject{ unit_class }

    let(:unit_class){ Assert::Macro }
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new{} }

    should "have an accessor for its (optional) name" do
      assert_that(subject).responds_to(:name)
      assert_that(subject).responds_to(:name=)
    end

    should "default its name if no given" do
      assert_that((unit_class.new{}).name).equals("run this macro")
    end

    should "initialize with a given name" do
      assert_that((unit_class.new("test"){}).name).equals("test")
    end

    should "be a Proc" do
      assert_that(subject).is_kind_of(::Proc)
    end

    should "complain if you create a macro without a block" do
      assert_that{ unit_class.new }.raises(ArgumentError)
    end
  end

  class InstanceMethodsTests < Assert::Context
    desc "have_instance_methods macro: this class"
    subject do
      class ::InstExample
        (1..6).each{ |i| define_method("method_#{i}"){} }
      end
      ::InstExample.new
    end

    should have_instance_method :method_1
    should have_instance_methods :method_2, :method_3
    should have_imeth :method_4
    should have_imeths :method_5, :method_6

    should not_have_instance_method :method_7
    should not_have_instance_methods :method_8, :method_9
    should not_have_imeth :method_10
    should not_have_imeths :method_11, :method_12
  end

  class ClassMethodsTests < Assert::Context
    desc "have_class_methods macro: this class"
    subject do
      class ::ClassExample
        class << self
          (1..6).each{ |i| define_method("method_#{i}"){} }
        end
      end
      ::ClassExample.new
    end

    should have_class_method :method_1
    should have_class_methods :method_2, :method_3
    should have_cmeth :method_4
    should have_cmeths :method_5, :method_6

    should not_have_class_method :method_7
    should not_have_class_methods :method_8, :method_9
    should not_have_cmeth :method_10
    should not_have_cmeths :method_11, :method_12
  end

  class ReadersTests < Assert::Context
    desc "have_readers macro: this class"
    subject do
      class ::ReaderExample
        (1..6).each{ |i| attr_reader "method_#{i}" }
      end
      ::ReaderExample.new
    end

    should have_reader :method_1
    should have_reader :method_2, :method_3
    should have_readers :method_4
    should have_readers :method_5, :method_6

    should not_have_reader :method_7
    should not_have_reader :method_8, :method_9
    should not_have_readers :method_10
    should not_have_readers :method_11, :method_12
  end

  class WritersTests < Assert::Context
    desc "have_writers macro: this class"
    subject do
      class ::WriterExample
        (1..6).each{ |i| attr_writer "method_#{i}" }
      end
      ::WriterExample.new
    end

    should have_writer :method_1
    should have_writer :method_2, :method_3
    should have_writers :method_4
    should have_writers :method_5, :method_6

    should not_have_writer :method_7
    should not_have_writer :method_8, :method_9
    should not_have_writers :method_10
    should not_have_writers :method_11, :method_12
  end

  class AccessorsTests < Assert::Context
    desc "have_accessors macro: this class"
    subject do
      class ::AccessorExample
        (1..6).each{ |i| attr_accessor "method_#{i}" }
      end
      ::AccessorExample.new
    end

    should have_accessor :method_1
    should have_accessor :method_2, :method_3
    should have_accessors :method_4
    should have_accessors :method_5, :method_6

    should not_have_accessor :method_7
    should not_have_accessor :method_8, :method_9
    should not_have_accessors :method_10
    should not_have_accessors :method_11, :method_12
  end
end
