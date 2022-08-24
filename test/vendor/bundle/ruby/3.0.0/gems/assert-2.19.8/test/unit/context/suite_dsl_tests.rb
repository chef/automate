# frozen_string_literal: true

require "assert"
require "assert/context/suite_dsl"

require "assert/suite"

module Assert::Context::SuiteDSL
  class UnitTests < Assert::Context
    desc "Assert::Context::SuiteDSL"
    subject{ Factory.context_class(parent_class1) }

    let(:parent_class1){ Factory.context_class }
    let(:custom_suite1){ Factory.modes_off_suite }

    should "use `Assert.suite` by default" do
      assert_that(subject.suite).equals(Assert.suite)
    end

    should "use any given custom suite" do
      subject.suite(custom_suite1)
      assert_that(subject.suite).equals(custom_suite1)
    end
  end

  class SuiteFromParentTests < UnitTests
    desc "`suite` method using parent context"

    setup do
      parent_class1.suite(custom_suite1)
    end

    let(:custom_suite2){ Factory.modes_off_suite }

    should "default to its parent's suite" do
      assert_that(subject.suite).equals(custom_suite1)
    end

    should "use any given custom suite" do
      subject.suite(custom_suite2)
      assert_that(subject.suite).equals(custom_suite2)
    end
  end
end
