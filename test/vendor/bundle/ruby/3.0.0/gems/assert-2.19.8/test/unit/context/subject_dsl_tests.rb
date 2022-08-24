# frozen_string_literal: true

require "assert"
require "assert/context/subject_dsl"

module Assert::Context::SubjectDSL
  class UnitTests < Assert::Context
    desc "Assert::Context::SubjectDSL"
    subject{ Factory.modes_off_context_class(parent_class1) }

    let(:parent_class1){ Factory.modes_off_context_class }
    let(:subject_block1){ Proc.new{} }
  end

  class DescriptionTests < UnitTests
    desc "`description` method"

    should "return a string of all the inherited descriptions" do
      parent_class1.desc("parent description")
      subject.desc("and the description for this context")

      exp = "parent description and the description for this context"
      assert_that(subject.description).equals(exp)
    end
  end

  class SubjectFromLocalTests < UnitTests
    desc "`subject` method using local context"

    should "set the subject block on the context class" do
      subject.subject(&subject_block1)

      assert_that(subject.subject).equals(subject_block1)
    end
  end

  class SubjectFromParentTests < UnitTests
    desc "`subject` method using parent context"

    should "default to its parents subject block" do
      parent_class1.subject(&subject_block1)

      assert_that(subject.subject).equals(subject_block1)
    end
  end
end
