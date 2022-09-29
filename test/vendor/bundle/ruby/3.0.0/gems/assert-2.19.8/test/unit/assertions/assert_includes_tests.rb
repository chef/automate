# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertIncludesTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_includes`"
    subject do
      args = args1
      Factory.test do
        assert_includes(1, [1]) # pass
        assert_includes(*args)  # fail
      end
    end

    let(:desc1){ "assert includes fail desc" }
    let(:args1){ [2, [1], desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\n"\
        "Expected #{Assert::U.show(args1[1], config1)}"\
        " to include #{Assert::U.show(args1[0], config1)}."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotIncludedTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_included`"
    subject do
      args = args1
      Factory.test do
        assert_not_included(2, [1]) # pass
        assert_not_included(*args)  # fail
      end
    end

    let(:desc1){ "assert not included fail desc" }
    let(:args1){ [1, [1], desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\n"\
        "Expected #{Assert::U.show(args1[1], config1)}"\
        " to not include #{Assert::U.show(args1[0], config1)}."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
