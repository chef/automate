# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertEmptyTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_empty`"
    subject do
      args = args1
      Factory.test do
        assert_empty([])    # pass
        assert_empty(*args) # fail
      end
    end

    let(:desc1){ "assert empty fail desc" }
    let(:args1){ [[1], desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to be "\
        "empty."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotEmptyTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_empty`"
    subject do
      args = args1
      Factory.test do
        assert_not_empty([1])   # pass
        assert_not_empty(*args) # fail
      end
    end

    let(:desc1){ "assert not empty fail desc" }
    let(:args1){ [[], desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to not "\
        "be empty."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
