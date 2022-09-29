# frozen_string_literal: true

require "assert"
require "assert/assertions"

module Assert::Assertions
  class AssertBlockTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_block`"
    subject do
      desc = desc1
      Factory.test do
        assert_block{ true }        # pass
        assert_block(desc){ false } # fail
      end
    end

    let(:desc1){ "assert block fail desc" }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp = "#{desc1}\nExpected block to return a true value."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotBlockTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_block`"
    subject do
      desc = desc1
      Factory.test do
        assert_not_block(desc){ true } # fail
        assert_not_block{ false }      # pass
      end
    end

    let(:desc1){ "assert not block fail desc" }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp = "#{desc1}\nExpected block to not return a true value."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
