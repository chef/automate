# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertNilTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_nil`"
    subject do
      args = args1
      Factory.test do
        assert_nil(nil)   # pass
        assert_nil(*args) # fail
      end
    end

    let(:desc1){ "assert nil empty fail desc" }
    let(:args1){ [1, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "be nil."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotNilTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_nil`"
    subject do
      args = args1
      Factory.test do
        assert_not_nil(1)     # pass
        assert_not_nil(*args) # fail
      end
    end

    let(:desc1){ "assert not nil empty fail desc" }
    let(:args1){ [nil, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "not be nil."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
