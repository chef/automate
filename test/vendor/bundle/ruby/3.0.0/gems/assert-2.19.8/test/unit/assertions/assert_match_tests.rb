# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertMatchTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_match`"
    subject do
      args = args1
      Factory.test do
        assert_match(/a/, "a string") # pass
        assert_match(*args)           # fail
      end
    end

    let(:desc1){ "assert match fail desc" }
    let(:args1){ ["not", "a string", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\nExpected #{Assert::U.show(args1[1], config1)}"\
        " to match #{Assert::U.show(args1[0], config1)}."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotMatchTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_match`"
    subject do
      args = args1
      Factory.test do
        assert_not_match(*args)             # fail
        assert_not_match("not", "a string") # pass
      end
    end

    let(:desc1){ "assert not match fail desc" }
    let(:args1){ [/a/, "a string", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\nExpected #{Assert::U.show(args1[1], config1)}"\
        " to not match #{Assert::U.show(args1[0], config1)}."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
