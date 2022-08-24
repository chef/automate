# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertRespondToTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_respond_to`"
    subject do
      args = args1
      Factory.test do
        assert_respond_to(:abs, 1) # pass
        assert_respond_to(*args)   # fail
      end
    end

    let(:desc1){ "assert respond to fail desc" }
    let(:args1){ [:abs, "1", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\n"\
        "Expected #{Assert::U.show(args1[1], config1)} (#{args1[1].class})"\
        " to respond to `#{args1[0]}`."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotRespondToTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_respond_to`"
    subject do
      args = args1
      Factory.test do
        assert_not_respond_to(*args)     # fail
        assert_not_respond_to(:abs, "1") # pass
      end
    end

    let(:desc1){ "assert not respond to fail desc" }
    let(:args1){ [:abs, 1, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\n"\
        "Expected #{Assert::U.show(args1[1], config1)} (#{args1[1].class})"\
        " to not respond to `#{args1[0]}`."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
