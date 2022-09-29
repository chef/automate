# frozen_string_literal: true

require "assert"
require "assert/assertions"

module Assert::Assertions
  class AssertChangesTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_changes`"
    subject do
      desc = desc1
      Factory.test do
        @my_var = 1
        assert_changes("@my_var", from: 1, to: 2){ @my_var = 2 } # pass
        @my_var = 1
        assert_changes("@my_var", from: 1){ @my_var = 2 }        # pass
        @my_var = 1
        assert_changes("@my_var", to: 2){ @my_var = 2 }          # pass
        @my_var = 1
        assert_changes("@my_var", desc: desc){ @my_var = 2 }     # pass

        @my_var = 1
        assert_changes("@my_var", from: 2, to: 1){ @my_var = 2 } # fail
        @my_var = 1
        assert_changes("@my_var", from: 2){ @my_var = 2 }        # fail
        @my_var = 1
        assert_changes("@my_var", to: 1){ @my_var = 2 }          # fail
        @my_var = 1
        assert_changes("@my_var", desc: desc){ @my_var = 1 }     # fail
      end
    end

    let(:desc1){ "assert changes fail desc" }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(10)
      assert_that(test_run_result_count(:pass)).equals(5)
      assert_that(test_run_result_count(:fail)).equals(5)

      exp =
        [
          "Expected `@my_var` to change from `2`",
          "Expected `@my_var` to change to `1`",
          "Expected `@my_var` to change from `2`",
          "Expected `@my_var` to change to `1`",
          "#{desc1}\nExpected `@my_var` to change; "\
          "it was `1` and didn't change",
        ]
      messages = test_run_results(:fail).map(&:message)
      messages.each_with_index do |msg, n|
        assert_that(msg).matches(/^#{exp[n]}/)
      end
    end
  end

  class AssertNotChangesTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_changes`"
    subject do
      desc = desc1
      Factory.test do
        @my_var = 1
        assert_not_changes("@my_var", from: 1){ @my_var = 1 }    # pass
        @my_var = 1
        assert_not_changes("@my_var", desc: desc){ @my_var = 1 } # pass

        @my_var = 1
        assert_not_changes("@my_var", from: 2){ @my_var = 1 }    # fail
        @my_var = 1
        assert_not_changes("@my_var", from: 1){ @my_var = 2 }    # fail
        @my_var = 1
        assert_not_changes("@my_var", desc: desc){ @my_var = 2 } # fail
      end
    end

    let(:desc1){ "assert not changes fail desc" }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(8)
      assert_that(test_run_result_count(:pass)).equals(5)
      assert_that(test_run_result_count(:fail)).equals(3)

      exp =
        [
          "Expected `@my_var` to not change from `2`",
          "Expected `@my_var` to not change; "\
          "it was `1` and changed to `2`",
          "#{desc1}\nExpected `@my_var` to not change; "\
          "it was `1` and changed to `2`",
        ]
      messages = test_run_results(:fail).map(&:message)
      messages.each_with_index do |msg, n|
        assert_that(msg).matches(/^#{exp[n]}/)
      end
    end
  end
end
