# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertTrueTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_true`"
    subject do
      args = args1
      Factory.test do
        assert_true(true)  # pass
        assert_true(*args) # fail
      end
    end

    let(:desc1){ "assert true fail desc" }
    let(:args1){ ["whatever", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "be true."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotTrueTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_true`"
    subject do
      args = args1
      Factory.test do
        assert_not_true(false) # pass
        assert_not_true(*args) # fail
      end
    end

    let(:desc1){ "assert not true fail desc" }
    let(:args1){ [true, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "not be true."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertFalseTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_false`"
    subject do
      args = args1
      Factory.test do
        assert_false(false) # pass
        assert_false(*args) # fail
      end
    end

    let(:desc1){ "assert false fail desc" }
    let(:args1){ ["whatever", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "be false."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotFalseTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_false`"
    subject do
      args = args1
      Factory.test do
        assert_not_false(true)  # pass
        assert_not_false(*args) # fail
      end
    end

    let(:desc1){ "assert not false fail desc" }
    let(:args1){ [false, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "not be false."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
