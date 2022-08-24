# frozen_string_literal: true

require "assert"

class Assert::Test
  class SystemTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "Assert::Test"

    setup do
      subject.run(&test_run_callback)
    end
  end

  class NoResultsTests < SystemTests
    desc "when producing no results"
    subject{ Factory.test }

    should "generate 0 results" do
      assert_that(test_run_result_count).equals(0)
    end
  end

  class PassTests < SystemTests
    desc "when passing a single assertion"
    subject do
      Factory.test do
        assert(
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
      end
    end

    should "generate 1 result" do
      assert_that(test_run_result_count).equals(1)
    end

    should "generate 1 pass result" do
      assert_that(test_run_result_count(:pass)).equals(1)
    end
  end

  class FailTests < SystemTests
    desc "when failing a single assertion"
    subject{ Factory.test{ assert(1 == 0) } }

    should "generate 1 result" do
      assert_that(test_run_result_count).equals(1)
    end

    should "generate 1 fail result" do
      assert_that(test_run_result_count(:fail)).equals(1)
    end
  end

  class SkipTests < SystemTests
    desc "when skipping once"
    subject{ Factory.test{ skip } }

    should "generate 1 result" do
      assert_that(test_run_result_count).equals(1)
    end

    should "generate 1 skip result" do
      assert_that(test_run_result_count(:skip)).equals(1)
    end
  end

  class ErrorTests < SystemTests
    desc "when erroring once"
    subject{ Factory.test{ raise("WHAT") } }

    should "generate 1 result" do
      assert_that(test_run_result_count).equals(1)
    end

    should "generate 1 error result" do
      assert_that(test_run_result_count(:error)).equals(1)
    end
  end

  class MixedTests < SystemTests
    desc "when passing 1 assertion and failing 1 assertion"
    subject do
      Factory.test do
        assert(
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
        assert(1 == 0)
      end
    end

    should "generate 2 total results" do
      assert_that(test_run_result_count).equals(2)
    end

    should "generate 1 pass result" do
      assert_that(test_run_result_count(:pass)).equals(1)
    end

    should "generate 1 fail result" do
      assert_that(test_run_result_count(:fail)).equals(1)
    end
  end

  class MixedSkipTests < SystemTests
    desc "when passing 1 assertion and failing 1 assertion with a skip call "\
         "in between"
    subject do
      Factory.test do
        assert(
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
        skip
        assert(1 == 0)
      end
    end

    should "generate 2 total results" do
      assert_that(test_run_result_count).equals(2)
    end

    should "generate a skip for its last result" do
      assert_that(last_test_run_result).is_kind_of(Assert::Result::Skip)
    end

    should "generate 1 pass result" do
      assert_that(test_run_result_count(:pass)).equals(1)
    end

    should "generate 1 skip result" do
      assert_that(test_run_result_count(:skip)).equals(1)
    end

    should "generate 0 fail results" do
      assert_that(test_run_result_count(:fail)).equals(0)
    end
  end

  class MixedErrorTests < SystemTests
    desc "when passing 1 assertion and failing 1 assertion with an exception "\
         "raised in between"
    subject do
      Factory.test do
        assert(
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
        raise "something errored"
        assert(1 == 0) # rubocop:disable Lint/UnreachableCode
      end
    end

    should "generate 2 total results" do
      assert_that(test_run_result_count).equals(2)
    end

    should "generate an error for its last result" do
      assert_that(last_test_run_result).is_kind_of(Assert::Result::Error)
    end

    should "generate 1 pass result" do
      assert_that(test_run_result_count(:pass)).equals(1)
    end

    should "generate 1 error result" do
      assert_that(test_run_result_count(:error)).equals(1)
    end

    should "generate 0 fail results" do
      assert_that(test_run_result_count(:fail)).equals(0)
    end
  end

  class MixedPassTests < SystemTests
    desc "when passing 1 assertion and failing 1 assertion with a pass call "\
         "in between"
    subject do
      Factory.test do
        assert(
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
        pass
        assert(1 == 0)
      end
    end

    should "generate 3 total results" do
      assert_that(test_run_result_count).equals(3)
    end

    should "generate a fail for its last result" do
      assert_that(last_test_run_result).is_kind_of(Assert::Result::Fail)
    end

    should "generate 2 pass results" do
      assert_that(test_run_result_count(:pass)).equals(2)
    end

    should "generate 1 fail result" do
      assert_that(test_run_result_count(:fail)).equals(1)
    end
  end

  class MixedFailTests < SystemTests
    desc "when failing 1 assertion and passing 1 assertion with a fail call "\
         "in between"
    subject do
      Factory.test do
        assert(1 == 0)
        fail # rubocop:disable Style/SignalException
        assert( # rubocop:disable Lint/UnreachableCode
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
      end
    end

    should "generate 3 total results" do
      assert_that(test_run_result_count).equals(3)
    end

    should "generate a pass for its last result" do
      assert_that(last_test_run_result).is_kind_of(Assert::Result::Pass)
    end

    should "generate 1 pass result" do
      assert_that(test_run_result_count(:pass)).equals(1)
    end

    should "generate 2 fail results" do
      assert_that(test_run_result_count(:fail)).equals(2)
    end
  end

  class MixedFlunkTests < SystemTests
    desc "has failing 1 assertion and passing 1 assertion with a flunk call "\
         "in between"
    subject do
      Factory.test do
        assert(1 == 0)
        flunk
        assert(
          1 == 1, # rubocop:disable Lint/BinaryOperatorWithIdenticalOperands
        )
      end
    end

    should "generate 3 total results" do
      assert_that(test_run_result_count).equals(3)
    end

    should "generate a pass for its last result" do
      assert_that(last_test_run_result).is_kind_of(Assert::Result::Pass)
    end

    should "generate 1 pass results" do
      assert_that(test_run_result_count(:pass)).equals(1)
    end

    should "generate 2 fail results" do
      assert_that(test_run_result_count(:fail)).equals(2)
    end
  end

  class WithSetupsTests < SystemTests
    desc "that has setup logic"
    subject do
      Factory.test("t", Factory.context_info(context_class1)){ pass "TEST" }
    end

    let(:context_class1) do
      Factory.context_class do
        # assert style
        setup{ pass "assert style setup" }
        # test/unit style
        def setup
          pass "test/unit style setup"
        end
      end
    end

    should "execute all setup logic when run" do
      assert_that(test_run_result_count(:pass)).equals(3)

      exp = ["assert style setup", "test/unit style setup", "TEST"]
      assert_that(test_run_result_messages).equals(exp)
    end
  end

  class WithTeardownsTests < SystemTests
    desc "that has teardown logic"
    subject do
      Factory.test("t", Factory.context_info(context_class1)){ pass "TEST" }
    end

    let(:context_class1) do
      Factory.context_class do
        # assert style
        teardown{ pass "assert style teardown" }
        # test/unit style
        def teardown
          pass "test/unit style teardown"
        end
      end
    end

    should "execute all teardown logic when run" do
      assert_that(test_run_result_count(:pass)).equals(3)

      exp = ["TEST", "assert style teardown", "test/unit style teardown"]
      assert_that(test_run_result_messages).equals(exp)
    end
  end

  class WithAroundsTests < SystemTests
    desc "that has around logic (in addition to setups/teardowns)"
    subject do
      Factory.test("t", Factory.context_info(context_class1)){ pass "TEST" }
    end

    let(:parent_context_class1) do
      Factory.modes_off_context_class do
        around do |block|
          pass "parent around start"
          block.call
          pass "parent around end"
        end
        setup{ pass "parent setup" }
        teardown{ pass "parent teardown" }
      end
    end
    let(:context_class1) do
      Factory.modes_off_context_class(parent_context_class1) do
        setup{ pass "child setup1" }
        around do |block|
          pass "child around1 start"
          block.call
          pass "child around1 end"
        end
        teardown{ pass "child teardown1" }
        setup{ pass "child setup2" }
        around do |block|
          pass "child around2 start"
          block.call
          pass "child around2 end"
        end
        teardown{ pass "child teardown2" }
      end
    end

    should "run the arounds outside of the setups/teardowns/test" do
      assert_that(test_run_result_count(:pass)).equals(13)

      exp = [
        "parent around start", "child around1 start", "child around2 start",
        "parent setup", "child setup1", "child setup2", "TEST",
        "child teardown1", "child teardown2", "parent teardown",
        "child around2 end", "child around1 end", "parent around end",
      ]
      assert_that(test_run_result_messages).equals(exp)
    end
  end
end
