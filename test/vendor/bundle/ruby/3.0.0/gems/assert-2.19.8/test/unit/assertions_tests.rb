# frozen_string_literal: true

require "assert"
require "assert/assertions"

module Assert::Assertions
  class UnitTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "Assert::Context"
    subject{ context_class1.new(test1, test1.config, proc{ |r| }) }

    let(:context_class1){ Factory.modes_off_context_class }
    let(:test1){ Factory.test }

    should have_imeths :assert_block, :assert_not_block, :refute_block
    should have_imeths :assert_empty, :assert_not_empty, :refute_empty
    should have_imeths :assert_equal, :assert_not_equal, :refute_equal
    should have_imeths :assert_file_exists, :assert_not_file_exists
    should have_imeths :refute_file_exists
    should have_imeths :assert_includes, :assert_not_includes
    should have_imeths :assert_included, :assert_not_included
    should have_imeths :refute_includes, :refute_included
    should have_imeths :assert_instance_of, :assert_not_instance_of
    should have_imeths :refute_instance_of
    should have_imeths :assert_is_a, :assert_is_not_a, :assert_not_a
    should have_imeths :refute_is_a
    should have_imeths :assert_kind_of, :assert_not_kind_of, :refute_kind_of
    should have_imeths :assert_match, :assert_not_match, :assert_no_match
    should have_imeths :refute_match
    should have_imeths :assert_nil, :assert_not_nil, :refute_nil
    should have_imeths :assert_true, :assert_not_true, :refute_true
    should have_imeths :assert_false, :assert_not_false, :refute_false
    should have_imeths :assert_raises, :assert_not_raises
    should have_imeths :assert_raise, :assert_not_raise, :assert_nothing_raised
    should have_imeths :assert_changes, :assert_not_changes, :refute_changes
    should have_imeths :assert_respond_to, :assert_responds_to
    should have_imeths :assert_not_respond_to, :assert_not_responds_to
    should have_imeths :refute_respond_to, :refute_responds_to
    should have_imeths :assert_same, :assert_not_same, :refute_same
  end

  class IgnoredTests < UnitTests
    desc "ignored assertions helpers"
    setup do
      tests.each{ |test| test.run(&test_run_callback) }
    end

    let(:tests) do
      context_info = Factory.context_info(context_class1)
      Assert::Assertions::IGNORED_ASSERTION_HELPERS.map do |helper|
        Factory.test("ignored assertion helper #{helper}", context_info) do
          send(helper, "doesn't matter")
        end
      end
    end

    should "have an ignored result for each helper in the constant" do
      exp = Assert::Assertions::IGNORED_ASSERTION_HELPERS.size
      assert_that(test_run_result_count).equals(exp)

      test_run_results.each do |result|
        assert_that(result).is_kind_of(Assert::Result::Ignore)
      end
    end

    should "have a custom ignore message for each helper in the constant" do
      exp =
        Assert::Assertions::IGNORED_ASSERTION_HELPERS.map do |helper|
          "The assertion `#{helper}` is not supported."\
          " Please use another assertion or the basic `assert`."
        end
      assert_that(test_run_results.map(&:message)).equals(exp)
    end
  end
end
