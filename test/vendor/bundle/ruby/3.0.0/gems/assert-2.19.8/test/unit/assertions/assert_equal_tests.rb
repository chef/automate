# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertEqualTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_equal`"
    subject do
      args = args1
      Factory.test do
        assert_equal(1, 1)  # pass
        assert_equal(*args) # fail
      end
    end

    let(:desc1){ "assert equal fail desc" }
    let(:args1){ ["1", "2", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\nExpected #{Assert::U.show(args1[1], config1)}"\
        " to be equal to #{Assert::U.show(args1[0], config1)}."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotEqualTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_equal`"
    subject do
      args = args1
      Factory.test do
        assert_not_equal(*args) # fail
        assert_not_equal(1, 2)  # pass
      end
    end

    let(:desc1){ "assert not equal fail desc" }
    let(:args1){ ["1", "1", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\nExpected #{Assert::U.show(args1[1], config1)}"\
        " to not be equal to #{Assert::U.show(args1[0], config1)}."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class EqualOrderTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "with objects that define custom equality operators"

    let(:is_class) do
      Class.new do
        def ==(other)
          if other.is_a?(Assert::ActualValue.not_given.class)
            super
          else
            true
          end
        end
      end
    end
    let(:is_not_class) do
      Class.new do
        def ==(other)
          if other.is_a?(Assert::ActualValue.not_given.class)
            super
          else
            false
          end
        end
      end
    end

    let(:is1){ is_class.new }
    let(:is_not1){ is_not_class.new }

    should "use the equality operator of the exp value" do
      assert_that(is1).equals(is_not1)
      assert_that(is_not1).does_not_equal(is1)
    end
  end

  class DiffTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "with objects that should use diff when showing"

    let(:config1) do
      Factory.modes_off_config.tap do |config|
        config.use_diff_proc(Assert::U.default_use_diff_proc)
        config.run_diff_proc(Assert::U.syscmd_diff_proc)
      end
    end

    let(:exp_obj1){ "I'm a\nstring" }
    let(:act_obj1){ "I am a \nstring" }
    let(:exp_obj_show1){ Assert::U.show_for_diff(exp_obj1, config1) }
    let(:act_obj_show1){ Assert::U.show_for_diff(act_obj1, config1) }
  end

  class AssertEqualDiffTests < DiffTests
    desc "`assert_equal`"
    subject do
      exp_obj, act_obj = exp_obj1, act_obj1
      Factory.test(config1) do
        assert_equal(exp_obj, act_obj)
      end
    end

    should "include diff output in the fail messages" do
      subject.run(&test_run_callback)

      exp =
        "Expected does not equal actual, diff:\n"\
        "#{Assert::U.syscmd_diff_proc.call(exp_obj_show1, act_obj_show1)}"
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotEqualDiffTests < DiffTests
    desc "`assert_not_equal`"
    subject do
      exp_obj = exp_obj1
      Factory.test(config1) do
        assert_not_equal(exp_obj, exp_obj)
      end
    end

    should "include diff output in the fail messages" do
      subject.run(&test_run_callback)

      exp =
        "Expected equals actual, diff:\n"\
        "#{Assert::U.syscmd_diff_proc.call(exp_obj_show1, exp_obj_show1)}"
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
