# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertSameTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_same`"
    subject do
      args   = args1
      object = object1
      Factory.test do
        assert_same(object, object) # pass
        assert_same(*args)          # fail
      end
    end

    let(:class1){ Class.new }
    let(:object1){ class1.new }
    let(:desc1){ "assert same fail desc" }
    let(:args1){ [object1, class1.new, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\n"\
        "Expected #{Assert::U.show(args1[1], config1)}"\
        " (#<#{args1[1].class}:#{"0x0%x" % (args1[1].object_id << 1)}>)"\
        " to be the same as #{Assert::U.show(args1[0], config1)}"\
        " (#<#{args1[0].class}:#{"0x0%x" % (args1[0].object_id << 1)}>)."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotSameTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_same`"
    subject do
      args   = args1
      object = object1
      klass  = class1
      Factory.test do
        assert_not_same(*args)             # fail
        assert_not_same(object, klass.new) # pass
      end
    end

    let(:class1){ Class.new }
    let(:object1){ class1.new }
    let(:desc1){ "assert not same fail desc" }
    let(:args1){ [object1, object1, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[2]}\n"\
        "Expected #{Assert::U.show(args1[1], config1)}"\
        " (#<#{args1[1].class}:#{"0x0%x" % (args1[1].object_id << 1)}>)"\
        " to not be the same as #{Assert::U.show(args1[0], config1)}"\
        " (#<#{args1[0].class}:#{"0x0%x" % (args1[0].object_id << 1)}>)."
      assert_that(test_run_results(:fail).first.message).equals(exp)
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

  class AssertSameDiffTests < DiffTests
    desc "`assert_same`"
    subject do
      exp_obj, act_obj = exp_obj1, act_obj1
      Factory.test(config1) do
        assert_same(exp_obj, act_obj)
      end
    end

    should "include diff output in the fail messages" do
      subject.run(&test_run_callback)

      exp =
        "Expected #<#{act_obj1.class}:#{"0x0%x" % (act_obj1.object_id << 1)}>"\
        " to be the same as"\
        " #<#{exp_obj1.class}:#{"0x0%x" % (exp_obj1.object_id << 1)}>"\
        ", diff:\n"\
        "#{Assert::U.syscmd_diff_proc.call(exp_obj_show1, act_obj_show1)}"
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotSameDiffTests < DiffTests
    desc "`assert_not_same`"
    subject do
      act_obj = act_obj1
      Factory.test(config1) do
        assert_not_same(act_obj, act_obj)
      end
    end

    should "include diff output in the fail messages" do
      subject.run(&test_run_callback)

      exp =
        "Expected #<#{act_obj1.class}:#{"0x0%x" % (act_obj1.object_id << 1)}>"\
        " to not be the same as"\
        " #<#{act_obj1.class}:#{"0x0%x" % (act_obj1.object_id << 1)}>"\
        ", diff:\n"\
        "#{Assert::U.syscmd_diff_proc.call(act_obj_show1, act_obj_show1)}"
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
