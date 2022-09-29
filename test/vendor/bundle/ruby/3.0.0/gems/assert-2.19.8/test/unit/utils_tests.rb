# frozen_string_literal: true

require "assert"
require "assert/utils"

require "tempfile"
require "assert/config"

module Assert::Utils
  class UnitTests < Assert::Context
    desc "Assert::Utils"
    subject{ unit_class }

    let(:unit_class){ Assert::Utils }

    let(:objs1){ [1, "hi there", {}, [:a, :b]] }

    should have_imeths :show, :show_for_diff
    should have_imeths :tempfile
    should have_imeths :stdlib_pp_proc, :default_use_diff_proc
    should have_imeths :syscmd_diff_proc
    should have_imeths :git_changed_proc
  end

  class ShowTests < UnitTests
    desc "`show`"

    let(:pp_config1) do
      Assert::Config.new({
        pp_objects: true,
        pp_proc: Proc.new{ |_input| "herp derp" },
      })
    end

    should "use `inspect` to show objs when `pp_objects` setting is false" do
      objs1.each do |obj|
        assert_that(subject.show(obj, Factory.modes_off_config))
          .equals(obj.inspect)
      end
    end

    should "use `pp_proc` to show objs when `pp_objects` setting is true" do
      objs1.each do |obj|
        assert_that(subject.show(obj, pp_config1))
          .equals(pp_config1.pp_proc.call(obj))
      end
    end
  end

  class ShowForDiffTests < ShowTests
    desc "`show_for_diff`"

    let(:w_newlines1){ { string: "herp derp, derp herp\nherpderpedia" } }
    let(:w_obj_id1){ Class.new.new }

    should "call show, escaping newlines" do
      exp_out = "{:string=>\"herp derp, derp herp\nherpderpedia\"}"
      assert_that(subject.show_for_diff(w_newlines1, Factory.modes_off_config))
        .equals(exp_out)
    end

    should "make any obj ids generic" do
      exp_out = "#<#<Class:0xXXXXXX>:0xXXXXXX>"
      assert_that(subject.show_for_diff(w_obj_id1, Factory.modes_off_config))
        .equals(exp_out)
    end
  end

  class TempfileTests < UnitTests
    desc "`tempfile`"

    should "require tempfile, open a tempfile, write the given content, "\
           "and yield it" do
      subject.tempfile("a-name", "some-content") do |tmpfile|
        assert_that((require "tempfile")).equals(false)
        assert tmpfile
        assert_that(tmpfile).is_kind_of(Tempfile)

        tmpfile.pos = 0
        assert_that(tmpfile.read).equals("some-content\n")
      end
    end
  end

  class StdlibPpProcTests < UnitTests
    desc "`stdlib_pp_proc`"

    should "build a pp proc that uses stdlib `PP.pp` to pretty print objects" do
      exp_obj_pps = objs1.map{ |o| PP.pp(o, +"", 79).strip }
      act_obj_pps = objs1.map{ |o| subject.stdlib_pp_proc.call(o) }
      assert_that(act_obj_pps).equals(exp_obj_pps)

      cust_width = 1
      exp_obj_pps = objs1.map{ |o| PP.pp(o, +"", cust_width).strip }
      act_obj_pps = objs1.map{ |o| subject.stdlib_pp_proc(cust_width).call(o) }
      assert_that(act_obj_pps).equals(exp_obj_pps)
    end
  end

  class DefaultUseDiffProcTests < UnitTests
    desc "`default_use_diff_proc`"

    let(:longer1) do
      "i am a really long string output; use diff when working with me"
    end
    let(:newlines1){ "i have\n newlines" }

    should "be true if either output has newlines or is bigger than 29 chars" do
      proc = subject.default_use_diff_proc

      assert_not proc.call("", "")
      assert proc.call(longer1, "")
      assert proc.call(newlines1, "")
      assert proc.call("", longer1)
      assert proc.call("", newlines1)
      assert proc.call(longer1, newlines1)
    end
  end

  class SyscmdDiffProc < UnitTests
    desc "`syscmd_diff_proc`"

    let(:diff_a_file1){ File.join(ROOT_PATH, "test/support/diff_a.txt") }
    let(:diff_b_file1){ File.join(ROOT_PATH, "test/support/diff_b.txt") }
    let(:diff_a1){ File.read(diff_a_file1) }
    let(:diff_b1){ File.read(diff_b_file1) }

    should "use the diff syscmd to output the diff between the exp/act "\
           "show output" do
      exp_diff_out =
        `diff --unified=-1 #{diff_a_file1} #{diff_b_file1}`.strip.tap do |out|
          out.sub!(/^\-\-\- .+/, "--- expected")
          out.sub!(/^\+\+\+ .+/, "+++ actual")
        end

      assert_that(subject.syscmd_diff_proc.call(diff_a1, diff_b1))
        .equals(exp_diff_out)
    end

    should "allow you to specify a custom syscmd" do
      cust_syscmd = "diff"
      exp_diff_out =
        `#{cust_syscmd} #{diff_a_file1} #{diff_b_file1}`.strip.tap do |out|
          out.sub!(/^\-\-\- .+/, "--- expected")
          out.sub!(/^\+\+\+ .+/, "+++ actual")
        end

      assert_that(subject.syscmd_diff_proc(cust_syscmd).call(diff_a1, diff_b1))
        .equals(exp_diff_out)
    end
  end
end
