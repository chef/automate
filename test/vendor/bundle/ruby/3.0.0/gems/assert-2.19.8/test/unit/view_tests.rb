# frozen_string_literal: true

require "assert"
require "assert/view"

require "stringio"
require "assert/config_helpers"
require "assert/suite"
require "assert/view_helpers"

class Assert::View
  class UnitTests < Assert::Context
    desc "Assert::View"
    subject{ unit_class }

    let(:unit_class){ Assert::View }

    should have_instance_method :require_user_view

    should "include the config helpers" do
      assert_that(subject).includes(Assert::ConfigHelpers)
    end

    should "include the view helpers" do
      assert_that(subject).includes(Assert::ViewHelpers)
    end
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ Assert::View.new(config1, io1) }

    let(:io1){ StringIO.new(+"", "w+") }
    let(:config1){ Factory.modes_off_config }

    should have_readers :config
    should have_imeths :view, :is_tty?
    should have_imeths :before_load, :after_load
    should have_imeths :on_start, :on_finish, :on_info, :on_interrupt
    should have_imeths :before_test, :after_test, :on_result

    should "default its style options" do
      assert_that(subject.styled).is_false

      assert_that(subject.pass_styles).is_nil
      assert_that(subject.fail_styles).is_nil
      assert_that(subject.error_styles).is_nil
      assert_that(subject.skip_styles).is_nil
      assert_that(subject.ignore_styles).is_nil
    end

    should "default its result abbreviations" do
      assert_that(subject.pass_abbrev).equals(".")
      assert_that(subject.fail_abbrev).equals("F")
      assert_that(subject.ignore_abbrev).equals("I")
      assert_that(subject.skip_abbrev).equals("S")
      assert_that(subject.error_abbrev).equals("E")
    end

    should "know its config" do
      assert_that(subject.config).equals(config1)
    end

    should "override the config helper's view value with itself" do
      assert_that(subject.view).equals(subject)
    end

    should "know if it is a tty" do
      assert_that(subject.is_tty?).equals(!!io1.isatty)
    end
  end
end
