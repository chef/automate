# frozen_string_literal: true

require "assert"
require "assert/context_info"

require "assert/context"

class Assert::ContextInfo
  class UnitTests < Assert::Context
    desc "Assert::ContextInfo"
    subject{ unit_class }

    let(:unit_class){ Assert::ContextInfo }
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new(context1, nil, @caller.first) }

    setup do
      @caller = caller
    end

    let(:context1){ Assert::Context }

    should have_readers :called_from, :klass, :file
    should have_imeths :test_name

    should "set its klass on init" do
      assert_that(subject.klass).equals(context1)
    end

    should "set its called_from to the called_from or first caller on init" do
      info = Assert::ContextInfo.new(context1, @caller.first, nil)
      assert_that(info.called_from).equals(@caller.first)

      info = Assert::ContextInfo.new(context1, nil, @caller.first)
      assert_that(info.called_from).equals(@caller.first)
    end

    should "set its file from caller info on init" do
      assert_that(subject.file).equals(@caller.first.gsub(/\:[0-9]+.*$/, ""))
    end

    should "not have any file info if no caller is given" do
      info = Assert::ContextInfo.new(context1)
      assert_that(info.file).is_nil
    end

    should "know how to build the contextual test name for a given name" do
      desc = Factory.string
      name = Factory.string

      assert_that(subject.test_name(name)).equals(name)
      assert_that(subject.test_name("")).equals("")
      assert_that(subject.test_name(nil)).equals("")

      Assert.stub(subject.klass, :description){ desc }
      assert_that(subject.test_name(name)).equals("#{desc} #{name}")
    end
  end
end
