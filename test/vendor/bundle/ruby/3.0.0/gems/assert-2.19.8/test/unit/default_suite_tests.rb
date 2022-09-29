# frozen_string_literal: true

require "assert"
require "assert/default_suite"

require "assert/suite"

class Assert::DefaultSuite
  class UnitTests < Assert::Context
    desc "Assert::DefaultSuite"
    subject{ unit_class }

    let(:unit_class){ Assert::DefaultSuite }
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new(config1) }

    let(:ci1){ Factory.context_info(Factory.modes_off_context_class) }
    let(:test1){ Factory.test(Factory.string, ci1){} }
    let(:config1){ Factory.modes_off_config }

    should have_readers :test_count, :result_count, :pass_result_count
    should have_readers :fail_result_count, :error_result_count
    should have_readers :skip_result_count, :ignore_result_count

    should "be a Suite" do
      assert_that(subject).is_kind_of(Assert::Suite)
    end

    should "default its test/result counts" do
      assert_that(subject.test_count).equals(0)
      assert_that(subject.result_count).equals(0)
      assert_that(subject.pass_result_count).equals(0)
      assert_that(subject.fail_result_count).equals(0)
      assert_that(subject.error_result_count).equals(0)
      assert_that(subject.skip_result_count).equals(0)
      assert_that(subject.ignore_result_count).equals(0)
    end

    should "increment its test count on `before_test`" do
      subject.before_test(@test)
      assert_that(subject.test_count).equals(1)
    end

    should "increment its result counts on `on_result`" do
      subject.on_result(Factory.pass_result)
      assert_that(subject.result_count).equals(1)
      assert_that(subject.pass_result_count).equals(1)
      assert_that(subject.fail_result_count).equals(0)
      assert_that(subject.error_result_count).equals(0)
      assert_that(subject.skip_result_count).equals(0)
      assert_that(subject.ignore_result_count).equals(0)

      subject.on_result(Factory.fail_result)
      assert_that(subject.result_count).equals(2)
      assert_that(subject.pass_result_count).equals(1)
      assert_that(subject.fail_result_count).equals(1)
      assert_that(subject.error_result_count).equals(0)
      assert_that(subject.skip_result_count).equals(0)
      assert_that(subject.ignore_result_count).equals(0)

      subject.on_result(Factory.error_result)
      assert_that(subject.result_count).equals(3)
      assert_that(subject.pass_result_count).equals(1)
      assert_that(subject.fail_result_count).equals(1)
      assert_that(subject.error_result_count).equals(1)
      assert_that(subject.skip_result_count).equals(0)
      assert_that(subject.ignore_result_count).equals(0)

      subject.on_result(Factory.skip_result)
      assert_that(subject.result_count).equals(4)
      assert_that(subject.pass_result_count).equals(1)
      assert_that(subject.fail_result_count).equals(1)
      assert_that(subject.error_result_count).equals(1)
      assert_that(subject.skip_result_count).equals(1)
      assert_that(subject.ignore_result_count).equals(0)

      subject.on_result(Factory.ignore_result)
      assert_that(subject.result_count).equals(5)
      assert_that(subject.pass_result_count).equals(1)
      assert_that(subject.fail_result_count).equals(1)
      assert_that(subject.error_result_count).equals(1)
      assert_that(subject.skip_result_count).equals(1)
      assert_that(subject.ignore_result_count).equals(1)
    end

    should "clear the run data on `on_start`" do
      subject.before_test(test1)
      subject.on_result(Factory.pass_result)

      assert_that(subject.test_count).equals(1)
      assert_that(subject.result_count).equals(1)
      assert_that(subject.pass_result_count).equals(1)

      subject.on_start

      assert_that(subject.test_count).equals(0)
      assert_that(subject.result_count).equals(0)
      assert_that(subject.pass_result_count).equals(0)
    end
  end
end
