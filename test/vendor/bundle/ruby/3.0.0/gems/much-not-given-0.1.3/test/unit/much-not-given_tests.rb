# frozen_string_literal: true

require "assert"
require "much-not-given"

module MuchNotGiven
  class UnitTests < Assert::Context
    desc "MuchNotGiven"
    subject{ unit_module }

    let(:unit_module){ MuchNotGiven }

    should "inlcude MuchMixin" do
      assert_that(unit_module).includes(MuchMixin)
    end
  end

  class ReceiverTests < UnitTests
    desc "reciver"
    subject{ receiver_class }

    let(:receiver_class) do
      Class.new do
        include MuchNotGiven
      end
    end

    let(:value1) do
      Factory.public_send(
        [:integer, :string, :float, :time, :path, :boolean].sample,
      )
    end

    should have_imeths :not_given, :not_given?, :given?

    should "have a not_given singleton value" do
      not_given = subject.not_given

      assert_that(not_given.blank?).is_true
      assert_that(not_given.present?).is_false
      assert_that(not_given.to_s).equals("#{subject.inspect}.not_given")
      assert_that(not_given.inspect).equals(not_given.to_s)

      assert_that(subject.not_given).is_the_same_as(not_given)
      assert_that(subject.not_given == not_given).is_true
      assert_that(value1 == not_given).is_false
    end

    should "know if values are given or not" do
      assert_that(subject.given?(value1)).is_true
      assert_that(subject.given?(subject.not_given)).is_false

      assert_that(subject.not_given?(value1)).is_false
      assert_that(subject.not_given?(subject.not_given)).is_true
    end
  end
end
