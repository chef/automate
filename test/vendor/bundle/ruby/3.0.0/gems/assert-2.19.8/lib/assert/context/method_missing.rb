# frozen_string_literal: true

require "assert/assertions"

module Assert; end
class Assert::Context; end

module Assert::Context::MethodMissing
  def method_missing(method, *args, &block)
    if Assert::Assertions::IGNORED_ASSERTION_HELPERS.include?(method.to_sym)
      ignore "The assertion `#{method}` is not supported."\
             " Please use another assertion or the basic `assert`."
    else
      super
    end
  end

  def respond_to_missing?(method, *)
    Assert::Assertions::IGNORED_ASSERTION_HELPERS.include?(method.to_sym) ||
    super
  end
end
