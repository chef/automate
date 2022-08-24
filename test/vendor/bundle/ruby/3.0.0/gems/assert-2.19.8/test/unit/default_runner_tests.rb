# frozen_string_literal: true

require "assert"
require "assert/default_runner"

require "assert/runner"

class Assert::DefaultRunner
  class UnitTests < Assert::Context
    desc "Assert::DefaultRunner"

    # This is tested implicitly by running Assert's test suite
  end
end
