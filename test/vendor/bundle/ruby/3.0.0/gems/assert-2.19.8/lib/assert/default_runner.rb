# frozen_string_literal: true

require "assert/runner"

module Assert
  # This is the default runner used by assert.  It adds no special behavior on
  # top of the base runner's behavior
  class DefaultRunner < Assert::Runner
  end
end
