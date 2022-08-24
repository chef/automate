# frozen_string_literal: true

# this file is automatically required when you run `assert`
# put any test helpers here

# add the root dir to the load path
$LOAD_PATH.unshift(File.expand_path("../..", __FILE__))

# require pry for debugging (`binding.pry`)
require "pry"

require "test/support/factory"

# 1.8.7 backfills

# Array#sample
if !(a = []).respond_to?(:sample) && a.respond_to?(:choice)
  class Array
    alias_method :sample, :choice
  end
end

# unstub all test stubs automatically for Assert test suite
require "assert"
class Assert::Context
  teardown{ MuchStub.unstub! }
end
