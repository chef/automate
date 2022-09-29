# frozen_string_literal: true

require "assert/version"

require "assert/config"
require "assert/context"
require "assert/runner"
require "assert/stub"
require "assert/suite"
require "assert/utils"
require "assert/view"

module Assert
  def self.config
    @config ||= Config.new
  end

  def self.configure
    yield config if block_given?
  end

  def self.view
    config.view
  end

  def self.suite
    config.suite
  end

  def self.runner
    config.runner
  end

  # unstub all stubs automatically (see stub.rb)
  class Context
    teardown{ Assert.unstub! }
  end
end
