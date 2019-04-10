
module CtlHelpers
  module Ohai
    REQUIRED_PLUGINS = [:Hostname]

    def self.attribute(key)
      ohai[key]
    end

    private

    # Cache ohai values because they take a long time to load
    def self.ohai
      require 'ohai/system'
      return @@ohai if class_variable_defined?(:@@ohai)
      # Swallow non-error ohai logs when running ctl commands
      ::Ohai.config[:log_level] = :error
      @@ohai = ::Ohai::System.new
      @@ohai.all_plugins
      @@ohai
    end

    # Simplifies resetting the cached ohai for testing purposes.
    def self.reset!
      remove_class_variable(:@@ohai) if class_variable_defined?(:@@ohai)
      # Dont' return the value of old @@ohai
      nil
    end
  end
end
