require 'mixlib/log'

module Automate
  module Backend
    # Log class
    class Log
      extend Mixlib::Log
      Log.level = if ENV['BACKEND_CTL_LOG_LEVEL']
                    ENV['BACKEND_CTL_LOG_LEVEL'].to_sym
                  else
                    :info
                  end
    end
  end
end
