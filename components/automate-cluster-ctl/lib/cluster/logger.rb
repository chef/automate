#
# Author:: Faizan Fulara (<ffulara@progress.com>)
# Copyright:: Copyright (c) Chef Software Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
require 'singleton'

class TTY::Logger
  def level
    @config.level
  end
end

module AutomateCluster
  class Logger
    attr_reader :logger

    def initialize
      reset
    end

    def reset
      @logger = TTY::Logger.new do |config|
        config.handlers = [ [:console, output: $stderr] ]
        config.level = :info
        config.date_format = "%c"
        config.metadata = [:date]
      end
    end

    def self.instance
      @instance ||= new
    end

    def level=(level)
      @logger = logger.copy({}) do |config|
        config.level = level
      end
    end

    def handlers=(handlers)
      @logger = logger.copy({}) do |config|
        config.handlers = handlers
      end
    end

    def filter(filters)
      @logger = logger.copy({}) do |config|
        config.filters.message = filters.uniq
      end
    end

    def level
      logger.level
    end

    def info(*args)
      logger.info(*args)
    end

    def debug(*args)
      logger.debug(*args)
    end

    def warn(*args)
      logger.warn(*args)
    end

    def error(*args)
      logger.error(*args)
    end
  end
end
