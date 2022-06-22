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
require 'yaml'

module AutomateCluster
  class InspecInputs
    def initialize
      @inputs = {}
    end

    def input_file
      @input_file ||= Tempfile.new(['inpsec-inputs', '.yml'])
    end

    def add(key, value)
      @inputs[key] = value
    end

    def path
      input_file.path
    end

    def delete
      input_file.close
      input_file.unlink
    end

    def save
      input_file.truncate(0)
      input_file << @inputs.to_yaml unless @inputs.keys.empty?
      input_file.fsync

      input_file.path
    end
  end
end
