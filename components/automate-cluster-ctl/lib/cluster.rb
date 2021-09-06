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
require 'cluster/config'
require 'cluster/terraform'
require 'cluster/habitat/info'
require 'cluster/logger'
require 'tty/logger'

module AutomateCluster
  def log_level=(level)
    logger.level = level
  end
  module_function :log_level=

  def logger
    AutomateCluster::Logger.instance
  end
  module_function :logger

  def terraform
    AutomateCluster::Terraform
  end
  module_function :terraform

  def hab_info
    @hab_info ||= AutomateCluster::Habitat::Info.new
  end
  module_function :hab_info

  def secrets
    @secrets ||= AutomateCluster::Secrets.instance
  end
  module_function :secrets
end
