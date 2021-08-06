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
