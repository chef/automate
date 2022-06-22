require_relative 'helpers'
require_relative '../helpers'
require_relative 'output'

module AutomateCluster
  module Terraform
    class Base
      include AutomateCluster::Helpers
      include AutomateCluster::Terraform::Helpers

      attr_reader :config, :logger

      def initialize
        @config = AutomateCluster::Config
        @logger = AutomateCluster.logger
      end
    end
  end
end
