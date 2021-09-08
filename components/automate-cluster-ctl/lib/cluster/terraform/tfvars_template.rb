require 'erb'

module AutomateCluster
  module Terraform
    class TFVarsTemplate
      attr_reader :config, :secrets

      def initialize(config, template)
        @config = config
        @template = ERB.new(File.read(template), 0, '<>')
      end

      def render
        @template.result(binding)
      end
    end
  end
end
