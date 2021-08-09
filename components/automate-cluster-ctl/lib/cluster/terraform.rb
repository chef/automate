require_relative 'terraform/tfvars_template'
require_relative 'terraform/output'
require_relative 'terraform/setup'

require 'openssl'
require 'tempfile'
require 'fileutils'
require 'json'

module AutomateCluster
  module Terraform
    def Terraform.output
      @output ||= Output.new
    end

    def Terraform.setup
      @setup ||= Setup.new
    end
  end
end
