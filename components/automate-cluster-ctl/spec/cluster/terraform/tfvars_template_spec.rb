require 'spec_helper'
require 'cluster/helpers'

describe AutomateCluster::Terraform::TFVarsTemplate do
  let(:config) { AutomateCluster::Config }

  context 'aws' do
    before { config.architecture = 'aws' }

    it 'should generate a valid tfvars file' do
      template = AutomateCluster::Terraform::TFVarsTemplate.new(config, 'templates/terraform.tfvars.erb')
      expect { template.render }.not_to raise_error
    end
  end

  context 'existing_nodes' do
    before { config.architecture = 'existing_nodes' }

    it 'should generate a valid tfvars file' do
      template = AutomateCluster::Terraform::TFVarsTemplate.new(config, 'templates/terraform.tfvars.erb')
      expect { template.render }.not_to raise_error
    end
  end

  context 'vsphere' do
    before { config.architecture = 'vsphere' }

    it 'should generate a valid tfvars file' do
      template = AutomateCluster::Terraform::TFVarsTemplate.new(config, 'templates/terraform.tfvars.erb')
      expect { template.render }.not_to raise_error
    end
  end
end
