require 'spec_helper'
require 'cluster/helpers'
require 'cluster/config/generator'

describe AutomateCluster::ConfigGenerator do
  let(:config) { AutomateCluster::Config }

  context 'aws' do
    before { config.architecture = 'aws' }

    it 'should generate a valid config file' do
      generator = AutomateCluster::ConfigGenerator.new(config)
      expect(generator.render).to match(/architecture "aws"/)
    end
  end

  context 'existing_nodes' do
    before { config.architecture = 'existing_nodes' }

    it 'should generate a valid config file' do
      generator = AutomateCluster::ConfigGenerator.new(config)
      expect(generator.render).to match(/architecture "existing_nodes"/)
    end
  end

  context 'vsphere' do
    before { config.architecture = 'vsphere' }

    it 'should generate a valid config file' do
      generator = AutomateCluster::ConfigGenerator.new(config)
      expect(generator.render).to match(/architecture "vsphere"/)
    end
  end

  context 'invalid' do
    before { config.architecture = 'invalid' }

    it 'should generate a valid config file' do
      generator = AutomateCluster::ConfigGenerator.new(config)
      expect{ generator.render }.to raise_error(AutomateCluster::Config::ConfigurationError)
    end
  end
end
