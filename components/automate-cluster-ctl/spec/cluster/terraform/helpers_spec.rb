require 'spec_helper'
require 'cluster/terraform/helpers'

class HelperTest
  include AutomateCluster::Terraform::Helpers
end

describe AutomateCluster::Terraform::Helpers do
  before do
    AutomateCluster::Config.workspace_path = '/test/workspace'
  end

  let(:instance) { HelperTest.new }
  let(:terraform_path) { '/test/workspace/terraform' }

  context 'valid_aib_files' do
    it "should pass if tfvars is valid" do
      expect(File).to receive(:exist?).with('/test/workspace/terraform/a2ha_manifest.auto.tfvars').and_return(true)

      expect(File).to receive(:exist?).with('/test/workspace/terraform/a2ha_aib_be.auto.tfvars').and_return(true)
      expect(Tomlrb).to receive(:load_file).and_return({ backend_aib_local_file: 'frontend.aib' })
      expect(File).to receive(:exist?).with('/test/workspace/terraform/transfer_files/backend.aib').and_return(true)

      expect(File).to receive(:exist?).with('/test/workspace/terraform/a2ha_aib_fe.auto.tfvars').and_return(true)
      expect(Tomlrb).to receive(:load_file).and_return({ frontend_aib_local_file: 'backend.aib' })
      expect(File).to receive(:exist?).with('/test/workspace/terraform/transfer_files/frontend.aib').and_return(true)

      expect(instance.valid_aibs?).to be true
    end
  end

  context "invalid aib file contents" do
    it "should return false for an invalid tfvars file" do
      expect(instance.check_aib_tfvars('spec/fixtures/invalid_aib.tfvars', :test)).to eq false
    end
  end

  context "empty aib filename" do
    it "should fail if be aib file is blank" do
      expect(File).to receive(:exist?).with("#{terraform_path}/a2ha_aib_be.auto.tfvars").and_return(true)
      expect(Tomlrb).to receive(:load_file).and_return({ backend_aib_local_file: '' })
      expect(instance.tf_be_aib_valid?).to be false
    end
  end

  context 'missing_manifest' do
    it "should pass if tfvars is valid" do
      expect(File).to receive(:exist?).with("#{terraform_path}/a2ha_manifest.auto.tfvars").and_return(false)
      expect(instance.valid_aibs?).to be false
    end
  end

  context 'empty_aib_files' do
    it "should fail if be tfvars key is missing" do
      expect(File).to receive(:exist?).with("#{terraform_path}/a2ha_aib_be.auto.tfvars").and_return(true)
      expect(Tomlrb).to receive(:load_file).and_return({ })
      expect(instance.tf_be_aib_valid?).to be false
    end

    it "should fail if fe tfvars key is missing" do
      expect(File).to receive(:exist?).with("#{terraform_path}/a2ha_aib_fe.auto.tfvars").and_return(true)
      expect(Tomlrb).to receive(:load_file).and_return({ })
      expect(instance.tf_fe_aib_valid?).to be false
    end
  end

  it "should check for a habitat file" do
    expect(File).to receive(:exist?).with("#{terraform_path}/a2ha_habitat.auto.tfvars").and_return(true)

    instance.tf_hab_secrets_exist?
  end

  it "should check if the aib file exists" do
    expect(File).to receive(:exist?).with("#{terraform_path}/transfer_files/somefile").and_return(true)

    instance.local_aib_exists?('somefile')
  end

  it "should return false if filename is invalid" do
    expect(instance.local_aib_exists?(false)).to be false
    expect(instance.local_aib_exists?('')).to be false
  end
end
