require 'spec_helper'
require 'cluster/config'

describe AutomateCluster::Config do
  let(:config) { AutomateCluster::Config }

  context 'ssh_key_file' do
    before do
      @old_home = ENV['HOME']
      ENV['HOME'] = '/test'
    end

    after do
      ENV['HOME'] = @old_home
    end

    context 'a2_deploy_workspace_handling' do
      before do
        allow(File).to receive(:exist?).with('/hab/a2_deploy_workspace').and_return true
        allow(File).to receive(:realpath).with('/hab/a2_deploy_workspace').and_return '/testing/workspace'
      end

      it 'should set put /hab/a2_deploy_workspace back into path' do
        path = config.expand_relative_paths('/testing/workspace/configs/automate.toml')
        expect(path).to eq '/hab/a2_deploy_workspace/configs/automate.toml'
      end

      it 'should update path to symlink path' do
        config.automate.config_file = '/testing/workspace/configs/automate.toml'
        expect(config.automate.config_file).to eq '/hab/a2_deploy_workspace/configs/automate.toml'
      end

      it 'should not update path to symlink path if not at start' do
        config.automate.config_file = '/opt/testing/workspace/configs/automate.toml'
        expect(config.automate.config_file).to eq '/opt/testing/workspace/configs/automate.toml'
      end
    end

    it 'should expand the ssh_key_file path' do
      config.ssh_key_file = '~/.ssh/id_rsa'
      expect(config.ssh_key_file).to eq '/test/.ssh/id_rsa'
    end

    it 'should expand the secrets_store_file path' do
      config.secrets_store_file = 'secrets.json'
      expect(config.secrets_store_file).to eq File.join(Dir.pwd, 'secrets.json')
    end

    it 'should expand when loading from file' do
      # expect(config).to receive(:read).with('test.rb').and_return("ssh_key_file '~/file'")
      config.from_file('spec/fixtures/test_config.rb')

      expect(config.ssh_key_file).to eq '/test/test_key'
    end
  end
end
