require 'spec_helper'
require 'cluster/helpers'

class HelperTest
  include AutomateCluster::Helpers
end

describe AutomateCluster::Helpers do
  before do
    AutomateCluster::Config.reset
    AutomateCluster.logger.handlers = [:null]
    ENV['_CLUSTER_CTL_ROOT'] = '/test/path'
  end

  after do
    ENV.delete('_CLUSTER_CTL_ROOT')
  end

  let(:instance) { HelperTest.new }

  it 'should return the correct root_path' do
    expect(instance.root_path).to eq '/test/path'
  end

  it 'should return the a2_deploy_workspace directory for workspace_path if nothing is specified' do
    expect(instance.workspace_path).to eq '/hab/a2_deploy_workspace'
  end

  context 'with workspace config path set' do
    before do
      AutomateCluster::Config.workspace_path = '/test/config_workspace'
    end

    it 'should return the configured workspace path if defined' do
      expect(instance.workspace_path).to eq '/test/config_workspace'
    end

    it 'should return the correct profile path' do
      expect(instance.profile_path).to eq '/test/path/inspec'
    end

    it 'should run make commands in workspace_path' do
      shellout = instance_double("Mixlib::ShellOut")
      expect(shellout).to receive(:stdout).and_return('Test Output')

      expect(instance).to receive(:shellout!).with('make test', cwd: '/test/config_workspace').and_return(shellout)

      instance.run_make_cmd('test')
    end

    it 'should handle running terraform commands with default options' do
      shellout = instance_double("Mixlib::ShellOut")
      expect(shellout).to receive(:stdout).at_least(:once) { 'Some output' }
      expect(shellout).to receive(:stderr).at_least(:once) { '' }

      opts = {
        cwd: '/test/config_workspace/terraform',
        environment: { 'TF_INPUT'=> '0', 'TF_IN_AUTOMATION' => 'true' },
        timeout: 3600
      }
      expect(instance).to receive(:shellout!).with('terraform plan', opts).and_return(shellout)

      instance.terraform_run('plan')
    end
  end

  it 'should call mixlib shellout with the correct options' do
    expect_any_instance_of(Mixlib::ShellOut).to receive(:run_command).and_return(true)
    expect_any_instance_of(Mixlib::ShellOut).to receive(:error!).and_return(true)

    instance.shellout!('echo "test"')
  end

  it 'should handle errors thrown by mixlib shellout' do
    failure = instance_double('Mixlib::Shellout', run_command: true, stdout: '', stderr: 'Failed command', exitstatus: 1)
    expect(failure).to receive(:error!) {
      raise Mixlib::ShellOut::ShellCommandFailed, 'Failed command'
    }

    expect(instance).to receive(:_shellout).and_return(failure)
    expect(AutomateCluster.logger).to receive(:error)

    instance.shellout!('failure')
  end

  it 'should run the okta_aws command' do
    instance.config.aws.profile = 'test'
    shellout = instance_double("Mixlib::ShellOut")
    expect(shellout).to receive(:error?).and_return(false)
    expect(instance).to receive(:shellout!).with('okta_aws test').and_return(shellout)

    instance.login_with_okta
  end

  it 'should handle okta_aws command failure' do
    instance.config.aws.profile = 'test'
    shellout = instance_double("Mixlib::ShellOut")

    expect(shellout).to receive(:error?).and_return(true)
    expect(instance).to receive(:shellout!).with('okta_aws test').and_return(shellout)

    expect { instance.login_with_okta }.to raise_error(SystemExit) do |error|
      expect(error.status).to eq(1)
    end
  end
end
