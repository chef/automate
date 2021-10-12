require 'spec_helper'
require 'cluster/ssh'
require 'tempfile'

describe AutomateCluster::SSH do
  let(:ssh) { AutomateCluster::SSH.new }
  let(:tfoutput) { instance_double('AutomateCluster::Terraform::Output') }
  let(:connection) { instance_double("AutomateCluster::SSHConnection") }

  before  do
    expect(tfoutput).to receive(:ssh_node_types).and_return(["automate", "postgresql", "elasticsearch"])
    expect(AutomateCluster::Terraform).to receive(:output).and_return(tfoutput)
  end

  it "should not raise error if the service does not exist" do
    expect{ ssh.connections(service: 'test').map }.not_to raise_error
  end

  it "should only return data for the specified service" do
    expect(tfoutput).to receive(:fetch).with("automate_private_ips").and_return (["10.10.10.10"])
    expect(connection).to receive(:wait_until_ready).and_return true
    expect(ssh).to receive(:connect).with("10.10.10.10", {}).and_return(connection)

    ssh.connections(service: 'automate') do |type, ssh|
      expect(type).to eq 'automate'
    end
  end
end
