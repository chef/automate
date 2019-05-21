require 'spec_helper'

require 'build-node/config'
require 'build-node/exceptions'
require 'build-node/local_knife'

describe BuildNode::LocalKnife do
  let(:local_knife) { BuildNode::LocalKnife.new(config) }

  # delivery.rb configuration values
  let(:delivery_fqdn) { 'delivery-server.example.com' }
  let(:delivery_user) { 'delivery-user' }
  let(:delivery_user_key) { 'delivery-user.pem' }
  let(:chef_server_url) { 'https://chef-server/organizations/delivery-org' }
  let(:chef_server_proxy) { false }
  let(:automate_ssl_port) { 443 }

  # mock objects
  let(:config) { BuildNode::Config.new({}) }
  let(:mock_shell_out) { double("Mixlib::ShellOut") }

  # delivery-ctl configuration values
  let(:username) { 'build-node-user' }
  let(:password) { 'build-node-user-password' }
  let(:fqdn) { 'build-node' }
  let(:ssh_identity_file) { 'id_rsa' }

  let(:knife_path) {
    "/opt/delivery/embedded/bin/knife"
  }

  let(:base_knife_options) {
    [ '-u', delivery_user, '-k', delivery_user_key,
      '--server-url', chef_server_url ].join(' ')
  }

  before(:each) do
    config.set_username(username)
    config.set_password(password)
    config.set_fqdn(fqdn)

    delivery_options = {
      chef_username: delivery_user,
      chef_private_key: delivery_user_key,
      chef_server: chef_server_url,
      chef_server_proxy: chef_server_proxy
    }
    allow(CtlHelpers::DeliveryConfig).to receive(:delivery).and_return(delivery_options)
    allow(CtlHelpers::DeliveryConfig).to receive(:delivery_fqdn).and_return(delivery_fqdn)
    nginx_options = {
      ssl_port: automate_ssl_port
    }
    allow(CtlHelpers::DeliveryConfig).to receive(:nginx).and_return(nginx_options)
  end

  describe "#create_command" do
    it "constructs a command with the correct required values for embedded knife" do
      expected_knife_command = [knife_path, "test", base_knife_options].join(' ')
      expect(local_knife.create_command("test")).to eq expected_knife_command
    end

    it "includes any arguments provided in the final command" do
      expected_knife_command = [knife_path, "test",
                                "--extra-option true",
                                base_knife_options].join(' ')

      expect(local_knife.create_command("test", "--extra-option", "true")).to eq(expected_knife_command)
    end

    context 'when using a proxied chef-server' do
      let(:chef_server_proxy) { true }
      let(:chef_server_url) { 'https://localhost:8443/organizations/delivery-org' }
      let(:base_knife_options) {
        [ '-u', delivery_user, '-k', delivery_user_key,
          '--server-url', chef_server_url ].join(' ')
      }

      it "fixes the chef-server-url used for knife commands" do
        expected_knife_command = [knife_path, "test",
                                  "--extra-option true",
                                  "-u", delivery_user,
                                  "-k", delivery_user_key,
                                  "--server-url", # this is the important part
                                  "https://delivery-server.example.com/organizations/delivery-org",
                                 ].join(' ')

        expect(local_knife.create_command("test", "--extra-option", "true")).to eq(expected_knife_command)
      end

      context "when having automate https on a non-default port" do
        let(:automate_ssl_port) { 440 }

        it "fixes the chef-server-url used for knife commands, with the right port, too" do
          expected_knife_command = [knife_path, "test",
                                    "--extra-option true",
                                    "-u", delivery_user,
                                    "-k", delivery_user_key,
                                    "--server-url", # this is the important part
                                    "https://delivery-server.example.com:440/organizations/delivery-org",
                                   ].join(' ')

          expect(local_knife.create_command("test", "--extra-option", "true")).to eq(expected_knife_command)
        end
      end
    end
  end

  describe "#run_command" do
    it "assembles the command using create_command, runs it via ShellOut, and verifies the result" do
      # Make sure that this runs what its told to by create_command by getting
      # it from create_command ourselves.
      expected_command = local_knife.create_command("test", "opt1", "opt2")

      expect(local_knife).to receive(:create_command).with("test", "opt1", "opt2").
        and_return expected_command

      expect(Mixlib::ShellOut).to receive(:new).with(expected_command).
        and_return mock_shell_out

      expect(mock_shell_out).to receive(:run_command)
      expect(mock_shell_out).to receive(:error!)

      local_knife.run_command("test", "opt1", "opt2")
    end

    it "raises KnifeCommandFailed error when the exitstatus is nonzero" do
      expected_command = local_knife.create_command("test", "-i", "identity")
      allow(local_knife).to receive(:create_command).
        and_return(expected_command, expected_command)
      expect(Mixlib::ShellOut).to receive(:new).
        with(expected_command).and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:run_command)
      expect(mock_shell_out).to receive(:exitstatus).and_return(100)
      expect(mock_shell_out).to receive(:error!).
        and_raise(Mixlib::ShellOut::ShellCommandFailed, "FAILURE!!")

      expect{local_knife.run_command("test", "-i", "identity")}.to raise_error(
        BuildNode::Exceptions::KnifeCommandFailed, "/opt/delivery/embedded/bin/knife test -i identity -u delivery-user -k delivery-user.pem --server-url https://chef-server/organizations/delivery-org returned 100"
      )
    end

    it "raises KnifeCommandFailed error and suppresses password in error output when the exitstatus is nonzero" do
      expected_command = local_knife.create_command("test", "--ssh-password", "password")
      hidden_command = local_knife.create_command("test", "--ssh-password", "(hidden)")
      allow(local_knife).to receive(:create_command).
        and_return(expected_command, hidden_command)
      expect(Mixlib::ShellOut).to receive(:new).
        with(expected_command).and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:run_command)
      expect(mock_shell_out).to receive(:exitstatus).and_return(100)
      expect(mock_shell_out).to receive(:error!).
        and_raise(Mixlib::ShellOut::ShellCommandFailed, "SENSITIVE OUTPUT!!")

      expect{local_knife.run_command("test", "--ssh-password", "password")}.to raise_error(
        BuildNode::Exceptions::KnifeCommandFailed, "/opt/delivery/embedded/bin/knife test --ssh-password (hidden) -u delivery-user -k delivery-user.pem --server-url https://chef-server/organizations/delivery-org returned 100"
      )
    end
  end

  describe '#delivery_cluster_node?' do
    it 'returns true when the node has the delivery_builders role' do
      expect(local_knife).to receive(:run_command).
        with("search node '(name:#{fqdn} OR fqdn:#{fqdn} OR ipaddress:#{fqdn}) AND role:delivery_builders'", *['-a', 'fqdn']).
        and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:stdout).
        and_return("#{fqdn}:\n  fqdn: #{fqdn}\n\n")

      expect(local_knife.delivery_cluster_node?).to be true
    end

    it 'returns false when the node does not have the delivery_builders role' do
      expect(local_knife).to receive(:run_command).
        with("search node '(name:#{fqdn} OR fqdn:#{fqdn} OR ipaddress:#{fqdn}) AND role:delivery_builders'", *['-a', 'fqdn']).
        and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:stdout).
        and_return("")

      expect(local_knife.delivery_cluster_node?).to be_falsey
    end
  end


  describe '#node_exists?' do
    it "returns true when the node is already registered with the Chef Server" do
      expect(local_knife).to receive(:run_command).
        with("search node name:#{fqdn}", *['-a', 'fqdn']).
        and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:stdout).
        and_return("#{fqdn}:\n  fqdn: #{fqdn}\n\n")

      expect(local_knife.node_exists?).to be true
    end

    it "returns false when the node is not registered with the Chef Server" do
      expect(local_knife).to receive(:run_command).
        with("search node name:#{fqdn}", *['-a', 'fqdn']).
        and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:stdout).
        and_return("")

      expect(local_knife.node_exists?).to be_falsey
    end
  end

  describe '#client_exists?' do
    it "returns true when the client is already registered with the Chef Server" do
      expect(local_knife).to receive(:run_command).
        with("search client name:#{fqdn}", *['-a', 'fqdn']).
        and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:stdout).
        and_return("#{fqdn}:\n  fqdn: #{fqdn}\n\n")

      expect(local_knife.client_exists?).to be true
    end

    it "returns false when the client is not registered with the Chef Server" do
      expect(local_knife).to receive(:run_command).
        with("search client name:#{fqdn}", *['-a', 'fqdn']).
        and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:stdout).
        and_return("")

      expect(local_knife.client_exists?).to be_falsey
    end
  end

  describe '#bootstrap_node' do
    let(:bootstrap_options) do
      ['--ssh-user', username, '--node-name', fqdn, '--sudo',
       '--use-sudo-password']
    end

    context "when no ssh identity file is given" do
      let(:bootstrap_options) do
        [ '--ssh-user', username, '--node-name', fqdn, '--sudo',
          '--use-sudo-password', '--ssh-password', "'#{password}'" ]
      end

      it "uses the provided password for ssh authentication" do
        expect(local_knife).to receive(:run_command).
          with("bootstrap", fqdn, *bootstrap_options)
        local_knife.bootstrap_node
      end
    end

    context "when an ssh identity file is given and no password is given" do
      let(:bootstrap_options) do
        ['--ssh-user', username, '--node-name', fqdn, '--sudo',
         '--use-sudo-password', '--ssh-identity-file', ssh_identity_file]
      end

      before :each do
        # config.set_ssh_identity_file does validation of the file, we'll
        # set the instance variable to avoid that
        config.instance_variable_set(:@ssh_identity_file, ssh_identity_file)
        config.instance_variable_set(:@password, nil)
      end

      it "uses the provided identify file for ssh authentication" do
        expect(local_knife).to receive(:run_command).
          with('bootstrap', fqdn, *bootstrap_options)
        local_knife.bootstrap_node
      end
    end

    context "when both an ssh identity file and a password are given" do
      let(:bootstrap_options) do
        ['--ssh-user', username, '--node-name', fqdn, '--sudo',
         '--use-sudo-password', '--ssh-identity-file', ssh_identity_file,
         '--ssh-password', "'#{password}'" ]
      end

      before :each do
        # config.set_ssh_identity_file does validation of the file, we'll
        # set the instance variable to avoid that
        config.instance_variable_set(:@ssh_identity_file, ssh_identity_file)
      end

      it 'sets the SSH password' do
        expect(local_knife).to receive(:run_command).
          with('bootstrap', fqdn, *bootstrap_options)
        local_knife.bootstrap_node
      end
    end

    context "when the overwrite flag is given" do
      let(:bootstrap_options) do
        ['--ssh-user', username, '--node-name', fqdn, '--sudo',
         '--use-sudo-password', '--ssh-password', "'#{password}'", '--yes']
      end

      it "forces overwrite if requested by flags" do
        expect(local_knife).to receive(:run_command).
          with("bootstrap", fqdn, *bootstrap_options)
        local_knife.bootstrap_node(overwrite: true)
      end
    end

    context "when --no-full-ohai is given (default)" do
      let(:bootstrap_options) do
        [ '--ssh-user', username, '--node-name', fqdn, '--sudo',
          '--use-sudo-password', '--ssh-password', "'#{password}'" ]
      end

      before :each do
        config.instance_variable_set(:@client_config, "minimal_ohai true\n")
      end

      it "passes --config-option client_d_dir" do
        expect(local_knife).to receive(:run_command).
          with("bootstrap", fqdn, *bootstrap_options, /--config-option client_d_dir=/)
        local_knife.bootstrap_node
      end
    end
  end

  describe '#tag_node' do
    it "tags the node with delivery-build-node" do
      expect(local_knife).to receive(:run_command).
        with('tag create', [config.fqdn, "delivery-build-node"])
      local_knife.tag_node
    end
  end

  describe '#bootstrap' do
    it "verifies the node can be registered, bootstraps it, and tags it" do
      flags = {overwrite: true}
      expect(local_knife).to receive(:verify_node_not_registered).with(flags)
      expect(local_knife).to receive(:bootstrap_node).with(flags)
      expect(local_knife).to receive(:tag_node).with(nil)
      local_knife.bootstrap(flags)
    end

    context "when tag is overridden" do
      it "tags the build node accordingly" do
        flags = {overwrite: true, tag: 'shiny-new-delivery-job-runner'}
        expect(local_knife).to receive(:verify_node_not_registered).with(flags)
        expect(local_knife).to receive(:bootstrap_node).with(flags)
        expect(local_knife).to receive(:tag_node).with('shiny-new-delivery-job-runner')
        local_knife.bootstrap(flags)
      end
    end
  end

  describe '#fetch_ssl_certificates' do
    it "fetches ssl certificates from the Chef Server via 'knife ssl fetch'" do
        expect(local_knife).to receive(:run_command).with("ssl fetch")
        local_knife.fetch_ssl_certificates
    end

    it "raises a CertFetchFailed when the 'knife ssl fetch' fails" do
      allow(local_knife).
        to receive(:run_command).
        and_raise(BuildNode::Exceptions::KnifeCommandFailed, "Failed to fetch certs")
      expect{local_knife.fetch_ssl_certificates}.
        to raise_error(BuildNode::Exceptions::CertFetchFailed, /.*Failed to fetch certs*/)
    end
  end

    describe '#verify_node_not_legacy' do
      before do
        allow(local_knife).to receive(:delivery_cluster_node?).and_return(true)
      end

      it 'raises LegacyNode error when node is a legacy node' do
        expect(local_knife).to receive(:delivery_cluster_node?).and_return(true)

        expect { local_knife.verify_node_not_legacy }.to raise_error(
          BuildNode::Exceptions::LegacyNode,
          /Delivery build client #{fqdn} is a Delivery Cluster installation already registered with the Chef Server/
        ) do |ex|
          expect(ex.remediation_steps).to match(/.*fresh host*/m)
        end
      end
    end

  describe '#verify_node_not_registered' do
    before do
      allow(local_knife).to receive(:client_exists?).and_return(false)
      allow(local_knife).to receive(:node_exists?).and_return(false)
    end

    it "returns without checking for node/client if overwrite: true is passed in" do
      expect(local_knife).to_not receive(:client_exists?)
      expect(local_knife).to_not receive(:node_exists?)

      local_knife.verify_node_not_registered(overwrite: true)
    end

    it "raises BuildNode::Exceptions::ClientExists when a client already exists" do
      expect(local_knife).to receive(:client_exists?).and_return(true)

      expect{ local_knife.verify_node_not_registered}.to raise_error(
        BuildNode::Exceptions::ClientExists,
        /Delivery build client #{fqdn} is already registered with the Chef Server/
      ) do |ex|
        expect(ex.remediation_steps).to match(/.*knife client delete*/m)
      end
    end

    it "raises BuildNode::Exceptions::NodeExists when a client already exists" do
      expect(local_knife).to receive(:node_exists?).and_return(true)
      expect{ local_knife.verify_node_not_registered}.to raise_error(
        BuildNode::Exceptions::NodeExists,
        /Delivery build node #{fqdn} is already registered with the Chef Server/
      ) do |ex|
        expect(ex.remediation_steps).to match(/.*knife node delete*/m)
      end
    end
  end
end
