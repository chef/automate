require 'spec_helper'

require 'runner/install/config'
require 'runner/exceptions'
require 'runner/local_knife'

describe Runner::LocalKnife do
  # delivery.rb configuration values
  let(:delivery_user) { 'delivery-user' }
  let(:delivery_user_key) { 'delivery-user.pem' }
  let(:chef_server_url) { 'https://chef-server/organizations/delivery-org' }
  let(:chef_server_proxy) { false }
  let(:automate_ssl_port) { 443 }

  # mock objects
  let(:output) { Proc.new { |output| } }
  let(:mock_shell_out) { double("Mixlib::ShellOut") }

  # delivery-ctl configuration values
  let(:username) { 'runner-user' }
  let(:password) { 'runner-user-password' }
  let(:fqdn) { 'runner' }
  let(:ssh_identity_file) { 'id_rsa' }
  let(:sudo) { true }
  let(:client_config) { "minimal_ohai true\n" }
  let(:port) { 22 }

  let(:knife_path) {
    "/opt/delivery/embedded/bin/knife"
  }

  let(:base_knife_options) {
    [ '-u', delivery_user, '-k', delivery_user_key,
      '--server-url', chef_server_url ].join(' ')
  }

  let(:local_knife) { Runner::LocalKnife.new(fqdn) }

  before(:each) do
    delivery_options = {
      chef_username: delivery_user,
      chef_private_key: delivery_user_key,
      chef_server: chef_server_url,
      chef_server_proxy: chef_server_proxy
    }
    allow(CtlHelpers::DeliveryConfig).to receive(:delivery).and_return(delivery_options)
    allow(CtlHelpers::DeliveryConfig).to receive(:delivery_fqdn).and_return('delivery-server.example.com')

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
      allow(mock_shell_out).to receive(:stderr).and_return(nil)
      expect(mock_shell_out).to receive(:error!).
        and_raise(Mixlib::ShellOut::ShellCommandFailed, "FAILURE!!")

      expect{local_knife.run_command("test", "-i", "identity")}.to raise_error(
        Runner::Exceptions::KnifeCommandFailed, "/opt/delivery/embedded/bin/knife test -i identity -u delivery-user -k delivery-user.pem --server-url https://chef-server/organizations/delivery-org returned 100"
      )
    end

    it "raises KnifeCommandFailed error and suppresses password in error output when the exitstatus is nonzero" do
      expected_command = local_knife.create_command("test", "--ssh-password", "'password'")
      hidden_command = local_knife.create_command("test", "--ssh-password", "(hidden)")
      allow(local_knife).to receive(:create_command).
        and_return(expected_command, hidden_command)
      expect(Mixlib::ShellOut).to receive(:new).
        with(expected_command).and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:run_command)
      expect(mock_shell_out).to receive(:exitstatus).and_return(100)
      allow(mock_shell_out).to receive(:stderr).and_return(nil)
      expect(mock_shell_out).to receive(:error!).
        and_raise(Mixlib::ShellOut::ShellCommandFailed, "SENSITIVE OUTPUT!!")

      expect{local_knife.run_command("test", "--ssh-password", "'password'")}.to raise_error(
        Runner::Exceptions::KnifeCommandFailed, "/opt/delivery/embedded/bin/knife test --ssh-password (hidden) -u delivery-user -k delivery-user.pem --server-url https://chef-server/organizations/delivery-org returned 100"
      )
    end

    it "raises KnifeCommandFailed error and adds stderr when the exitstatus is nonzero and stderr is not nil" do
      expected_command = local_knife.create_command("test", "-i", "identity")
      allow(local_knife).to receive(:create_command).
        and_return(expected_command, expected_command)
      expect(Mixlib::ShellOut).to receive(:new).
        with(expected_command).and_return(mock_shell_out)
      expect(mock_shell_out).to receive(:run_command)
      expect(mock_shell_out).to receive(:exitstatus).and_return(100)
      allow(mock_shell_out).to receive(:stderr).and_return("this is a failure\n")
      expect(mock_shell_out).to receive(:error!).
        and_raise(Mixlib::ShellOut::ShellCommandFailed, "FAILURE!!")

      expect{local_knife.run_command("test", "-i", "identity")}.to raise_error(
        Runner::Exceptions::KnifeCommandFailed, /this is a failure/)
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
      ['--ssh-user', username, '--node-name', fqdn, '--yes', '--ssh-identity-file', ssh_identity_file,
       '--ssh-password', "'#{password}'", '--sudo',
        '--use-sudo-password', '--ssh-port', port , /--config-option client_d_dir=/]
    end

    context "when no ssh identity file is given" do
      let(:bootstrap_options) do
        [ '--ssh-user', username, '--node-name', fqdn, '--yes', '--ssh-password', "'#{password}'", '--sudo',
          '--use-sudo-password', '--ssh-port', port , /--config-option client_d_dir=/ ]
      end
      let(:ssh_identity_file) { nil }

      it "uses the provided password for ssh authentication" do
        expect(local_knife).to receive(:run_command).
          with("bootstrap", fqdn, *bootstrap_options)
        local_knife.bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      end
    end

    context "whan an ssh identity file is given and no password is given" do
      let(:bootstrap_options) do
        ['--ssh-user', username, '--node-name', fqdn, '--yes', '--ssh-identity-file', ssh_identity_file, '--sudo',
         '--use-sudo-password', '--ssh-port', port , /--config-option client_d_dir=/]
      end
      let(:password) { nil }

      it "uses the provided identify file for ssh authentication" do
        expect(local_knife).to receive(:run_command).
          with('bootstrap', fqdn, *bootstrap_options)
        local_knife.bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      end
    end

    context "when both an ssh identity file and a password are given" do
      it 'sets the SSH password' do
        expect(local_knife).to receive(:run_command).
          with('bootstrap', fqdn, *bootstrap_options)
        local_knife.bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      end
    end

    context "when sudo is false" do
      let (:sudo) { false }
      let(:bootstrap_options) do
        ['--ssh-user', username, '--node-name', fqdn, '--yes', '--ssh-identity-file', ssh_identity_file,
         '--ssh-password', "'#{password}'", '--ssh-port', port , /--config-option client_d_dir=/]
      end
      it "disables sudo" do
        expect(local_knife).to receive(:run_command).
          with('bootstrap', fqdn, *bootstrap_options)
        local_knife.bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      end
    end

    context "when custom ssh-port is given" do
      let(:port) { 2222 }
      let(:bootstrap_options) do
        ['--ssh-user', username, '--node-name', fqdn, '--yes', '--ssh-identity-file', ssh_identity_file,
       '--ssh-password', "'#{password}'", '--sudo',
        '--use-sudo-password', '--ssh-port', port , /--config-option client_d_dir=/]
      end

      it "uses custom ssh-port while bootstrap" do
        expect(local_knife).to receive(:run_command).
          with("bootstrap", fqdn, *bootstrap_options)
        local_knife.bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      end
    end
  end

  describe '#tag_node' do
    it "tags the node with delivery-job-runner" do
      expect(local_knife).to receive(:run_command).
        with('tag create', [fqdn, "delivery-job-runner"])
      local_knife.tag_node
    end
  end

  describe '#bootstrap' do
    it "verifies the node can be registered, bootstraps it, and tags it" do
      expect(local_knife).to receive(:bootstrap_node).with(username, password, ssh_identity_file, sudo, client_config, port)
      expect(local_knife).to receive(:tag_node)
      local_knife.bootstrap(username, password, ssh_identity_file, sudo, client_config, port)
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
        and_raise(Runner::Exceptions::KnifeCommandFailed, "Failed to fetch certs")
      expect{local_knife.fetch_ssl_certificates}.
        to raise_error(Runner::Exceptions::CertFetchFailed, /.*Failed to fetch certs*/)
    end
  end

    describe '#verify_node_not_legacy' do
      before do
        allow(local_knife).to receive(:delivery_cluster_node?).and_return(true)
      end

      it 'raises LegacyNode error when node is a legacy node' do
        expect(local_knife).to receive(:delivery_cluster_node?).and_return(true)

        expect { local_knife.verify_node_not_legacy }.to raise_error(
          Runner::Exceptions::LegacyNode,
          /Delivery runner client #{fqdn} is a Delivery Cluster installation already registered with the Chef Server/
        ) do |ex|
          expect(ex.remediation_steps).to match(/.*fresh host*/m)
        end
      end
    end

  describe '#node_registration_status' do
    before do
      allow(local_knife).to receive(:client_exists?).and_return(false)
      allow(local_knife).to receive(:node_exists?).and_return(false)
    end

    it "raises Runner::Exceptions::ClientExists when a client already exists" do
      expect(local_knife).to receive(:client_exists?).and_return(true)

      expect(local_knife.node_registration_status).to eq(:client_exists)
    end

    it "raises Runner::Exceptions::NodeExists when a client already exists" do
      expect(local_knife).to receive(:node_exists?).and_return(true)
      expect(local_knife.node_registration_status).to eq(:node_exists)
    end
  end
end
