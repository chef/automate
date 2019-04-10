require "spec_helper.rb"
require 'ostruct'

require 'build-node/exceptions'
require 'build-node/remote_connection'

context BuildNode::RemoteConnection do

  ConfigClass = Struct.new(:username, :password, :fqdn, :port, :ssh_identity_file)
  let(:logger) { double("logger") }
  let(:config) { ConfigClass.new("a_user", "its_password", "example.com", 22, nil) }
  let(:mock_connection) { double("Train::Transports::SSH::Connection") }

  describe "#connect!" do
    let(:remote_connection) { BuildNode::RemoteConnection.new(config, logger) }

    it "establishes an ssh connection on the specified port when a different port is provided" do
      config.port = 122 # Differ from default so we can verify it
      expect(Train).to receive(:create).
        with('ssh', hash_including(port: 122)).
        and_return(double("Train::Transports::SSH", connection: mock_connection))
      remote_connection.connect!
    end

    it "establishes an ssh connection with an identity files when an identity file is provided" do
      config.password = nil # OK because we are using an identity file
      config.ssh_identity_file = "a/key/file"
      expect(Train).to receive(:create).
        with('ssh', hash_including(key_files: "a/key/file")).
        and_return(double("Train::Transports::SSH", connection: mock_connection))
      remote_connection.connect!
    end

    context "when given an ssh key but not a password" do
      let(:config) { ConfigClass.new("a_user", nil, "example.com", 22, "ssh_identity_file") }

      it "does not include the password or sudo_password keys" do
        expect(Train).to receive(:create).
                           with('ssh', hash_not_including(:password, :sudo_password)).
                           and_return(double("Train::Transports::SSH", connection: mock_connection))
        remote_connection.connect!
      end
    end

    context "when given an ssh key and a password" do
      let(:config) { ConfigClass.new("a_user", "its_password", "example.com", 22, "ssh_identity_file") }
      let(:connect_options) {
        {
          host: "example.com",
          port: 22,
          user: "a_user",
          key_files: "ssh_identity_file",
          logger: logger,
          pty: true,
          sudo: true,
          sudo_password: "its_password"
        }
      }

      it "does not include the password key but does include the sudo_password key" do
        expect(Train).to receive(:create).
                           with('ssh', connect_options).
                           and_return(double("Train::Transports::SSH", connection: mock_connection))
        remote_connection.connect!
      end
    end

    it "creates the actual ssh connection" do
      expect(Train).to receive(:create).
        with('ssh', anything()).
        and_return(mock_connection)
      expect(mock_connection).to receive(:connection)
      remote_connection.connect!
    end

    context "raises BuildNode::Exceptions::RemoteConnectionFailed error when" do
      it "ssh credentials are invalid" do
        expect(Train).to receive(:create).and_raise(
          Train::ClientError,
          /.*client error message.*/
        )
        expect{ remote_connection.connect! }.to raise_error(
          BuildNode::Exceptions::RemoteConnectionFailed,
          /.*client error message.*/
        )
      end

      it "ssh connection attempt times out" do
        # Timeout/other errors are transport-specific
        expect(Train).to receive(:create).and_raise(Train::Transports::SSHFailed)
        expect{ remote_connection.connect! }.to raise_error(BuildNode::Exceptions::RemoteConnectionFailed) do |ex|
          expect(ex.message).to eq("SSH connection failed for '#{config.fqdn}'.")
          expect(ex.remediation_steps).to match(/Please ensure 'ssh #{config.username}@#{config.fqdn}.* succeeds before proceeding./)
        end
      end

      it "Socket error is returned because of an invalid fqdn" do
        expect(Train).to receive(:create).and_raise(SocketError)
        expect{ remote_connection.connect! }.to raise_error(BuildNode::Exceptions::RemoteConnectionFailed) do |ex|
          expect(ex.message).to eq("Error connecting to '#{config.fqdn}'.")
          expect(ex.remediation_steps).to eq("Please verify the FQDN is correct, and that the host is available.")
        end
      end
    end
  end

  describe "#run_command" do
    let(:remote_connection) { BuildNode::RemoteConnection.new(config, logger) }

    it "Runs the command specified using Train and checks the result" do
      allow(remote_connection).to receive(:connection).and_return(mock_connection)
      response_double = double("command")
      expect(mock_connection).to receive(:run_command).with("a command").and_return(response_double)
      expect(response_double).to receive(:exit_status).and_return 0
      remote_connection.run_command("a command")
    end

    it "raises BuildNode::Exceptions::NoConnection when a connection has not been established" do
      expect{ remote_connection.run_command("test") }.to raise_error(
        BuildNode::Exceptions::NoConnection,
        /No connection established. Call BuildNode::RemoteConnection#connect! to establish a connection./
      )
    end

    it "raises BuildNode::Exceptions::RemoteExecutionFailed when the command exits nonzero" do
      command = "test"
      exitstatus = 127
      result = double("run_command result", exit_status: exitstatus, stdout: "", stderr: "")

      allow(remote_connection).to receive(:connection).and_return(mock_connection)
      expect(mock_connection).to receive(:run_command).with(command).and_return(result)
      expect(logger).to receive(:error).with(/Remote execution of 'test' failed/)
      allow(logger).to receive(:error)
      expect{ remote_connection.run_command(command) }.to raise_error(
        BuildNode::Exceptions::RemoteExecutionFailed,
        "Remote '#{command}' exited with #{exitstatus}."
      )
    end

    it "raises BuildNode::Exceptions::RemoteConnectionFailed when ssh times out" do
      allow(remote_connection).to receive(:connection).and_return(mock_connection)
      expect(mock_connection).to receive(:run_command).and_raise(Train::Transports::SSHFailed)
      expect{ remote_connection.run_command("test") }.to raise_error(BuildNode::Exceptions::RemoteConnectionFailed) do |ex|
        expect(ex.message).to eq("SSH connection failed for '#{config.fqdn}'.")
        expect(ex.remediation_steps).to match(/Please ensure 'ssh #{config.username}@#{config.fqdn}.* succeeds before proceeding./)
      end
    end
  end

  describe "#copy_file" do
    let(:from_file) { "copy/from/path" }
    let(:to_file) { "copy/to/path" }
    let(:remote_connection) { BuildNode::RemoteConnection.new(config, logger) }

    before(:each) do
      allow(File).to receive(:exist?).and_return(true)
    end

    it "raises BuildNode::Exceptions::NoConnection when a connection has not been established" do
      expect{ remote_connection.copy_file(from_file, to_file) }.to raise_error(
        BuildNode::Exceptions::NoConnection,
        /No connection established. Call BuildNode::RemoteConnection#connect! to establish a connection./
      )
    end

    context "when a connection has been established" do
      before(:each) do
        allow(remote_connection).to receive(:connection).and_return(mock_connection)
      end

      it "Uploads the file to the remote host via the Train connection" do
        expect(File).to receive(:exist?).with(from_file).and_return(true)
        expect(mock_connection).to receive(:upload).with(from_file, to_file)
        remote_connection.copy_file(from_file, to_file)
      end

      it "raises BuildNode::Exceptions::RemoteCopyFailed when the local file does not exist" do
        expect(File).to receive(:exist?).with(from_file).and_return(false)
        expect{ remote_connection.copy_file(from_file, to_file) }.to raise_error(
          BuildNode::Exceptions::RemoteCopyFailed,
          /File '#{from_file}' does not exist. Please verify the path provided./
        )
      end
    end
  end

  describe "#os_name" do
    let(:remote_connection) { BuildNode::RemoteConnection.new(config, logger) }

    it "returns the remote node's operating system family" do
      allow(remote_connection).to receive(:connection).and_return(mock_connection)
      allow(mock_connection).to receive(:os).and_return({name: 'ubuntu'})
      expect(remote_connection.os_name).to eq('ubuntu')
    end

    it "raises BuildNode::Exceptions::NoConnection when a connection has not been established" do
      expect{ remote_connection.os_name }.to raise_error(
        BuildNode::Exceptions::NoConnection,
        /No connection established. Call BuildNode::RemoteConnection#connect! to establish a connection./
      )
    end
  end
end
