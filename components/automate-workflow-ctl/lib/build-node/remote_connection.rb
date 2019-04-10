require 'train'
require 'train/transports/ssh'

require 'build-node/exceptions'

# Connects to a remote node using SSH. Allows you to run arbitrary commands and
# copy files from the local machine to the remote machine.

module BuildNode
  class RemoteConnection

    def initialize(config, logger)
      @config = config
      @logger = logger
    end

    def connect!
      @train = Train.create('ssh', connect_options)
      @connection ||= @train.connection
      nil
    rescue Train::ClientError => e
      raise BuildNode::Exceptions::RemoteConnectionFailed.new(
        "Error received connecting to '#{@config.fqdn}': #{e.message}")
    rescue Train::Transports::SSHFailed
      ssh_failed!
    rescue SocketError
      raise BuildNode::Exceptions::RemoteConnectionFailed.new(
        "Error connecting to '#{@config.fqdn}'.",
        "Please verify the FQDN is correct, and that the host is available."
      )
    end

    def os_name
      @os ||= connection.os
      @os[:name]
    end

    def run_command(command)
      result = connection.run_command(command)
      if result.exit_status != 0
        @logger.error("Remote execution of '#{command}' failed.")
        @logger.error("STDOUT:")
        @logger.error(result.stdout)
        @logger.error("STDERR:")
        @logger.error(result.stderr)
      raise BuildNode::Exceptions::RemoteExecutionFailed,
        "Remote '#{command}' exited with #{result.exit_status}."
      end
    rescue Train::Transports::SSHFailed
      ssh_failed!
    end

    def copy_file(from, to)
      raise BuildNode::Exceptions::RemoteCopyFailed, "File '#{from}' does not exist. Please verify the path provided." unless File.exist?(from)
      connection.upload(from, to)
    rescue Train::Transports::SSHFailed => e
      logger.error("Copy of #{from} to #{to} failed. Reason: #{e.message}")
      raise BuildNode::Exceptions::RemoteCopyFailed,
        "Failed to upload file #{from} to #{to}."
    end

    private

    def connect_options
      h = { host: @config.fqdn,
            port: @config.port,
            user: @config.username,
            key_files: @config.ssh_identity_file,
            logger: @logger,
            pty: true,
            sudo: true }

      if @config.password
        h[:password] = @config.password unless @config.ssh_identity_file
        h[:sudo_password] = @config.password
      end

      h
    end

    def connection
      @connection or raise BuildNode::Exceptions::NoConnection.new(
        "No connection established." \
        " Call BuildNode::RemoteConnection#connect! to establish a connection."
      )
    end

    def ssh_failed!
      ssh_command = ["ssh #{@config.username}@#{@config.fqdn}"]
      ssh_command << "-p #{@config.port}" if @config.port != 22
      ssh_command << "-i #{@config.ssh_identity_file}" if @config.ssh_identity_file

      raise BuildNode::Exceptions::RemoteConnectionFailed.new(
        "SSH connection failed for '#{@config.fqdn}'.",
        "Please ensure '#{ssh_command.join(' ')}' succeeds before proceeding.")
    end
  end
end
