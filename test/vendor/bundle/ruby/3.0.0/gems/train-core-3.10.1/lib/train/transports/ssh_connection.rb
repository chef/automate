#
# Author:: Fletcher Nichol (<fnichol@nichol.ca>)
# Author:: Dominik Richter (<dominik.richter@gmail.com>)
# Author:: Christoph Hartmann (<chris@lollyrock.com>)
#
# Copyright (C) 2014, Fletcher Nichol
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require "net/ssh" unless defined?(Net::SSH)
require "net/scp"
require "timeout" unless defined?(Timeout)

class Train::Transports::SSH
  # A Connection instance can be generated and re-generated, given new
  # connection details such as connection port, hostname, credentials, etc.
  # This object is responsible for carrying out the actions on the remote
  # host such as executing commands, transferring files, etc.
  #
  # @author Fletcher Nichol <fnichol@nichol.ca>
  class Connection < BaseConnection # rubocop:disable Metrics/ClassLength
    attr_reader   :hostname
    attr_accessor :transport_options

    # If we use the GNU timeout utility to timout a command server-side, it will
    # exit with this status code if the command timed out.
    GNU_TIMEOUT_EXIT_STATUS = 124

    def initialize(options)
      # Track IOS command retries to prevent infinite loop on IOError. This must
      # be done before `super()` because the parent runs detection commands.
      @ios_cmd_retries = 0

      super(options)

      @session                = nil
      @username               = @options.delete(:username)
      @hostname               = @options.delete(:hostname)
      @port                   = @options[:port] # don't delete from options
      @connection_retries     = @options.delete(:connection_retries)
      @connection_retry_sleep = @options.delete(:connection_retry_sleep)
      @max_wait_until_ready   = @options.delete(:max_wait_until_ready)
      @max_ssh_sessions       = @options.delete(:max_ssh_connections) { 9 }
      @transport_options      = @options.delete(:transport_options)
      @proxy_command          = @options.delete(:proxy_command)
      @bastion_host           = @options.delete(:bastion_host)
      @bastion_user           = @options.delete(:bastion_user)
      @bastion_port           = @options.delete(:bastion_port)

      @cmd_wrapper = CommandWrapper.load(self, @transport_options)
    end

    # (see Base::Connection#close)
    def close
      return if @session.nil?

      logger.debug("[SSH] closing connection to #{self}")
      session.close
    ensure
      @session = nil
    end

    def ssh_opts
      level = logger.debug? ? "VERBOSE" : "ERROR"
      fwd_agent = options[:forward_agent] ? "yes" : "no"

      args  = %w{ -o UserKnownHostsFile=/dev/null }
      args += %w{ -o StrictHostKeyChecking=no }
      args += %w{ -o BatchMode=yes } if options[:non_interactive]
      args += %W{ -o LogLevel=#{level} }
      args += %W{ -o ForwardAgent=#{fwd_agent} } if options.key?(:forward_agent)

      keys = Array(options[:keys])
      unless keys.empty?
        args += %w{ -o IdentitiesOnly=yes }
        keys.each do |ssh_key|
          args += %W{ -i #{ssh_key} }
        end
      end

      args
    end

    def check_proxy
      [@proxy_command, @bastion_host].any? { |type| !type.nil? }
    end

    def generate_proxy_command
      return @proxy_command unless @proxy_command.nil?

      args = %w{ ssh }
      args += ssh_opts
      args += %W{ #{@bastion_user}@#{@bastion_host} }
      args += %W{ -p #{@bastion_port} }
      args += %w{ -W %h:%p }
      args.join(" ")
    end

    # (see Base::Connection#login_command)
    def login_command
      args = ssh_opts
      args += %W{ -o ProxyCommand='#{generate_proxy_command}' } if check_proxy
      args += %W{ -p #{@port} }
      args += %W{ #{@username}@#{@hostname} }
      LoginCommand.new("ssh", args)
    end

    # (see Base::Connection#upload)
    def upload(locals, remote)
      waits = []
      Array(locals).each do |local|
        opts = File.directory?(local) ? { recursive: true } : {}

        waits.push session.scp.upload(local, remote, opts) do |_ch, name, sent, total|
          logger.debug("Uploaded #{name} (#{total} bytes)") if sent == total
        end
        waits.shift.wait while waits.length >= @max_ssh_sessions
      end
      waits.each(&:wait)
    rescue Net::SSH::Exception => ex
      raise Train::Transports::SSHFailed, "SCP upload failed (#{ex.message})"
    end

    def download(remotes, local)
      waits = []
      Array(remotes).map do |remote|
        opts = file(remote).directory? ? { recursive: true } : {}
        waits.push session.scp.download(remote, local, opts) do |_ch, name, recv, total|
          logger.debug("Downloaded #{name} (#{total} bytes)") if recv == total
        end
        waits.shift.wait while waits.length >= @max_ssh_sessions
      end
      waits.each(&:wait)
    rescue Net::SSH::Exception => ex
      raise Train::Transports::SSHFailed, "SCP download failed (#{ex.message})"
    end

    # (see Base::Connection#wait_until_ready)
    def wait_until_ready
      delay = 3
      session(
        retries: @max_wait_until_ready / delay,
        delay:   delay,
        message: "Waiting for SSH service on #{@hostname}:#{@port}, " \
                 "retrying in #{delay} seconds"
      )
      run_command(PING_COMMAND.dup)
    end

    def uri
      "ssh://#{@username}@#{@hostname}:#{@port}"
    end

    # remote_port_forwarding
    def forward_remote(port, host, remote_port, remote_host = "127.0.0.1")
      @session.forward.remote(port, host, remote_port, remote_host)
    end

    def obscured_options
      options_to_print = @options.clone
      options_to_print[:password] = "<hidden>" if options_to_print.key?(:password)
      options_to_print
    end

    def with_sudo_pty
      old_pty = transport_options[:pty]
      transport_options[:pty] = true if @sudo

      yield
    ensure
      transport_options[:pty] = old_pty
    end

    private

    PING_COMMAND = "echo '[SSH] Established'".freeze

    RESCUE_EXCEPTIONS_ON_ESTABLISH = [
      Errno::EACCES, Errno::EADDRINUSE, Errno::ECONNREFUSED, Errno::ETIMEDOUT,
      Errno::ECONNRESET, Errno::ENETUNREACH, Errno::EHOSTUNREACH, Errno::EPIPE,
      Net::SSH::Disconnect, Net::SSH::AuthenticationFailed, Net::SSH::ConnectionTimeout,
      Timeout::Error
    ].freeze

    # Establish an SSH session on the remote host.
    #
    # @param opts [Hash] retry options
    # @option opts [Integer] :retries the number of times to retry before
    #   failing
    # @option opts [Float] :delay the number of seconds to wait until
    #   attempting a retry
    # @option opts [String] :message an optional message to be logged on
    #   debug (overriding the default) when a rescuable exception is raised
    # @return [Net::SSH::Connection::Session] the SSH connection session
    # @api private
    def establish_connection(opts)
      logger.debug("[SSH] opening connection to #{self}")
      logger.debug("[SSH] using options %p" % [obscured_options])
      if check_proxy
        require "net/ssh/proxy/command"
        @options[:proxy] = Net::SSH::Proxy::Command.new(generate_proxy_command)
      end
      Net::SSH.start(@hostname, @username, @options.clone.delete_if { |_key, value| value.nil? })
    rescue *RESCUE_EXCEPTIONS_ON_ESTABLISH => e
      if (opts[:retries] -= 1) <= 0
        logger.warn("[SSH] connection failed, terminating (#{e.inspect})")
        raise Train::Transports::SSHFailed, "SSH session could not be established"
      end

      if opts[:message]
        logger.debug("[SSH] connection failed (#{e.inspect})")
        message = opts[:message]
      else
        message = "[SSH] connection failed, retrying in #{opts[:delay]}"\
                  " seconds (#{e.inspect})"
      end
      logger.info(message)

      sleep(opts[:delay])
      retry
    end

    def file_via_connection(path, *args)
      if os.aix?
        Train::File::Remote::Aix.new(self, path, *args)
      elsif os.solaris?
        Train::File::Remote::Unix.new(self, path, *args)
      elsif os[:name] == "qnx"
        Train::File::Remote::Qnx.new(self, path, *args)
      elsif os.windows?
        Train::File::Remote::Windows.new(self, path, *args)
      else
        Train::File::Remote::Linux.new(self, path, *args)
      end
    end

    def run_command_via_connection(cmd, opts = {}, &data_handler)
      cmd.dup.force_encoding("binary") if cmd.respond_to?(:force_encoding)

      reset_session if session.closed?

      exit_status, stdout, stderr = execute_on_channel(cmd, opts, &data_handler)

      # An interactive console might contain the STDERR in STDOUT
      # concat both outputs for non-zero exit status. 
      output = "#{stdout} #{stderr}".strip if exit_status != 0

      # Abstract the su - USER authentication failure
      # raise the Train::UserError and passes message & reason
      if output && output.match?("su: Authentication failure")
        raise Train::UserError.new(output, :bad_su_user_password)
      end

      # Since `@session.loop` succeeded, reset the IOS command retry counter
      @ios_cmd_retries = 0

      CommandResult.new(stdout, stderr, exit_status)
    rescue Net::SSH::Exception => ex
      raise Train::Transports::SSHFailed, "SSH command failed (#{ex.message})"
    rescue IOError
      # Cisco IOS occasionally closes the stream prematurely while we are
      # running commands to detect if we need to switch to the Cisco IOS
      # transport. This retries the command if this is the case.
      # See:
      #  https://github.com/inspec/train/pull/271
      logger.debug("[SSH] Possible Cisco IOS race condition, retrying command")

      # Only attempt retry up to 5 times to avoid infinite loop
      @ios_cmd_retries += 1
      raise if @ios_cmd_retries >= 5

      retry
    end

    # Returns a connection session, or establishes one when invoked the
    # first time.
    #
    # @param retry_options [Hash] retry options for the initial connection
    # @return [Net::SSH::Connection::Session] the SSH connection session
    # @api private
    def session(retry_options = {})
      @session ||= establish_connection({
        retries: @connection_retries.to_i,
        delay: @connection_retry_sleep.to_i,
      }.merge(retry_options))
    end

    def reset_session
      @session = nil
    end

    # String representation of object, reporting its connection details and
    # configuration.
    #
    # @api private
    def to_s
      "#{@username}@#{@hostname}"
    end

    # Given a channel and a command string, it will execute the command on the channel
    # and accumulate results in  @stdout/@stderr.
    #
    # @param channel [Net::SSH::Connection::Channel] an open ssh channel
    # @param cmd [String] the command to execute
    # @return [Integer] exit status or nil if exit-status/exit-signal requests
    #         not received.
    #
    # @api private
    def execute_on_channel(cmd, opts)
      timeout = opts[:timeout]&.to_i
      stdout = ""
      stderr = ""
      exit_status = nil
      session.open_channel do |channel|
        # wrap commands if that is configured
        cmd = @cmd_wrapper.run(cmd) if @cmd_wrapper

        # Timeout the command if requested and able
        if timeout && timeoutable?(cmd)
          # if cmd start with sudo then we need to make sure the timeout should be prepend with sudo else actual timeout is not working.
          if cmd.strip.split[0] == "sudo"
            split_cmd = cmd.strip.split
            split_cmd[0] = "sudo timeout #{timeout}s"
            cmd = split_cmd.join(" ")
          else
            cmd = "timeout #{timeout}s #{cmd}"
          end
        end

        logger.debug("[SSH] #{self} cmd = #{cmd}")

        if @transport_options[:pty]
          channel.request_pty do |_ch, success|
            raise Train::Transports::SSHPTYFailed, "Requesting PTY failed" unless success
          end
        end

        channel.exec(cmd) do |_, success|
          abort "Couldn't execute command on SSH." unless success
          channel.on_data do |_, data|
            yield(data, channel) if block_given?
            stdout += data
          end

          channel.on_extended_data do |_, _type, data|
            yield(data, channel) if block_given?
            stderr += data
          end

          channel.on_request("exit-status") do |_, data|
            exit_status = data.read_long
          end

          channel.on_request("exit-signal") do |_, data|
            exit_status = data.read_long
          end
        end
      end
      session.loop

      if timeout && timeoutable?(cmd) && exit_status == GNU_TIMEOUT_EXIT_STATUS
        logger.debug("train ssh command '#{cmd}' reached requested timeout (#{timeout}s)")
        session.channels.each_value { |c| c.eof!; c.close }
        raise Train::CommandTimeoutReached.new "ssh command reached timeout (#{timeout}s)"
      end

      [exit_status, stdout, stderr]
    end

    # Returns true if we think we can attempt to timeout the command
    def timeoutable?(cmd)
      have_timeout_cli? && !cmd.include?("|") # Don't try to timeout a command that has pipes
    end

    # Returns true if the GNU timeout command is available
    def have_timeout_cli?
      return @have_timeout_cli unless @have_timeout_cli.nil?

      res = session.exec!("timeout --version")
      @have_timeout_cli = res.exitstatus == 0
      logger.debug("train ssh have_timeout_cli status is '#{@have_timeout_cli}'")
      @have_timeout_cli
    end
  end
end
