require "docker"
require "train/plugins"

module Train::Transports
  class Podman < Train.plugin(1)

    name "podman"

    include_options Train::Extras::CommandWrapper
    option :host, required: true
    option :podman_url, required: false

    def connection(state = {}, &block)
      opts = merge_options(options, state || {})

      validate_options(opts)

      if @connection && @connection_options == opts
        reuse_connection(&block)
      else
        create_new_connection(opts, &block)
      end
    end

    private

    # Creates a new Podman connection instance and save it for potential future
    # reuse.
    #
    # @param options [Hash] connection options
    # @return [Podman::Connection] a Podman connection instance
    # @api private
    def create_new_connection(options, &block)
      if @connection
        logger.debug("[Podman] shutting previous connection #{@connection}")
        @connection.close
      end

      @connection_options = options
      @connection = Connection.new(options, &block)
    end

    # Return the last saved Podman connection instance.
    #
    # @return [Podman::Connection] a Podman connection instance
    # @api private
    def reuse_connection
      logger.debug("[Podman] reusing existing connection #{@connection}")
      yield @connection if block_given?
      @connection
    end
  end

  class Train::Transports::Podman
    class Connection < BaseConnection
      def initialize(options)
        super(options)

        @id = options[:host]

        if RUBY_PLATFORM =~ /windows|mswin|msys|mingw|cygwin/
          raise "Unsupported host platform."
        end

        # Currently Podman url can be set using option and setting the environment variable.
        uid = Process.uid
        podman_url = options[:podman_url] || ENV["CONTAINER_HOST"]
        podman_url ||= "unix:///run/podman/podman.sock" if uid == 0
        podman_url ||= "unix:///run/user/#{uid}/podman/podman.sock"

        Docker.url = podman_url

        # Using docker-api ruby library to fetch the Podman container data.
        @container = ::Docker::Container.get(@id) ||
          raise("Can't find Podman container #{@id}")
        @cmd_wrapper = nil
        @cmd_wrapper = CommandWrapper.load(self, @options)
        @probably_windows = nil
      rescue Excon::Error::Socket
        raise Train::TransportError, "Unable to connect to Podman using #{podman_url}"
      rescue Docker::Error::NotFoundError => e
        raise Train::TransportError, "Container Not Found: #{e.message}"
      rescue Docker::Error::ServerError => e
        raise Train::TransportError, "#{e.message}"
      end

      def close
        # nothing to do at the moment
      end

      def uri
        if @container.nil?
          "podman://#{@id}"
        else
          "podman://#{@container.id}"
        end
      end

      def unique_identifier
        @container.nil? ? @id : @container.id # default uuid set to the podman host.
      end

      private

      def file_via_connection(path)
        if os.aix?
          Train::File::Remote::Aix.new(self, path)
        elsif os.solaris?
          Train::File::Remote::Unix.new(self, path)
        elsif os.windows?
          Train::File::Remote::Windows.new(self, path)
        else
          Train::File::Remote::Linux.new(self, path)
        end
      end

      def run_command_via_connection(cmd, &_data_handler)
        cmd = @cmd_wrapper.run(cmd) unless @cmd_wrapper.nil?

        # Cannot use os.windows? here because it calls run_command_via_connection,
        # causing infinite recursion during initial platform detection
        if sniff_for_windows?
          invocation = cmd_run_command(cmd)
        else
          invocation = sh_run_command(cmd)
        end
        stdout, stderr, exit_status = @container.exec(
          invocation, user: @options[:user]
        )
        CommandResult.new(stdout.join, stderr.join, exit_status)
      rescue ::Docker::Error::DockerError => _
        raise
      rescue => _
        # @TODO: differentiate any other error
        raise
      end

      def sh_run_command(cmd)
        ["/bin/sh", "-c", cmd]
      end

      def cmd_run_command(cmd)
        ["cmd.exe", "/s", "/c", cmd]
      end

      def sniff_for_windows?
        return @probably_windows unless @probably_windows.nil?

        # Run a command using /bin/sh, which should fail under Windows
        stdout, _stderr, _exit_status = @container.exec(
          sh_run_command("true"), user: @options[:user]
        )
        @probably_windows = !!stdout.detect { |l| l.include? "failure in a Windows system call" }
      end
    end
  end
end