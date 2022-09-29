require "docker"

module Train::Transports
  class Docker < Train.plugin(1)
    name "docker"

    include_options Train::Extras::CommandWrapper
    option :host, required: true
    option :docker_url, required: false

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

    # Creates a new Docker connection instance and save it for potential future
    # reuse.
    #
    # @param options [Hash] connection options
    # @return [Docker::Connection] a Docker connection instance
    # @api private
    def create_new_connection(options, &block)
      if @connection
        logger.debug("[Docker] shutting previous connection #{@connection}")
        @connection.close
      end

      @connection_options = options
      @connection = Connection.new(options, &block)
    end

    # Return the last saved Docker connection instance.
    #
    # @return [Docker::Connection] a Docker connection instance
    # @api private
    def reuse_connection
      logger.debug("[Docker] reusing existing connection #{@connection}")
      yield @connection if block_given?
      @connection
    end
  end
end

class Train::Transports::Docker
  class Connection < BaseConnection
    def initialize(conf)
      super(conf)
      @id = options[:host]

      docker_url = options[:docker_url]
      if RUBY_PLATFORM =~ /windows|mswin|msys|mingw|cygwin/
        # Docker Desktop for windows. Must override socket location.
        # https://docs.docker.com/desktop/faqs/#how-do-i-connect-to-the-remote-docker-engine-api
        # docker_socket ||= "npipe:////./pipe/docker_engine" # # Doesn't require a settings change, but also doesn't work
        docker_url ||= "tcp://localhost:2375"
      end
      Docker.url = docker_url if docker_url

      @container = ::Docker::Container.get(@id) ||
        raise("Can't find Docker container #{@id}")
      @cmd_wrapper = nil
      @cmd_wrapper = CommandWrapper.load(self, @options)
      @probably_windows = nil
    end

    def close
      # nothing to do at the moment
    end

    def uri
      if @container.nil?
        "docker://#{@id}"
      else
        "docker://#{@container.id}"
      end
    end

    def unique_identifier
      uuid = @container.nil? ? @id : @container.id # default uuid set to the docker host.
      unless sniff_for_windows?
        cmd = run_command_via_connection("head -1 /proc/self/cgroup|cut -d/ -f3") if file("/proc/self/cgroup").exist?
        unless cmd.stdout.empty?
          uuid = cmd.stdout.strip
        end
      end
      uuid
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
