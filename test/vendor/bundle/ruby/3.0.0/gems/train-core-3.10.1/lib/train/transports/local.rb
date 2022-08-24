#
# author: Dominik Richter
# author: Christoph Hartmann

require_relative "../plugins"
require_relative "../errors"
require "mixlib/shellout" unless defined?(Mixlib::ShellOut)

module Train::Transports
  class Local < Train.plugin(1)
    name "local"

    class PipeError < Train::TransportError; end

    def connection(_ = nil)
      @connection ||= Connection.new(@options)
    end

    class Connection < BaseConnection
      def initialize(options)
        super(options)

        @runner = if options[:command_runner]
                    force_runner(options[:command_runner])
                  else
                    select_runner(options)
                  end
      end

      def login_command
        nil # none, open your shell
      end

      def close
        @runner.close
      end

      def uri
        "local://"
      end

      def upload(locals, remote)
        FileUtils.mkdir_p(remote)

        Array(locals).each do |local|
          FileUtils.cp_r(local, remote)
        end
      end

      def download(remotes, local)
        upload(remotes, local)
      end

      private

      def select_runner(options)
        if os.windows?
          # Force a 64 bit poweshell if needed
          if RUBY_PLATFORM == "i386-mingw32" && os.arch == "x86_64"
            powershell_cmd = "#{ENV["SystemRoot"]}\\sysnative\\WindowsPowerShell\\v1.0\\powershell.exe"
          else
            powershell_cmd = "powershell"
          end

          # Attempt to use a named pipe but fallback to ShellOut if that fails
          begin
            WindowsPipeRunner.new(powershell_cmd)
          rescue PipeError
            WindowsShellRunner.new(powershell_cmd)
          end
        else
          GenericRunner.new(self, options)
        end
      end

      def force_runner(command_runner)
        case command_runner
        when :generic
          GenericRunner.new(self, options)
        when :windows_pipe
          WindowsPipeRunner.new
        when :windows_shell
          WindowsShellRunner.new
        else
          raise "Runner type `#{command_runner}` not supported"
        end
      end

      def run_command_via_connection(cmd, opts, &_data_handler)
        # Use the runner if it is available
        return @runner.run_command(cmd, opts) if defined?(@runner)

        # If we don't have a runner, such as at the beginning of setting up the
        # transport and performing the first few steps of OS detection, fall
        # back to shelling out.
        res = Mixlib::ShellOut.new(cmd)
        res.run_command
        Local::CommandResult.new(res.stdout, res.stderr, res.exitstatus)
      rescue Errno::ENOENT => _
        CommandResult.new("", "", 1)
      end

      def file_via_connection(path)
        if os.windows?
          Train::File::Local::Windows.new(self, path)
        else
          Train::File::Local::Unix.new(self, path)
        end
      end

      class GenericRunner
        include_options Train::Extras::CommandWrapper

        def initialize(connection, options)
          @cmd_wrapper = Local::CommandWrapper.load(connection, options)
        end

        def run_command(cmd, opts = {})
          if defined?(@cmd_wrapper) && !@cmd_wrapper.nil?
            cmd = @cmd_wrapper.run(cmd)
          end

          res = Mixlib::ShellOut.new(cmd)
          res.timeout = opts[:timeout]
          begin
            res.run_command
          rescue Mixlib::ShellOut::CommandTimeout
            raise Train::CommandTimeoutReached
          end
          Local::CommandResult.new(res.stdout, res.stderr, res.exitstatus)
        end

        def close
          # nothing to do at the moment
        end
      end

      class WindowsShellRunner
        require "json" unless defined?(JSON)
        require "base64" unless defined?(Base64)

        def initialize(powershell_cmd = "powershell")
          @powershell_cmd = powershell_cmd
        end

        def run_command(script, opts)
          # Prevent progress stream from leaking into stderr
          script = "$ProgressPreference='SilentlyContinue';" + script

          # Encode script so PowerShell can use it
          script = script.encode("UTF-16LE", "UTF-8")
          base64_script = Base64.strict_encode64(script)

          cmd = "#{@powershell_cmd} -NoProfile -EncodedCommand #{base64_script}"

          res = Mixlib::ShellOut.new(cmd)
          res.timeout = opts[:timeout]
          begin
            res.run_command
          rescue Mixlib::ShellOut::CommandTimeout
            raise Train::CommandTimeoutReached
          end
          Local::CommandResult.new(res.stdout, res.stderr, res.exitstatus)
        end

        def close
          # nothing to do at the moment
        end
      end

      class WindowsPipeRunner
        require "json" unless defined?(JSON)
        require "base64" unless defined?(Base64)
        require "securerandom" unless defined?(SecureRandom)

        def initialize(powershell_cmd = "powershell")
          @powershell_cmd = powershell_cmd
          @server_pid = nil
          @pipe = acquire_pipe
          raise PipeError if @pipe.nil?
        end

        # @param  cmd The command to execute
        # @return Local::CommandResult with stdout, stderr and exitstatus
        #         Note that exitstatus ($?) in PowerShell is boolean, but we use a numeric exit code.
        #         A command that succeeds without setting an exit code will have exitstatus 0
        #         A command that exits with an exit code will have that value as exitstatus
        #         A command that fails (e.g. throws exception) before setting an exit code will have exitstatus 1
        def run_command(cmd, _opts)
          script = "$ProgressPreference='SilentlyContinue';" + cmd
          encoded_script = Base64.strict_encode64(script)
          # TODO: no way to safely implement timeouts here.
          begin
            @pipe.puts(encoded_script)
            @pipe.flush
          rescue Errno::EPIPE
            # Retry once if the pipe went away
            begin
              # Maybe the pipe went away, but the server didn't? Reset it, to get a clean start.
              close
            rescue Errno::EIO
              # Ignore - server already went away
            end
            @pipe = acquire_pipe
            raise PipeError if @pipe.nil?

            @pipe.puts(encoded_script)
            @pipe.flush
          end
          res = OpenStruct.new(JSON.parse(Base64.decode64(@pipe.readline)))
          Local::CommandResult.new(res.stdout, res.stderr, res.exitstatus)
        end

        def close
          Process.kill("KILL", @server_pid) unless @server_pid.nil?
          @server_pid = nil
        end

        private

        def acquire_pipe
          require "win32/process"
          pipe_name = "inspec_#{SecureRandom.hex}"

          @server_pid = start_pipe_server(pipe_name)

          # Ensure process is killed when the Train process exits
          at_exit { close rescue Errno::EIO }

          pipe = nil

          # PowerShell needs time to create pipe.
          100.times do
            pipe = open("//./pipe/#{pipe_name}", "r+")
            break
          rescue
            sleep 0.1
          end

          pipe
        end

        def start_pipe_server(pipe_name)
          require "win32/process"

          script = <<-EOF
            $ErrorActionPreference = 'Stop'

            $pipeServer = New-Object System.IO.Pipes.NamedPipeServerStream('#{pipe_name}')
            $pipeReader = New-Object System.IO.StreamReader($pipeServer)
            $pipeWriter = New-Object System.IO.StreamWriter($pipeServer)

            $pipeServer.WaitForConnection()

            # Create loop to receive and process user commands/scripts
            $clientConnected = $true
            while($clientConnected) {
              $input = $pipeReader.ReadLine()
              $command = [System.Text.Encoding]::UTF8.GetString([System.Convert]::FromBase64String($input))

              # Execute user command/script and convert result to JSON
              $scriptBlock = $ExecutionContext.InvokeCommand.NewScriptBlock($command)
              try {
                $stdout = & $scriptBlock | Out-String
                $exit_code = $LastExitCode
                if ($exit_code -eq $null)
                {
                  $exit_code = 0
                }
                $result = @{ 'stdout' = $stdout ; 'stderr' = ''; 'exitstatus' = $exit_code }
              } catch {
                $stderr = $_ | Out-String
                $exit_code = $LastExitCode
                $result = @{ 'stdout' = ''; 'stderr' = $stderr; 'exitstatus' = $exit_code }
              }
              $resultJSON = $result | ConvertTo-JSON

              # Encode JSON in Base64 and write to pipe
              $encodedResult = [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($resultJSON))
              $pipeWriter.WriteLine($encodedResult)
              $pipeWriter.Flush()
            }
          EOF

          utf8_script = script.encode("UTF-16LE", "UTF-8")
          base64_script = Base64.strict_encode64(utf8_script)
          cmd = "#{@powershell_cmd} -NoProfile -ExecutionPolicy bypass -NonInteractive -EncodedCommand #{base64_script}"
          Process.create(command_line: cmd).process_id
        end
      end
    end
  end
end
