#
# Copyright 2015 Shawn Neal <sneal@sneal.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'erubi'
require 'winrm' unless defined?(WinRM::Connection)
require 'winrm-fs'
require 'securerandom' unless defined?(SecureRandom)
require 'stringio'

module WinRM
  module Shells
    # Runs PowerShell commands elevated via a scheduled task
    class Elevated
      # Create a new elevated shell
      # @param connection_opts [ConnectionOpts] The WinRM connection options
      # @param transport [HttpTransport] The WinRM SOAP transport
      # @param logger [Logger] The logger to log diagnostic messages to
      def initialize(connection_opts, transport, logger)
        @logger = logger
        @username = connection_opts[:user]
        @password = connection_opts[:password]
        @interactive_logon = false
        @shell = Powershell.new(connection_opts, transport, logger)
        @winrm_file_transporter = WinRM::FS::Core::FileTransporter.new(@shell)
        @execution_timeout = 86_400
      end

      # @return [String] The admin user name to execute the scheduled task as
      attr_accessor :username

      # @return [String] The admin user password
      attr_accessor :password

      # @return [Bool] Using an interactive logon
      attr_accessor :interactive_logon

      # @return [Integer] Timeout for the task to be executed
      attr_accessor :execution_timeout

      # Run a command or PowerShell script elevated without any of the
      # restrictions that WinRM puts in place.
      #
      # @param [String] The command or PS script to wrap in a scheduled task
      #
      # @return [WinRM::Output] :stdout and :stderr
      def run(command, &block)
        # if an IO object is passed read it, otherwise assume the contents of the file were passed
        script_text = command.respond_to?(:read) ? command.read : command

        script_path = upload_elevated_shell_script(script_text)
        wrapped_script = wrap_in_scheduled_task(script_path, username, password)
        @shell.run(wrapped_script, &block)
      end

      # Closes the shell if one is open
      def close
        @shell.close
      end

      private

      def upload_elevated_shell_script(script_text)
        elevated_shell_path = 'c:/windows/temp/winrm-elevated-shell-' + SecureRandom.uuid + '.ps1'
        # Prepend the content of the file with an UTF-8 BOM for Windows to read it as such instead of the default
        # Windows-XXXX encoding, and convert script_text accordingly if needed.
        script_text_with_exit = "\uFEFF#{script_text.encode(Encoding::UTF_8)}\r\n$Host.SetShouldExit($LASTEXITCODE)"
        @winrm_file_transporter.upload(StringIO.new(script_text_with_exit), elevated_shell_path)
        elevated_shell_path
      end

      def elevated_shell_script_content
        IO.read(File.expand_path('../../winrm-elevated/scripts/elevated_shell.ps1', __dir__))
      end

      def wrap_in_scheduled_task(script_path, username, password)
        context = {
          username: username,
          password: password,
          script_path: script_path,
          interactive_logon: interactive_logon,
          execution_timeout: execution_timeout
        }

        b = binding
        locals = context.collect { |k, _| "#{k} = context[#{k.inspect}]; " }
        b.eval(locals.join)
        b.eval(Erubi::Engine.new(elevated_shell_script_content).src)
      end
    end
  end
end
