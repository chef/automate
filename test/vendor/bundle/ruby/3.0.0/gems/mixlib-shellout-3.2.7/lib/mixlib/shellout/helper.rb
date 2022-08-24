#--
# Author:: Daniel DeLeo (<dan@chef.io>)
# Copyright:: Copyright (c) Chef Software Inc.
# License:: Apache License, Version 2.0
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

require_relative "../shellout"
require "chef-utils" unless defined?(ChefUtils)
require "chef-utils/dsl/default_paths"
require "chef-utils/internal"

module Mixlib
  class ShellOut
    module Helper
      include ChefUtils::Internal
      include ChefUtils::DSL::DefaultPaths

      #
      # These APIs are considered public for use in ohai and chef (by cookbooks and plugins, etc)
      # but are considered private/experimental for now for the direct users of mixlib-shellout.
      #
      # You can see an example of how to handle the "dependency injection" in the rspec unit test.
      # That backend API is left deliberately undocumented for now and may not follow SemVer and may
      # break at any time (at least for the rest of 2020).
      #

      def shell_out(*args, **options)
        options = options.dup
        options = __maybe_add_timeout(self, options)
        if options.empty?
          shell_out_compacted(*__clean_array(*args))
        else
          shell_out_compacted(*__clean_array(*args), **options)
        end
      end

      def shell_out!(*args, **options)
        options = options.dup
        options = __maybe_add_timeout(self, options)
        if options.empty?
          shell_out_compacted!(*__clean_array(*args))
        else
          shell_out_compacted!(*__clean_array(*args), **options)
        end
      end

      private

      # helper sugar for resources that support passing timeouts to shell_out
      #
      # module method to not pollute namespaces, but that means we need self injected as an arg
      # @api private
      def __maybe_add_timeout(obj, options)
        options = options.dup
        # historically resources have not properly declared defaults on their timeouts, so a default default of 900s was enforced here
        default_val = 900
        return options if options.key?(:timeout)

        # FIXME: need to nuke descendent tracker out of Chef::Provider so we can just define that class here without requiring the
        # world, and then just use symbol lookup
        if obj.class.ancestors.map(&:name).include?("Chef::Provider") && obj.respond_to?(:new_resource) && obj.new_resource.respond_to?(:timeout) && !options.key?(:timeout)
          options[:timeout] = obj.new_resource.timeout ? obj.new_resource.timeout.to_f : default_val
        end
        options
      end

      # helper function to mangle options when `default_env` is true
      #
      # @api private
      def __apply_default_env(options)
        options = options.dup
        default_env = options.delete(:default_env)
        default_env = true if default_env.nil?
        if default_env
          env_key = options.key?(:env) ? :env : :environment
          options[env_key] = {
            "LC_ALL" => __config[:internal_locale],
            "LANGUAGE" => __config[:internal_locale],
            "LANG" => __config[:internal_locale],
            __env_path_name => default_paths,
          }.update(options[env_key] || {})
        end
        options
      end

      # The shell_out_compacted/shell_out_compacted! APIs are private but are intended for use
      # in rspec tests.  They should always be used in rspec tests instead of shell_out to allow
      # for less brittle rspec tests.
      #
      # This expectation:
      #
      # allow(provider).to receive(:shell_out_compacted!).with("foo", "bar", "baz")
      #
      # Is met by many different possible calling conventions that mean the same thing:
      #
      # provider.shell_out!("foo", [ "bar", nil, "baz"])
      # provider.shell_out!(["foo", nil, "bar" ], ["baz"])
      #
      # Note that when setting `default_env: false` that you should just setup an expectation on
      # :shell_out_compacted for `default_env: false`, rather than the expanded env settings so
      # that the default_env implementation can change without breaking unit tests.
      #
      def shell_out_compacted(*args, **options)
        options = __apply_default_env(options)
        if options.empty?
          __shell_out_command(*args)
        else
          __shell_out_command(*args, **options)
        end
      end

      def shell_out_compacted!(*args, **options)
        options = __apply_default_env(options)
        cmd = if options.empty?
                __shell_out_command(*args)
              else
                __shell_out_command(*args, **options)
              end
        cmd.error!
        cmd
      end

      # Helper for subclasses to reject nil out of an array.  It allows using the array form of
      # shell_out (which avoids the need to surround arguments with quote marks to deal with shells).
      #
      # @param args [String] variable number of string arguments
      # @return [Array] array of strings with nil and null string rejection
      #
      def __clean_array(*args)
        args.flatten.compact.map(&:to_s)
      end

      def __shell_out_command(*args, **options)
        if __transport_connection
          FakeShellOut.new(args, options, __transport_connection.run_command(args.join(" "))) # FIXME: train should accept run_command(*args)
        else
          cmd = if options.empty?
                  Mixlib::ShellOut.new(*args)
                else
                  Mixlib::ShellOut.new(*args, **options)
                end
          cmd.live_stream ||= __io_for_live_stream
          cmd.run_command
          cmd
        end
      end

      def __io_for_live_stream
        if !STDOUT.closed? && __log.trace?
          STDOUT
        else
          nil
        end
      end

      def __env_path_name
        if ChefUtils.windows?
          "Path"
        else
          "PATH"
        end
      end

      class FakeShellOut
        attr_reader :stdout, :stderr, :exitstatus, :status

        def initialize(args, options, result)
          @args = args
          @options = options
          @stdout = result.stdout
          @stderr = result.stderr
          @exitstatus = result.exit_status
          @status = OpenStruct.new(success?: ( exitstatus == 0 ))
        end

        def error?
          exitstatus != 0
        end

        def error!
          raise Mixlib::ShellOut::ShellCommandFailed, "Unexpected exit status of #{exitstatus} running #{@args}" if error?
        end
      end
    end
  end
end
