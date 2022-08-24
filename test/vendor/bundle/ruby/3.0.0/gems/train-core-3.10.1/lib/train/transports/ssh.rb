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
require_relative "../errors"

module Train::Transports
  # Wrapped exception for any internally raised SSH-related errors.
  #
  # @author Fletcher Nichol <fnichol@nichol.ca>
  class SSHFailed < Train::TransportError; end
  class SSHPTYFailed < Train::TransportError; end

  # A Transport which uses the SSH protocol to execute commands and transfer
  # files.
  #
  # @author Fletcher Nichol <fnichol@nichol.ca>
  class SSH < Train.plugin(1) # rubocop:disable Metrics/ClassLength
    name "ssh"

    require_relative "ssh_connection"
    require_relative "cisco_ios_connection"

    # add options for submodules
    include_options Train::Extras::CommandWrapper

    # common target configuration
    option :host, required: true
    option :ssh_config_file, default: true
    option :port, default: 22, coerce: proc { |v| read_options_from_ssh_config(v, :port) }, required: true
    option :user, default: "root", coerce: proc { |v| read_options_from_ssh_config(v, :user) }, required: true
    option :key_files, default: nil
    option :password,  default: nil
    # additional ssh options
    option :keepalive, default: true
    option :keepalive_interval, default: 60
    option :connection_timeout, default: 15
    option :connection_retries, default: 5
    option :connection_retry_sleep, default: 1
    option :max_wait_until_ready, default: 600
    option :compression, default: false
    option :pty, default: false
    option :proxy_command, default: nil
    option :bastion_host, default: nil
    option :bastion_user, default: "root"
    option :bastion_port, default: 22
    option :non_interactive, default: false
    option :verify_host_key, default: false
    option :forward_agent, default: false

    # Allow connecting with older algorithms
    option :append_all_supported_algorithms, default: true

    option :compression_level do |opts|
      # on nil or false: set compression level to 0
      opts[:compression] ? 6 : 0
    end

    # (see Base#connection)
    def connection(state = {}, &block)
      apply_ssh_config_file(options[:host])
      opts = merge_options(options, state || {})
      validate_options(opts)
      conn_opts = connection_options(opts)

      if defined?(@connection) && reusable_connection?(conn_opts)
        reuse_connection(&block)
      else
        create_new_connection(conn_opts, &block)
      end
    end

    # Returns the ssh config option like user, port from config files
    # Params options [Hash], option_type [String]
    # Return String
    def self.read_options_from_ssh_config(options, option_type)
      files = options[:ssh_config_file].nil? || options[:ssh_config_file] == true ? Net::SSH::Config.default_files : options[:ssh_config_file]
      config_options = Net::SSH::Config.for(options[:host], files)
      config_options[option_type]
    end

    def apply_ssh_config_file(host)
      files = options[:ssh_config_file] == true ? Net::SSH::Config.default_files : options[:ssh_config_file]
      host_cfg = ssh_config_file_for_host(host, files)
      host_cfg.each do |key, value|
        # setting the key_files option to the private keys set in ssh config file
        if key == :keys && options[:key_files].nil? && !host_cfg[:keys].nil? && options[:password].nil?
          options[:key_files] = host_cfg[key]
        elsif options[key].nil?
          # Precedence is given to the option set by the user manually.
          # And only assigning value to the option from the ssh config file when it is not set by the user
          # in the option. When the option has a default value for e.g. option "keepalive_interval" has the "60" as the default
          # value, then the default value will be used even though the value for "user" is present in the ssh
          # config file. That is because the precedence is to the options set manually, and currently we don't have
          # any way to differentiate between the value set by the user or is it the default. This has a future of improvement.
          options[key] = host_cfg[key]
        end
      end
    end

    private

    def ssh_config_file_for_host(host, files)
      Net::SSH::Config.for(host, files)
    end

    def reusable_connection?(conn_opts)
      return false unless @connection_options

      # Do requested options match their current settings
      @connection_options.all? { |k, v| conn_opts[k] == v }
    end

    def validate_options(options)
      super(options)

      key_files = Array(options[:key_files])
      options[:auth_methods] ||= ["none"]

      # by default auth_methods has a default values [none publickey password keyboard-interactive]
      # REF: https://github.com/net-ssh/net-ssh/blob/master/lib/net/ssh/authentication/session.rb#L48
      if key_files.empty?
        options[:auth_methods].delete("publickey")
      else
        options[:keys_only] = true if options[:password].nil?
        options[:key_files] = key_files
      end

      if options[:password].nil?
        options[:auth_methods].delete("password")
        options[:auth_methods].delete("keyboard-interactive")
      end

      if options[:auth_methods] == ["none"]
        if ssh_known_identities.empty?
          raise Train::ClientError.new(
            "Your SSH Agent has no keys added, and you have not specified a password or a key file",
            :no_ssh_password_or_key_available
          )
        else
          logger.debug("[SSH] Using Agent keys as no password or key file have been specified")
          options[:auth_methods].push("publickey")
        end
      end

      options[:auth_methods] = options[:auth_methods].uniq

      if options[:pty]
        logger.warn("[SSH] PTY requested: stderr will be merged into stdout")
      end

      if [options[:proxy_command], options[:bastion_host]].all? { |type| !type.nil? }
        raise Train::ClientError, "Only one of proxy_command or bastion_host needs to be specified"
      end

      super
      self
    end

    # Creates an SSH Authentication KeyManager instance and saves it for
    # potential future reuse.
    #
    # @return [Hash] hash of SSH Known Identities
    # @api private
    def ssh_known_identities
      # Force KeyManager to load the key(s)
      @manager ||= Net::SSH::Authentication::KeyManager.new(nil).each_identity {}
      @manager.known_identities
    end

    # Builds the hash of options needed by the Connection object on
    # construction.
    #
    # @param opts [Hash] merged configuration and mutable state data
    # @return [Hash] hash of connection options
    # @api private
    def connection_options(opts)
      connection_options = {
        logger: logger,
        user_known_hosts_file: "/dev/null",
        hostname: opts[:host],
        port: opts[:port],
        username: opts[:user],
        compression: opts[:compression],
        compression_level: opts[:compression_level],
        keepalive: opts[:keepalive],
        keepalive_interval: opts[:keepalive_interval],
        timeout: opts[:connection_timeout],
        connection_retries: opts[:connection_retries],
        connection_retry_sleep: opts[:connection_retry_sleep],
        max_wait_until_ready: opts[:max_wait_until_ready],
        auth_methods: opts[:auth_methods],
        keys_only: opts[:keys_only],
        keys: opts[:key_files],
        password: opts[:password],
        forward_agent: opts[:forward_agent],
        proxy_command: opts[:proxy_command],
        bastion_host: opts[:bastion_host],
        bastion_user: opts[:bastion_user],
        bastion_port: opts[:bastion_port],
        non_interactive: opts[:non_interactive],
        append_all_supported_algorithms: opts[:append_all_supported_algorithms],
        config: options[:ssh_config_file],
        transport_options: opts,
      }
      # disable host key verification. The hash key and value to use
      # depends on the version of net-ssh in use.
      connection_options[verify_host_key_option] = verify_host_key_value(opts[:verify_host_key])

      connection_options
    end

    #
    # Returns the correct host-key-verification option key to use depending
    # on what version of net-ssh is in use. In net-ssh <= 4.1, the supported
    # parameter is `paranoid` but in 4.2, it became `verify_host_key`
    #
    # `verify_host_key` does not work in <= 4.1, and `paranoid` throws
    # deprecation warnings in >= 4.2.
    #
    # While the "right thing" to do would be to pin train's dependency on
    # net-ssh to ~> 4.2, this will prevent InSpec from being used in
    # Chef v12 because of it pinning to a v3 of net-ssh.
    #
    def verify_host_key_option
      current_net_ssh = Net::SSH::Version::CURRENT
      new_option_version = Net::SSH::Version[4, 2, 0]

      current_net_ssh >= new_option_version ? :verify_host_key : :paranoid
    end

    # Likewise, version <5 accepted false; 5+ requires :never or will
    # issue a deprecation warning. This method allows a lot of common
    # things through.
    def verify_host_key_value(given)
      current_net_ssh = Net::SSH::Version::CURRENT
      new_value_version = Net::SSH::Version[5, 0, 0]
      if current_net_ssh >= new_value_version
        # 5.0+ style
        {
          # It's not a boolean anymore.
          "true" => :always,
          "false" => :never,
          true => :always,
          false => :never,
          # May be correct value, but strings from JSON config
          "always" => :always,
          "never" => :never,
          nil => :never,
        }.fetch(given, given)
      else
        # up to 4.2 style
        {
          "true" => true,
          "false" => false,
          nil => false,
        }.fetch(given, given)
      end
    end

    # Creates a new SSH Connection instance and save it for potential future
    # reuse.
    #
    # @param options [Hash] connection options
    # @return [Ssh::Connection] an SSH Connection instance
    # @api private
    def create_new_connection(options, &block)
      if defined?(@connection)
        logger.debug("[SSH] shutting previous connection #{@connection}")
        @connection.close
      end

      @connection_options = options
      conn = Connection.new(options, &block)

      # Cisco IOS requires a special implementation of `Net:SSH`. This uses the
      # SSH transport to identify the platform, but then replaces SSHConnection
      # with a CiscoIOSConnection in order to behave as expected for the user.
      if defined?(conn.platform.cisco_ios?) && conn.platform.cisco_ios?
        ios_options = {}
        ios_options[:host] = @options[:host]
        ios_options[:user] = @options[:user]
        # The enable password is used to elevate privileges on Cisco devices
        # We will also support the sudo password field for the same purpose
        # for the interim. # TODO
        ios_options[:enable_password] = @options[:enable_password] || @options[:sudo_password]
        ios_options[:logger] = @options[:logger]
        ios_options.merge!(@connection_options)
        conn = CiscoIOSConnection.new(ios_options)
      end

      @connection = conn unless conn.nil?
    end

    # Return the last saved SSH connection instance.
    #
    # @return [Ssh::Connection] an SSH Connection instance
    # @api private
    def reuse_connection
      logger.debug("[SSH] reusing existing connection #{@connection}")
      yield @connection if block_given?
      @connection
    end

  end
end
