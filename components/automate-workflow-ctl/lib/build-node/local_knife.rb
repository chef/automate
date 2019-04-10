require 'json'

require 'mixlib/shellout'
require 'mixlib/config'

require 'ctl-helpers/delivery_config'
require 'build-node/exceptions'

# Pre-baked knife commands for interacting with a build node from a Delivery Server.

module BuildNode
  # Wraps CLI calls to knife, using the embedded knife/chef that we ship
  # with Delivery server. Usage of this class assumes that DeliveryConfig has
  # been initialized and verified.
  class LocalKnife
    def initialize(config)
      @config = config
    end

    # Registers a delivery build node with the Chef Server and tags
    # it as delivery-build-node
    # Fails with NodeExists or ClientExists if a same-named node or client
    # already exists on the chef-server.
    # Provide overwrite: true to `flags` if you want to
    # force node and client creation even if they already exist.
    def bootstrap(flags = {})
      verify_node_not_registered(flags)
      bootstrap_node(flags)
      tag_node(flags[:tag])
    end

    def verify_node_not_legacy
      if delivery_cluster_node?
        err = BuildNode::Exceptions::LegacyNode.new(
          "Delivery build client #{@config.fqdn} is a Delivery Cluster installation already registered with" \
          " the Chef Server at #{chef_server}.")
        err.remediation_steps = "#{@config.fqdn} cannot be upgraded. Please retry the installation on a fresh host."
        raise err
      end
    end

    def gather_node_os_info_hash
      JSON.parse(run_command("node show #{@config.fqdn} -a platform -a platform_family -a platform_version -a os -F json").stdout)[@config.fqdn]
    end

    def verify_node_not_registered(flags = {})
      return if flags[:overwrite]
      if client_exists?
        err = BuildNode::Exceptions::ClientExists.new(
          "Delivery build client #{@config.fqdn} is already registered with" \
          " the Chef Server at #{chef_server}.")
        err.remediation_steps = "To delete client #{@config.fqdn} run" \
          "\n  #{create_command("client delete #{@config.fqdn}")}"
        raise err
      end

      if node_exists?
        err = BuildNode::Exceptions::NodeExists.new(
          "Delivery build node #{@config.fqdn} is already registered with" \
          " the Chef Server at #{chef_server}.")
        err.remediation_steps = "To delete node #{@config.fqdn} run" \
          "\n  #{create_command("node delete #{@config.fqdn}")}"
        raise err
      end

    end

    def bootstrap_node(flags = {})
      bootstrap_options = [ "--ssh-user", @config.username,
                            "--node-name", @config.fqdn,
                            "--sudo",
                            "--use-sudo-password" ]

      if @config.ssh_identity_file
        bootstrap_options += ["--ssh-identity-file", @config.ssh_identity_file]
      end

      if @config.password
        bootstrap_options += ["--ssh-password", "'#{@config.password}'"]
      end

      if flags[:overwrite]
        bootstrap_options += ["--yes"]
      end

      Dir.mktmpdir do |client_d_dir|
        if @config.client_config
          File.write(File.join(client_d_dir, "runner_install.rb"), @config.client_config)

          bootstrap_options += ["--config-option client_d_dir=#{client_d_dir}"]
        end

        run_command("bootstrap", @config.fqdn, *bootstrap_options)
      end
    end

    def tag_node(tag = nil)
      tag ||= "delivery-build-node"
      run_command("tag create", [@config.fqdn, tag])
    end

    # Fetch the Chef Server SSL certificates so we can communicate.
    def fetch_ssl_certificates
      run_command("ssl fetch")
    rescue BuildNode::Exceptions::KnifeCommandFailed => e
      raise BuildNode::Exceptions::CertFetchFailed.new(e.message)
    end

    def delivery_cluster_node?
      search_node_options = [ '-a', 'fqdn' ]
      # Chef 13+ 'knife search' returns 1 when a search returns no results
      shellout_options = { returns: [ 0, 1 ] }
      result = run_command("search node '(name:#{@config.fqdn} OR fqdn:#{@config.fqdn} OR ipaddress:#{@config.fqdn}) AND role:delivery_builders'", *search_node_options, shellout_options)
      return true unless result.stdout.empty?
    end

    def node_exists?
      search_node_options = [ '-a', 'fqdn' ]
      # Chef 13+ 'knife search' returns 1 when a search returns no results
      shellout_options = { returns: [ 0, 1 ] }
      result = run_command("search node name:#{@config.fqdn}", *search_node_options, shellout_options)
      return true unless result.stdout.empty?
    end

    def client_exists?
      search_client_options = [ '-a', 'fqdn' ]
      # Chef 13+ 'knife search' returns 1 when a search returns no results
      shellout_options = { returns: [ 0, 1 ] }
      result = run_command("search client name:#{@config.fqdn}", *search_client_options, shellout_options)
      return true unless result.stdout.empty?
    end

    # Runs knife commands from the knife packaged with Delivery. Always runs
    # with required default options. Additional options can be supplied as an
    # Array or String.
    #
    # Default options:
    #   -u delivery['chef_username']
    #   -k delivery['chef_private_key']
    #   --server-url delivery['chef_server']
    def run_command(cmd, *options, **shellout_options)
      so = Mixlib::ShellOut.new(create_command(cmd, *options), shellout_options)
      so.run_command
      so.error!
      so
    rescue Mixlib::ShellOut::ShellCommandFailed => e
      # Knife commands can include the password, so show a redacted version
      # of the command line along with the exit code, instead of the mixlib output
      pwd_index = options.index("--ssh-password")
      options[pwd_index+1] = "(hidden)" if pwd_index && options.length > pwd_index+1
      redacted_cmd = create_command(cmd, options)
      raise BuildNode::Exceptions::KnifeCommandFailed.new("#{redacted_cmd} returned #{so.exitstatus}")
    end

    def create_command(cmd, *options)
      knife = "/opt/delivery/embedded/bin/knife"
      base_options = [ "-u", chef_username,
                       "-k", chef_private_key,
                       "--server-url", chef_server ]
      ([ knife, cmd ] + options + base_options).join(" ")
    end

    private

    def chef_username
      CtlHelpers::DeliveryConfig.delivery[:chef_username]
    end

    def chef_private_key
      CtlHelpers::DeliveryConfig.delivery[:chef_private_key]
    end

    def chef_server
      # Note: this `== true` looks odd, but is correct: if it has not been set,
      #       we'd still get NestedHash.new here, which is truthy
      if CtlHelpers::DeliveryConfig.delivery[:chef_server_proxy] == true
        proxied_chef_server
      else
        CtlHelpers::DeliveryConfig.delivery[:chef_server]
      end
    end

    def proxied_chef_server
      chef_server_uri = URI(CtlHelpers::DeliveryConfig.delivery[:chef_server])
      chef_server_uri.host = delivery_fqdn
      chef_server_uri.port = CtlHelpers::DeliveryConfig.nginx[:ssl_port].is_a?(Fixnum) ? CtlHelpers::DeliveryConfig.nginx[:ssl_port] : 443
      chef_server_uri.to_s
    end

    def delivery_fqdn
      [CtlHelpers::DeliveryConfig.delivery_fqdn].flatten.first
    end
  end
end
