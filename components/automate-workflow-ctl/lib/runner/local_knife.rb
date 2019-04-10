require 'json'
require 'uri'
require 'fileutils'

require 'mixlib/shellout'
require 'mixlib/config'

require 'ctl-helpers/delivery_config'
require 'runner/exceptions'

# Pre-baked knife commands for interacting with a runner from a Delivery Server.

module Runner
  # Wraps CLI calls to knife, using the embedded knife/chef that we ship
  # with Delivery server. Usage of this class assumes that DeliveryConfig has
  # been initialized and verified.
  class LocalKnife
    def initialize(fqdn)
      @fqdn = fqdn
    end

    def bootstrap(username, password, ssh_identity_file, sudo, client_config, port)
      bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      tag_node
    end

    def verify_node_not_legacy
      if delivery_cluster_node?
        err = Runner::Exceptions::LegacyNode.new(
          "Delivery runner client #{@fqdn} is a Delivery Cluster installation already registered with" \
          " the Chef Server at #{chef_server}.")
        err.remediation_steps = "#{@fqdn} cannot be upgraded. Please retry the installation on a fresh host."
        raise err
      end
    end

    def gather_node_os_info_hash
      JSON.parse(run_command("node show #{@fqdn} -a platform -a platform_family -a platform_version -a os -F json").stdout)[@fqdn]
    end

    def node_registration_status
      if client_exists?
        return :client_exists
      end

      if node_exists?
        return :node_exists
      end

      false
    end

    def bootstrap_node(username, password, ssh_identity_file, sudo, client_config, port)
      bootstrap_options = [ "--ssh-user", username,
                            "--node-name", @fqdn,
                            "--yes"
                          ]

      if ssh_identity_file
        bootstrap_options += ["--ssh-identity-file", ssh_identity_file]
      end

      if password
        bootstrap_options += ["--ssh-password", "'#{password}'"]
      end

      if sudo
        bootstrap_options += ["--sudo", "--use-sudo-password"]
      end

      bootstrap_options += ["--ssh-port", port]

      Dir.mktmpdir do |client_d_dir|
        if client_config
          File.write(File.join(client_d_dir, "runner_install.rb"), client_config)

          bootstrap_options += ["--config-option client_d_dir=#{client_d_dir}"]
        end
        run_command("bootstrap", @fqdn, *bootstrap_options)
      end
    end

    def tag_node
      run_command("tag create", [@fqdn, "delivery-job-runner"])
    end

    # Fetch the Chef Server SSL certificates so we can communicate.
    def fetch_ssl_certificates
      run_command("ssl fetch")
    rescue Runner::Exceptions::KnifeCommandFailed => e
      raise Runner::Exceptions::CertFetchFailed.new(e.message)
    end

    def delivery_cluster_node?
      search_node_options = [ '-a', 'fqdn' ]
      # Chef 13+ 'knife search' returns 1 when a search returns no results
      shellout_options = { returns: [ 0, 1 ] }
      result = run_command("search node '(name:#{@fqdn} OR fqdn:#{@fqdn} OR ipaddress:#{@fqdn}) AND role:delivery_builders'", *search_node_options, shellout_options)
      return true unless result.stdout.empty?
    end

    def node_exists?
      search_node_options = [ '-a', 'fqdn' ]
      # Chef 13+ 'knife search' returns 1 when a search returns no results
      shellout_options = { returns: [ 0, 1 ] }
      result = run_command("search node name:#{@fqdn}", *search_node_options, shellout_options)
      return true unless result.stdout.empty?
    end

    def client_exists?
      search_client_options = [ '-a', 'fqdn' ]
      # Chef 13+ 'knife search' returns 1 when a search returns no results
      shellout_options = { returns: [ 0, 1 ] }
      result = run_command("search client name:#{@fqdn}", *search_client_options, shellout_options)
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
      message = "#{redacted_cmd} returned #{so.exitstatus}"
      if so.stderr
        message += "\n***********\n"
        message += so.stderr
        message += "***********\n"
      end
      raise Runner::Exceptions::KnifeCommandFailed.new(message)
    end

    def create_command(cmd, *options)
      knife = ENV['KNIFE_PATH'] || "/opt/delivery/embedded/bin/knife"
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
