require 'optparse'

require 'chef-cli-unity/options'

module Runner
  module Install
    # Class solely responsible handling install-runner option definitions and parsing.
    class Options < ChefCliUnity::Options
      attr_reader :options, :option_parser

      def initialize(args, desc)
        super(args,
              "install-runner FQDN USERNAME [options...]",
              description: desc,
              arguments: {
                "FQDN" => "Fully qualified domain name of the remote host that will be configured into a runner",
                "USERNAME" => "The username used for authentication to the remote host that will be configured into a runner"
              })
      end

      def additional_options(opts)
        opts.on("-I", "--installer PATH_TO_INSTALLER",
                "The location of the ChefDK package local to this server to install on the runner. This option cannot be passed with --chefdk-version as that option specifies remote download. If neither are passed, the latest ChefDK will be downloaded remotely from https://packages.chef.io.") do |installer|
          options[:installer] = installer
        end

        opts.on("-v", "--chefdk-version VERSION",
                "ChefDK version to download and install on the runner. This option cannot be passed with --installer as that option specifies using a package local to this server. If neither are passed, the latest ChefDK will be downloaded remotely.") do |version|
          options[:chefdk_version] = version
        end

        opts.on("-P", "--password [PASSWORD]", "Pass if you need to set a password for ssh and / or sudo access. If --ssh-identify-file is also passed, will only be used for sudo access.") do |password|
          options[:password] = password
          options[:password_flag_set] = true
        end

        opts.on("-p", "--port PORT", "Port to connect to on the remote host.") do |port|
          options[:port] = port
          options[:port_flag_set] = true
        end

        opts.on("-i", "--ssh-identity-file IDENTITY_FILE",
                "The SSH identity file used for authentication.") do |identity|
          options[:ssh_identity_file] = identity
        end

        opts.on("-e", "--enterprise ENTERPRISE", "Enterprise to use. Legacy option, only required if you have more than 1 Enterprise configured") do |enterprise|
          options[:enterprise] = enterprise
        end

        opts.on("-y", "--yes", "Skip configuration confirmation and overwrite any existing Chef Server nodes of the same name as FQDN.") do |yes|
          options[:yes] = yes
        end

        opts.on("--full-ohai", "If `--full-ohai` flag set, Chef will run with full Ohai plugins.") do |full_ohai|
          options[:full_ohai] = full_ohai
        end

        opts.on("--fips-custom-cert-filename CERTIFICATE_FILE", "The path to a pem file that contains a self-signed certificate or certificate chain. " \
                                                                "Use this setting only when Automate server has a custom certificate authority or a self-signed certificate. " \
                                                                "Please see the Automate FIPS docs for more details.") do |filename|
          options[:fips_custom_cert_filename] = filename
        end
      end
    end
  end
end

