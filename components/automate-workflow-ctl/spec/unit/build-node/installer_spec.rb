require 'spec_helper.rb'

require 'logger'
require 'build-node/installer'
require 'build-node/local_delivery'
require 'ctl-helpers/delivery_config'
require 'ctl-helpers/exceptions'

# Mock out the Config class since we're not testing it here.
# This variant will have defaults that are generally applicable,
# but can be overridden by calling config.$attribute = x
# in your test.
class MockConfig < BuildNode::Config
  attr_accessor :username, :password, :fqdn, :port, :installer, :ssh_identity_file,
    :job_dispatch_version

  def initialize(job_dispatch_version = nil)
    @username = 'user'
    @password = 'password'
    @fqdn = 'example.com'
    @port = 22
    @installer = "package.deb"
    @ssh_identity_file = "something.pem"
    @job_dispatch_version = job_dispatch_version || "v1"
  end

  def validate_and_prompt! ; end
end

describe BuildNode::Installer do
  let(:logger) { Logger.new("/dev/null") }

  let(:installer_output) { Proc.new { |output| } }
  let(:config) { MockConfig.new }
  let(:installer) { BuildNode::Installer.new([], installer_output) }
  let(:connection) { instance_double("connection") }
  let(:temp_path) { "tmp-build-node-installer" }
  let(:local_knife) { double(BuildNode::LocalKnife) }
  let(:local_delivery) { double(BuildNode::LocalDelivery) }

  before :each do
    # installer creates a Logger - make sure it's one we control
    allow(Logger).to receive(:new).and_return(logger)
    allow(installer).to receive(:logger).and_return(logger)

    # Use our mock config
    allow(BuildNode::Config).to receive(:new).and_return config

    # By default, make sure attempts to validate delivery config
    # don't end in a SystemExit which causes remaining tests to
    # silently not run:
    allow(CtlHelpers::DeliveryConfig).to receive(:validate!)

    # Don't create the /var/opt/logs/delivery/build-node-install dir
    allow(FileUtils).to receive(:mkdir_p)

    # Mock knife, delivery, train
    allow(BuildNode::LocalKnife).to receive(:new).and_return(local_knife)
    allow(BuildNode::LocalDelivery).to receive(:new).and_return(local_delivery)
    allow(BuildNode::RemoteConnection).to receive(:new).and_return(connection)


    # By default connections should work
    allow(installer).to receive(:connection).and_return(connection)
    allow(connection).to receive(:connect!)
    allow(connection).to receive(:os_name).and_return('ubuntu')

    allow(installer).to receive(:remote_temp_path).and_return(temp_path)

    installer.configure!
  end

  describe "#configure!" do
    it "sets up Installer instance configuration" do
      cli_options = double("options")
      installer_config = double("config", fqdn: "example.com")
      expect(BuildNode::Config).to receive(:parse_args!).
        with(Array, installer_output).and_return(cli_options)
      expect(BuildNode::Config).to receive(:new).
        with(cli_options).and_return(installer_config)
      expect(installer_config).to receive(:validate_and_prompt!)
      expect(CtlHelpers::DeliveryConfig).to receive(:validate!)
      expect(BuildNode::LocalKnife).to receive(:new)
      installer.configure!
      # These should be properly configured after a successful configure! run -
      # since we stub out many accessors by default, ensure the underling
      # attributes are set.
      expect(installer.log_path).to eq "/var/log/delivery-ctl/build-node-install_example.com.log"
      expect(installer.instance_variable_get(:@config)).not_to be nil
      expect(installer.instance_variable_get(:@local_knife)).not_to be nil
    end

    it "exits with SystemExit when delivery configuration file is incorrect" do
      allow(CtlHelpers::DeliveryConfig).to receive(:validate!).
        and_raise CtlHelpers::Exceptions::ConfigurationError.new("Sorry")
      expect{installer.configure!}.to raise_error(SystemExit)
    end
  end

  context "when Installer is properly configured" do
    context "#install" do
      # Generally speaking any failure should result in a clean exit via SystemExit
      # These tests ensure that this is done and appropriate additional information is provided
      # where appropriate for handled error types.
      context "exits with SystemExit error when" do
        let(:connection) { double("BuildNode::RemoteConnection") }

        before(:each) do
          allow(logger).to receive(:fatal)
        end

        it "fails to connect" do
          # Timeout/other errors are transport-specific
          expect(connection).to receive(:connect!).and_raise BuildNode::Exceptions::RemoteConnectionFailed.new("Oops")
          expect{installer.install}.to raise_error SystemExit
        end

        it "remote OS is unsupported" do
          expect(connection).to receive(:os_name).and_return('windows')
          expect{installer.install}.to raise_error SystemExit
        end

        it "installer extension doesn't match build node operating system" do
          config.installer = "package.rpm"
          allow(connection).to receive(:os_name).and_return('ubuntu')
          expect{installer.install}.to raise_error SystemExit
        end

        it "RuntimeError occurs, also captures and logs stack trace" do
          expect(connection).to receive(:connect!).and_raise { RuntimeError.new("oops.") }
          expect(logger).to receive(:fatal).with(/.*RuntimeError.*/)
          expect(logger).to receive(:fatal).with(/.*installer_spec\.rb.*/)
          expect{installer.install}.to raise_error SystemExit
        end

        it "Any file fails to copy" do
          allow(installer).to receive(:install_and_configure).
            and_raise BuildNode::Exceptions::RemoteCopyFailed
          expect{installer.install}.to raise_error SystemExit
        end

        it "Any remote command fails to execute" do
          allow(installer).to receive(:install_and_configure).
            and_raise BuildNode::Exceptions::RemoteExecutionFailed
          expect{installer.install}.to raise_error SystemExit
        end

        it "KnifeCommandFailed exception occurs during registration" do
          # Ignore messages leading up to the error we're looking for.
          allow(installer_output).to receive(:call)
          allow(installer).to receive(:install_and_configure).
            and_raise BuildNode::Exceptions::KnifeCommandFailed.new("oopsie")
          expect(installer_output).to receive(:call).
            with(/Failed to register #{config.fqdn} with Chef Server/)
          expect(logger).to receive(:fatal).
            with(/.*Error bootstrapping #{config.fqdn}.*oopsie.*/)
          expect{installer.install}.to raise_error SystemExit
        end

        it "BuildNode::Exceptions::LegacyNode" do
          allow(installer_output).to receive(:call)
          allow(installer).to receive(:install_and_configure).and_raise(
            BuildNode::Exceptions::LegacyNode.new("legacy node!!")
          )
          expect(installer_output).to receive(:call).
            with(/Failed to register #{config.fqdn} with Chef Server/)
          expect(logger).to receive(:fatal).
            with(/.*Error bootstrapping #{config.fqdn}.*legacy node!!.*/m)
          expect{ installer.install }.to raise_error(SystemExit)
        end

        it "BuildNode::Exceptions::NodeExists" do
          allow(installer_output).to receive(:call)
          allow(installer).to receive(:install_and_configure).and_raise(
            BuildNode::Exceptions::NodeExists.new("node exists!!")
          )
          expect(installer_output).to receive(:call).
            with(/Failed to register #{config.fqdn} with Chef Server/)
          expect(logger).to receive(:fatal).
            with(/.*Error bootstrapping #{config.fqdn}.*node exists!!.*/m)
          expect{ installer.install }.to raise_error(SystemExit)
        end

        it "BuildNode::Exceptions::ClientExists" do
          allow(installer_output).to receive(:call)
          allow(installer).to receive(:install_and_configure).and_raise(
            BuildNode::Exceptions::ClientExists.new("client exists!!")
          )
          expect(installer_output).to receive(:call).
            with(/Failed to register #{config.fqdn} with Chef Server/)
          expect(logger).to receive(:fatal).
            with(/.*Error bootstrapping #{config.fqdn}.*client exists!!.*/m)
          expect{ installer.install }.to raise_error(SystemExit)
        end
      end

      context "when all configuration is correct and the target platform is correct for the package" do
        it "performs the installation tasks" do
          expect(installer).to receive(:install_and_configure)
          installer.install
        end
      end

      context "when remote setup fails" do
        it "removes sensitive files" do
          expect(installer).to receive(:install_and_configure).and_raise(BuildNode::Exceptions::KnifeCommandFailed)
          expect(installer).to receive(:remove_sensitive_files)
          expect{ installer.install }.to raise_error(SystemExit)
        end
      end
    end

    context "#install_and_configure" do
      it "invokes all required functions to properly set up the build node" do
        expect(installer).to receive(:fetch_local_certificates).ordered
        expect(installer).to receive(:node_precheck).ordered
        expect(installer).to receive(:existing_client_d_move).ordered
        expect(installer).to receive(:create_temp_path).ordered
        expect(installer).to receive(:copy_chefdk).ordered
        expect(installer).to receive(:copy_base_files).ordered
        expect(installer).to receive(:copy_keys).ordered
        expect(installer).to receive(:install_chefdk!).ordered
        expect(installer).to receive(:fetch_certificates).ordered
        expect(installer).to receive(:register_node!).ordered
        expect(installer).to receive(:configure_node).ordered
        expect(installer).to receive(:configure_job_dispatch).ordered
        expect(installer).to receive(:cleanup).ordered

        installer.install_and_configure
      end
    end

    describe '#node_precheck' do
      it "should ask local knife to check if node is legacy build node" do
        expect(local_knife).to receive(:verify_node_not_legacy)
        installer.node_precheck
      end
    end

    describe "#fetch_local_certificates" do
      it "should ask local_knife to fetch local certificates" do
        expect(local_knife).to receive(:fetch_ssl_certificates)
        installer.fetch_local_certificates
      end
    end

    describe "#create_temp_path" do
      it "creates a temporary directory on the remote machine and sets ownership" do
        expect(installer).to receive(:run_command).with(anything(), "mkdir #{temp_path}")
        expect(installer).to receive(:run_command).with(anything(), "chown #{config.username} #{temp_path}")
        installer.create_temp_path
      end
    end

    describe "#copy_chefdk" do
      it "copies the installer to the correct path on the remote host" do
        expect(installer).to receive(:extension).and_return(".deb")
        expect(installer).to receive(:copy_file).
          with(anything(), config.installer, "#{temp_path}/chef_dk.deb")
        installer.copy_chefdk
      end
    end

    describe "#copy_base_files" do
      it "copies the install file directory to the correct path on the remote host" do
        expect(installer).to receive(:copy_file).
          with(anything(), "/opt/delivery/embedded/service/omnibus-ctl/installer", "#{temp_path}")
        installer.copy_base_files
      end
    end

    describe "#fetch_certificates" do
      let(:chef_server_fqdn) { "chef-server.example.com" }
      let(:chef_server_url) { "https://#{chef_server_fqdn}/organizations/delivery-org" }
      let(:chef_server_proxy) { false }
      let(:delivery_server_fqdn) { "delivery-server.example.com" }

      let(:delivery_config) { double("CtlHelpers::DeliveryConfig.delivery") }

      before(:each) do
        allow(CtlHelpers::DeliveryConfig).to receive(:delivery_fqdn).and_return(delivery_server_fqdn)
        allow(CtlHelpers::DeliveryConfig).to receive(:delivery).and_return(delivery_config)
        allow(delivery_config).to receive(:[]).with(:chef_server).and_return(chef_server_url)
        allow(delivery_config).to receive(:[]).with(:chef_server_proxy).and_return(chef_server_proxy)
      end

      context "when Delivery has multiple fqdns" do
        before(:each) do
          allow(delivery_config).to receive(:[]).with(:supermarket_fqdn).and_return(CtlHelpers::DeliveryConfig::NestedHash.new)
          allow(CtlHelpers::DeliveryConfig).to receive(:delivery_fqdn).and_return([delivery_server_fqdn])
        end

        it "fetches SSL certificates for the Chef Server and Delivery Server" do
          expect(installer).to receive(:run_command).with(anything(), "mkdir -p /etc/chef/trusted_certs")
          expect(installer).to receive(:fetch_certificate).with(chef_server_fqdn)
          expect(installer).to receive(:fetch_certificate).with(delivery_server_fqdn)
          installer.fetch_certificates
        end
      end

      context "when Supermarket is not set up" do
        before(:each) do
          allow(delivery_config).to receive(:[]).with(:supermarket_fqdn).and_return(CtlHelpers::DeliveryConfig::NestedHash.new)
        end

        it "fetches SSL certificates for the Chef Server and Delivery Server" do
          expect(installer).to receive(:run_command).with(anything(), "mkdir -p /etc/chef/trusted_certs")
          expect(installer).to receive(:fetch_certificate).with(chef_server_fqdn)
          expect(installer).to receive(:fetch_certificate).with(delivery_server_fqdn)
          expect(installer).to_not receive(:fetch_certificate).with(nil)
          installer.fetch_certificates
        end

        context "when Chef server is proxied" do
          let(:chef_server_url) { "https://localhost:8443/organizations/delivery-org" }
          let(:chef_server_proxy) { true }

          it "fetches SSL certificates for only the Delivery Server" do
            expect(installer).to receive(:run_command).with("Creating /etc/chef/trusted_certs",
                                                            "mkdir -p /etc/chef/trusted_certs")
            expect(installer).to receive(:fetch_certificate).with(delivery_server_fqdn)
            installer.fetch_certificates
          end
        end
      end

      context "when Supermarket is set up" do
        let(:supermarket_fqdn) { "supermarket.example.com" }

        before(:each) do
          allow(delivery_config).to receive(:[]).with(:supermarket_fqdn).and_return(supermarket_fqdn)
        end

        it "fetches SSL certificates for the Chef Server, Delivery Server, and Supermarket" do
          expect(installer).to receive(:run_command).with(anything(), "mkdir -p /etc/chef/trusted_certs")
          expect(installer).to receive(:fetch_certificate).with(chef_server_fqdn)
          expect(installer).to receive(:fetch_certificate).with(delivery_server_fqdn)
          expect(installer).to receive(:fetch_certificate).with(supermarket_fqdn)
          installer.fetch_certificates
        end
      end
    end

    describe "#copy_keys" do
      it "copies keys to the correct path on the remote host" do
        expect(installer).to receive(:copy_file).with(anything(), "/etc/delivery/builder_key",
                                                      "#{temp_path}/installer/builder_key")

        expect(installer).to receive(:copy_file).with(
          anything(),
          "/etc/delivery/delivery.pem",
          "#{temp_path}/installer/delivery.pem")
        installer.copy_keys
      end
    end

    describe "#install_chefdk!" do
      it "runs the remote installation command for chefdk" do
        expect(connection).to receive(:os_name).and_return('ubuntu')
        expect(installer).to receive(:run_command).with(anything(), "dpkg -i #{temp_path}/chef_dk.deb")
        installer.install_chefdk!
      end

      it "runs the remote installation command for chefdk on debian" do
        expect(connection).to receive(:os_name).and_return('debian')
        expect(installer).to receive(:run_command).with(anything(), "dpkg -i #{temp_path}/chef_dk.deb")
        installer.install_chefdk!
      end

      it "runs the remote installation command for chefdk on rhel" do
        expect(connection).to receive(:os_name).and_return('redhat')
        expect(installer).to receive(:run_command).with(anything(), "rpm -Uvh --replacepkgs #{temp_path}/chef_dk.rpm")
        installer.install_chefdk!
      end

      it "runs the remote installation command for chefdk on centos" do
        expect(connection).to receive(:os_name).and_return('centos')
        expect(installer).to receive(:run_command).with(anything(), "rpm -Uvh --replacepkgs #{temp_path}/chef_dk.rpm")
        installer.install_chefdk!
      end
    end

    describe "#register_node!" do
      context "when neither node nor client has been previously registered" do
        it "registers the build node with the chef server" do
          expect(installer_output).to receive(:call).
            with(/Registering #{config.fqdn} with the Chef Server/)
          expect(local_knife).to receive(:bootstrap)
          installer.register_node!
        end
      end

      context "when the node has been previously registered" do
        let(:exception) { BuildNode::Exceptions::AlreadyRegistered.new("a", "b")}
        before do
          expect(local_knife).to receive(:bootstrap).with(tag: nil).and_raise(exception)
        end

        it "raises the original exception if user does not choose over-writing" do
          expect(config).to receive(:overwrite_registration?).and_return(false)
          expect{installer.register_node!}.to raise_error(exception)
        end

        it "registers with the overwrite option if the user does choose over-writing" do
          expect(config).to receive(:overwrite_registration?).and_return(true)
          expect(local_knife).to receive(:bootstrap).with(overwrite: true, tag: nil)
          installer.register_node!
        end
      end

      context "job dispatch v2" do
        let(:config) { MockConfig.new("v2") }
        it "tags the build node with the chef server as job runner" do
          expect(installer_output).to receive(:call).
            with(/Registering #{config.fqdn} with the Chef Server/)
          expect(local_knife).to receive(:bootstrap).with(tag: 'delivery-job-runner')
          installer.register_node!
        end
      end
    end

    describe "#configure_node" do
      it "runs the remote configuration command from the correct location" do
        expect(installer).to receive(:run_command).
          with(anything(), "sh -c 'cd #{temp_path}/installer && ./install-build-node.sh'")
        installer.configure_node
      end
    end

    describe "#configure_job_dispatch" do
      context "job dispatch v1" do
        it "runs configure_push_client" do
          expect(installer).to receive(:configure_push_client)
          installer.configure_job_dispatch
        end
      end

      context "job dispatch v2" do
        let(:config) { MockConfig.new("v2") }
        it "runs configure_job_runner" do
          expect(installer).to receive(:configure_job_runner)
          installer.configure_job_dispatch
        end
      end
    end

    describe "#configure_push_client" do
      it "runs the push client configuration from the correct location" do
        expect(installer).to receive(:run_command).
          with(anything(), "sh -c 'cd #{temp_path}/installer && sh -c ./gen_push_config.sh'")
        installer.configure_push_client
      end
    end

    describe "#configure_job_runner" do
      let(:mock_runner) { { 'openssh_public_key' => 'openssh_public_key_string' } }
      let(:ohai_hash) do
        {
          "platform" => "centos",
          "platform_family" => "rhel",
          "platform_version" =>"7.2.1511",
          "os" => "linux"
        }
      end

      before do
        allow(config).to receive(:fqdn).and_return("dazzle-builder-1.local.dev")
      end

      it "registers the runner with the local delivery instance" do
        expect(local_knife).to receive(:gather_node_os_info_hash).and_return(ohai_hash)
        ohai_hash['hostname'] = "dazzle-builder-1.local.dev"
        expect(local_delivery).to receive(:create_runner).with(ohai_hash).and_return(mock_runner)
        expect(installer).to receive(:run_command).with(anything(), "chef-apply #{temp_path}/installer/install-job-runner.rb 'openssh_public_key_string'")

        installer.configure_job_runner
      end
    end

    describe "#cleanup" do
      it "runs removes the installation directory upon completion" do
        expect(installer).to receive(:run_command).with(anything(), "rm -rf #{temp_path}/")
        installer.cleanup
      end
    end

    describe "#output_error" do
      it "displays the associated message as-is to stdout" do
        expect(installer_output).to receive(:call).with("oopsie")
        installer.output_error(BuildNode::Exceptions::BaseError.new("oopsie"))
      end

      it "does not display remediation steps if they are not available" do
        expect(installer_output).to receive(:call).with("").exactly(0).times
        installer.output_error(BuildNode::Exceptions::BaseError.new("oopsie"))
      end

      it "displays the associated message and remediation steps when they are available" do
        expect(installer_output).to receive(:call).with("oopsie")
        expect(installer_output).to receive(:call).with("")
        expect(installer_output).to receive(:call).with("Fix it.")
        installer.output_error(BuildNode::Exceptions::BaseError.new("oopsie", "Fix it."))
      end

      it "writes the exception class and message to the log as a fatal error" do
        expect(logger).to receive(:fatal).with(/.*BaseError.*oopsie.*/)
        installer.output_error(BuildNode::Exceptions::BaseError.new("oopsie", "Fix it."))
      end
    end

    describe "#fetch_certificate" do
      let(:fqdn) { "server.example.com" }
      let(:cert_filename) { "server_example_com" }

      it "fetches the server SSL certificate" do
        expect(installer).to receive(:run_command).with(
          anything(),
          "sh -c 'openssl s_client -showcerts -connect #{fqdn}:443" \
          " </dev/null 2> /dev/null | openssl x509 -outform PEM >" \
          " /etc/chef/trusted_certs/#{cert_filename}.crt'"
        )
        installer.fetch_certificate(fqdn)
      end
    end

    describe "#copy_file" do
      let(:source) { "local/source" }
      let(:destination) { "remote/destination" }

      it "records the copied file destination on success" do
        expect(installer_output).to receive(:call).with(/Copying/)
        expect(connection).to receive(:copy_file).with(source, destination)
        installer.copy_file("test file", source, destination)
        expect(installer.copied_files).to include(destination)
      end

      it "does not record the copied file destination on failure" do
        expect(installer_output).to receive(:call).with(/Copying/)
        expect(connection).to receive(:copy_file).
            and_raise(BuildNode::Exceptions::RemoteCopyFailed)
        expect{ installer.copy_file("test file", source, destination) }.
            to raise_error(BuildNode::Exceptions::RemoteCopyFailed)
        expect(installer.copied_files).to_not include(destination)
      end
    end

    describe "#sensitive_files" do
      it "includes builder_key" do
        expect(installer.sensitive_files).to include("builder_key")
      end

      it "includes delivery_pem" do
        expect(installer.sensitive_files).to include("delivery.pem")
      end
    end

    describe "#remove_sensitive_files" do
      let(:file_list) { [] }
      let(:sensitive_files) { %w{sensitive_1 sensitive_2 sensitive_3} }

      before(:each) do
        expect(installer).to receive(:copied_files).and_return file_list
        expect(installer).to receive(:sensitive_files).at_least(1).times.
          and_return sensitive_files
      end

      it "does not attempt to remove non-sensitive files" do
        file_list << "#{temp_path}/installer/keepme"
        expect(installer).to_not receive(:run_command).
            with(String, "rm #{temp_path}/installer/keepme")
        installer.remove_sensitive_files
      end

      it "attempts to remotely delete only sensitive files" do
        %w{safe_1 sensitive_1 sensitive_2}.each { |actual| file_list << actual }
        expect(installer).to receive(:run_command).with(String, "rm sensitive_1")
        expect(installer).to receive(:run_command).with(String, "rm sensitive_2")
        installer.remove_sensitive_files
      end

      describe "displays manual removal instructions" do
        before(:each) do
          %w{safe_1 sensitive_1}.each { |actual| file_list << actual }
        end

        [BuildNode::Exceptions::RemoteExecutionFailed,
         BuildNode::Exceptions::RemoteConnectionFailed].each do |err|
          it "when the  error #{err} is received" do
            expect(installer).to receive(:run_command).and_raise(err)
            ["I was unable to remove the following sensitive file from the build node:",
             "",
             " * sensitive_1",
             "",
             "Please log into #{config.fqdn} and remove it."].each do |line|
               expect(installer_output).to receive(:call).with(line)
             end
             installer.remove_sensitive_files
          end
        end

        describe "and uses correct subject/object form when" do
          before(:each) do
            allow(installer).to receive(:run_command).with(String, /rm/).
              at_least(1).times.
              and_raise(BuildNode::Exceptions::RemoteExecutionFailed)
            allow(installer_output).to receive(:call)
          end

          it "a single file can't be removed" do
            file_list << "sensitive_2"
            expect(installer_output).to receive(:call).with(/sensitive files/)
            expect(installer_output).to receive(:call).with(/ \* /).exactly(2).times
            expect(installer_output).to receive(:call).with(/remove them/)
            installer.remove_sensitive_files
          end

          it "multiple files cannot be removed" do
            expect(installer_output).to receive(:call).with(/sensitive file/)
            expect(installer_output).to receive(:call).with(/ \*/).once
            expect(installer_output).to receive(:call).with(/remove it/)
            installer.remove_sensitive_files
          end
        end
      end
    end
  end
end
