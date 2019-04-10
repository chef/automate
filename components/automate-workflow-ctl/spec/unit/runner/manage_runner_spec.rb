require 'spec_helper.rb'

require 'logger'
require 'runner/manage_runner'
require 'runner/manage_runner_ctl'
require 'ctl-helpers/delivery_config'
require 'ctl-helpers/exceptions'
require 'runner/chefdk_package'

describe Runner::ManageRunner do
  let(:logger) { Logger.new("/dev/null") }
  let(:description) { "desc" }

  let(:temp_path) { "tmp-runner-installer" }
  let(:local_knife) { double(Runner::LocalKnife) }
  let(:client) { double(Runner::ManageRunnerCtl) }

  # config values
  let(:install_config) { double(Runner::Install::Config) }
  let(:delete_runner_config) { double(Runner::Delete::Config) }
  let(:username) { "user" }
  let(:password) { "password" }
  let(:fqdn) { "example.com" }
  let(:port) { 22 }
  let(:installer_string) { "package.deb" }
  let(:ssh_identity_file) { "something.pem" }
  let(:job_dispatch_version) { "v2" }
  let(:chefdk_version) { nil }
  let(:client_config) { "minimal_ohai true\n" }

  # remote_connection
  let(:os_name) { 'ubuntu' }
  let(:os_release) { '12.04' }
  let(:connection) { instance_double("connection") }

  describe "delete runner process" do
    let(:installer) { Runner::ManageRunner.new([], description) }
    let(:hash) { {} }

    before :each do
      # Use our mock install_config
      allow(Runner::Delete::Config).to receive(:new).and_return delete_runner_config
      allow(Runner::ManageRunnerCtl).to receive(:new).and_return(client)
      # mocking out the config
      allow(delete_runner_config).to receive(:fqdn).and_return(fqdn)
    end

    describe "delete" do
      it "deletes runner" do
        installer_config = double("delete_runner_config", fqdn: "example.com")
        expect(Runner::Delete::Config).to receive(:new).
          with(installer.args, installer.description).and_return(delete_runner_config)
        expect(delete_runner_config).to receive(:validate!)
        expect(client).to receive(:validate_runner).with(hash)
        expect(delete_runner_config).to receive(:confirm_config)
        hash['hostname'] = delete_runner_config.fqdn
        expect(client).to receive(:delete_runner).with(hash)
        installer.delete
        expect(installer.instance_variable_get(:@config)).not_to be nil
      end
    end
  end # describe "delete runner process"

  describe "install runner process" do
    let(:installer) { Runner::ManageRunner.new([], description) }

    before :each do
      # installer creates a Logger - make sure it's one we control
      allow(Logger).to receive(:new).and_return(logger)
      allow(installer).to receive(:logger).and_return(logger)

      # Use our mock config
      allow(Runner::Install::Config).to receive(:new).and_return install_config

      # By default, make sure attempts to validate delivery config
      # don't end in a SystemExit which causes remaining tests to
      # silently not run:
      allow(CtlHelpers::DeliveryConfig).to receive(:validate!)

      # Don't create the /var/opt/logs/delivery/build-node-install dir
      allow(FileUtils).to receive(:mkdir_p)

      # Mock knife, delivery, train
      allow(Runner::LocalKnife).to receive(:new).and_return(local_knife)
      allow(Runner::ManageRunnerCtl).to receive(:new).and_return(client)
      allow(Runner::RemoteConnection).to receive(:new).and_return(connection)
      allow(connection).to receive(:need_sudo?).and_return(false)

      # By default connections should work
      allow(installer).to receive(:connection).and_return(connection)
      allow(connection).to receive(:connect!)
      allow(connection).to receive(:os_name).and_return(os_name)
      allow(connection).to receive(:os_release).and_return(os_release)

      allow(installer).to receive(:remote_temp_path).and_return(temp_path)
      allow(installer).to receive(:puts)
      allow(installer).to receive(:print)

      allow(local_knife).to receive(:node_registration_status).and_return(false)
      allow(local_knife).to receive(:verify_node_not_legacy)
      allow(local_knife).to receive(:fetch_ssl_certificates)

      # mocking out the config
      allow(install_config).to receive(:username).and_return(username)
      allow(install_config).to receive(:password).and_return(password)
      allow(install_config).to receive(:fqdn).and_return(fqdn)
      allow(install_config).to receive(:port).and_return(port)
      allow(install_config).to receive(:installer).and_return(installer_string)
      allow(install_config).to receive(:ssh_identity_file).and_return(ssh_identity_file)
      allow(install_config).to receive(:job_dispatch_version).and_return(job_dispatch_version)
      allow(install_config).to receive(:chefdk_version).and_return(chefdk_version)
      allow(install_config).to receive(:validate!)
      allow(install_config).to receive(:confirm_config)
      allow(install_config).to receive(:display_usage)
      allow(install_config).to receive(:sudo).and_return(true)
      allow(install_config).to receive(:fips_custom_cert_filename).and_return(nil)
      allow(install_config).to receive(:client_config).and_return(client_config)

      installer.configure!
    end

    describe "#configure!" do
      it "sets up Installer instance configuration" do
        cli_options = double("options")
        installer_config = double("config", fqdn: "example.com")
        expect(Runner::Install::Config).to receive(:new).
          with(installer.args, installer.description).and_return(installer_config)
        expect(installer_config).to receive(:validate!)
        expect(installer_config).to receive(:confirm_config)
        expect(CtlHelpers::DeliveryConfig).to receive(:validate!)
        expect(Runner::LocalKnife).to receive(:new)
        installer.configure!
        # These should be properly configured after a successful configure! run -
        # since we stub out many accessors by default, ensure the underling
        # attributes are set.
        expect(installer.log_path).to eq "/var/log/delivery-ctl/runner-install_example.com.log"
        expect(installer.instance_variable_get(:@config)).not_to be nil
        expect(installer.instance_variable_get(:@local_knife)).not_to be nil
      end

      it "exits with SystemExit when delivery configuration file is incorrect" do
        allow(CtlHelpers::DeliveryConfig).to receive(:validate!).
          and_raise CtlHelpers::Exceptions::ConfigurationError.new("Sorry")
        expect{installer.configure!}.to raise_error(SystemExit)
      end

      let (:node_registration_status) { false }
      it "adds sudo boolean response to the config" do
        expect(connection).to receive(:need_sudo?).and_return(true)
        expect(install_config).to receive(:needs_sudo!)
        installer.configure!
      end
    end

    context "when Installer is properly configured" do
      context "#install" do
        # Generally speaking any failure should result in a clean exit via SystemExit
        # These tests ensure that this is done and appropriate additional information is provided
        # where appropriate for handled error types.
        context "exits with SystemExit error when" do
          let(:connection) { double("Runner::RemoteConnection") }

          before(:each) do
            allow(logger).to receive(:fatal)
          end

          it "fails to connect" do
            # Timeout/other errors are transport-specific
            expect(connection).to receive(:connect!).and_raise Runner::Exceptions::RemoteConnectionFailed.new("Oops")
            expect{installer.install}.to raise_error SystemExit
          end

          context "when remote OS is unsupported" do
            let(:os_name) { "windows" }

            it "remote OS is unsupported" do
              expect{installer.install}.to raise_error SystemExit
            end
          end

          context "when installer extension doesn't match runner operating system" do
            let(:installer_string) { "package.rpm" }
            it "raises SystemExit" do
              allow(connection).to receive(:os_name).and_return('ubuntu')
              expect{installer.install}.to raise_error SystemExit
            end
          end

          it "RuntimeError occurs, also captures and logs stack trace" do
            expect(connection).to receive(:connect!).and_raise { RuntimeError.new("oops.") }
            expect(logger).to receive(:fatal).with(/.*RuntimeError.*/)
            expect(logger).to receive(:fatal).with(/.*manage_runner_spec\.rb.*/)
            expect{installer.install}.to raise_error SystemExit
          end

          it "Any file fails to copy" do
            allow(installer).to receive(:install_and_configure).
              and_raise Runner::Exceptions::RemoteCopyFailed
            expect{installer.install}.to raise_error SystemExit
          end

          it "Any remote command fails to execute" do
            allow(installer).to receive(:install_and_configure).
              and_raise Runner::Exceptions::RemoteExecutionFailed
            expect{installer.install}.to raise_error SystemExit
          end

          it "KnifeCommandFailed exception occurs during registration" do
            # Ignore messages leading up to the error we're looking for.
            allow(installer).to receive(:install_and_configure).
              and_raise Runner::Exceptions::KnifeCommandFailed.new("oopsie")
            expect(installer).to receive(:puts).
              with(/Failed to register #{install_config.fqdn} with Chef Server/)
            expect(logger).to receive(:fatal).
              with(/.*Error bootstrapping #{install_config.fqdn}.*oopsie.*/)
            expect{installer.install}.to raise_error SystemExit
          end

          it "Runner::Exceptions::LegacyNode" do
            allow(installer).to receive(:install_and_configure).and_raise(
              Runner::Exceptions::LegacyNode.new("legacy node!!")
            )
            expect(installer).to receive(:puts).
              with(/Failed to register #{install_config.fqdn} with Chef Server/)
            expect(logger).to receive(:fatal).
              with(/.*Error bootstrapping #{install_config.fqdn}.*legacy node!!.*/m)
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
            expect(installer).to receive(:install_and_configure).and_raise(Runner::Exceptions::KnifeCommandFailed)
            expect(installer).to receive(:remove_sensitive_files)
            expect{ installer.install }.to raise_error(SystemExit)
          end
        end
      end

      context "#install_and_configure" do
        let(:path) { "this/is/a/path" }

        it "invokes all required functions to properly set up the runner" do
          expect(installer).to receive(:existing_key_backup).ordered
          expect(installer).to receive(:existing_client_d_move).ordered
          expect(installer).to receive(:create_temp_path).ordered
          expect(installer).to receive(:copy_chefdk).with(path).ordered
          expect(installer).to receive(:copy_base_files).ordered
          expect(installer).to receive(:copy_keys).ordered
          expect(installer).to receive(:copy_custom_certificate_chain).ordered
          expect(installer).to receive(:install_chefdk!).ordered
          expect(installer).to receive(:fetch_certificates).ordered
          expect(installer).to receive(:register_node!).ordered
          expect(installer).to receive(:configure_node).ordered
          expect(installer).to receive(:configure_job_runner).ordered
          expect(installer).to receive(:restore_backup_key).ordered
          expect(installer).to receive(:cleanup).ordered

          installer.install_and_configure(path)
        end
      end

      describe "#ensure_chefdk_package" do
        context "when --installer was passed" do
          it "adds the provided installer path to local_chefdk_path for later use" do
            expect(installer).to receive(:validate_package!).with(install_config.installer)
            expect(installer.ensure_chefdk_package).to eq(install_config.installer)
          end
        end

        context "when --installer was not passed" do
          let(:installer_string) { nil }
          let(:chefdk_package) { instance_double("chefdk_package") }
          let(:filepath) { "/some/file/path/to/chefdk.deb" }

          shared_examples "downloads the ChefDK and puts it in tmp" do
            it "returns the path to the downloaded chefdk" do
              expect(Runner::ChefDKPackage).to receive(:new)
                                                .with(chefdk_version,
                                                      os_name,
                                                      os_release,
                                                     ).and_return(chefdk_package)

              expect(chefdk_package).to receive(:download).and_return(filepath)

              expect(installer.ensure_chefdk_package).to eq(filepath)
            end
          end

          context "when --chefdk-version was specified" do
            let(:chefdk_version) { "0.16.12" }

            it_should_behave_like "downloads the ChefDK and puts it in tmp"
          end

          context "when --chefdk-version was not specified" do
            let(:chefdk_version) { nil }

            it_should_behave_like "downloads the ChefDK and puts it in tmp"
          end
        end
      end

      describe '#preflight_check' do
        it "should ask local knife to check if node is legacy build node" do
          expect(local_knife).to receive(:verify_node_not_legacy)
          installer.preflight_check
        end

        it "should ask local_knife to fetch local certificates" do
          expect(local_knife).to receive(:fetch_ssl_certificates)
          installer.preflight_check
        end
      end

      describe "#create_temp_path" do
        it "creates a temporary directory on the remote machine and sets ownership" do
          expect(installer).to receive(:run_command).with("mkdir #{temp_path}")
          expect(installer).to receive(:run_command).with("chown #{install_config.username} #{temp_path}")
          installer.create_temp_path
        end
      end

      describe "#copy_chefdk" do
        let(:path) { "this/is/a/path/chef_dk.deb" }

        it "copies the installer to the correct path on the remote host" do
          expect(installer).to receive(:copy_file).
            with(path, "#{temp_path}/chef_dk.deb")
          installer.copy_chefdk(path)
        end
      end

      describe "#copy_base_files" do
        it "copies the install file directory to the correct path on the remote host" do
          expect(installer).to receive(:copy_file).
            with("/opt/delivery/embedded/service/omnibus-ctl/installer", "#{temp_path}")
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
            expect(installer).to receive(:run_command).with("mkdir -p /etc/chef/trusted_certs")
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
            expect(installer).to receive(:run_command).with("mkdir -p /etc/chef/trusted_certs")
            expect(installer).to receive(:fetch_certificate).with(chef_server_fqdn)
            expect(installer).to receive(:fetch_certificate).with(delivery_server_fqdn)
            expect(installer).to_not receive(:fetch_certificate).with(nil)
            installer.fetch_certificates
          end

          context "when Chef server is proxied" do
            let(:chef_server_url) { "https://localhost:8443/organizations/delivery-org" }
            let(:chef_server_proxy) { true }

            it "fetches SSL certificates for only the Delivery Server" do
              expect(installer).to receive(:run_command).with("mkdir -p /etc/chef/trusted_certs")
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
            expect(installer).to receive(:run_command).with("mkdir -p /etc/chef/trusted_certs")
            expect(installer).to receive(:fetch_certificate).with(chef_server_fqdn)
            expect(installer).to receive(:fetch_certificate).with(delivery_server_fqdn)
            expect(installer).to receive(:fetch_certificate).with(supermarket_fqdn)
            installer.fetch_certificates
          end
        end
      end

      describe "#copy_custom_certificate_chain" do
        context "when the user did not pass --fips-custom-cert-filename" do
          before do
            allow(install_config).to receive(:fips_custom_cert_filename).and_return(nil)
          end

          it "does nothing" do
            expect(installer).not_to receive(:copy_file)
            expect(installer).not_to receive(:puts_substep)
            installer.copy_custom_certificate_chain
          end
        end

        context "when the user passed --fips-custom-cert-filename" do
          let(:filepath) { "/file/path" }
          let(:runner_destination) { "#{temp_path}/installer/custom_certificate_chain.crt" }
          before do
            allow(install_config).to receive(:fips_custom_cert_filename).and_return(filepath)
          end

          it "copies the custom cert file onto the runner" do
            expect(installer).to receive(:copy_file).with(filepath, runner_destination)
            expect(installer).to receive(:puts_substep)
            installer.copy_custom_certificate_chain
          end
        end
      end

      describe "#copy_keys" do
        before(:each) do
          CtlHelpers::DeliveryConfig.delivery[:chef_private_key] = "/etc/delivery/delivery.pem"
        end
        it "copies keys to the correct path on the remote host" do
          expect(installer).to receive(:copy_file).with("/etc/delivery/builder_key",
                                                        "#{temp_path}/installer/builder_key")

          expect(installer).to receive(:copy_file).with(
            "/etc/delivery/delivery.pem",
            "#{temp_path}/installer/delivery.pem")
          installer.copy_keys
        end
      end

      describe "#install_chefdk!" do
        shared_examples_for "deb-based systems" do
          it "installs via dpkg" do
            expect(connection).to receive(:os_name).and_return(os)
            expect(installer).to receive(:run_command).with("dpkg -i #{temp_path}/chef_dk.deb")
            installer.install_chefdk!
          end
        end

        context "when the os is ubuntu" do
          let(:os) { 'ubuntu' }

          it_should_behave_like "deb-based systems"
        end

        context "when the os is debian" do
          let(:os) { 'debian' }

          it_should_behave_like "deb-based systems"
        end

        shared_examples_for "rpm-based systems" do
          it "installs via rpm" do
            expect(connection).to receive(:os_name).and_return(os)
            expect(installer).to receive(:run_command).with("rpm -Uvh --replacepkgs #{temp_path}/chef_dk.rpm")
            installer.install_chefdk!
          end
        end

        context  "when the os is centos" do
          let(:os) { 'centos' }

          it_should_behave_like "rpm-based systems"
        end

        context  "when the os is suse" do
          let(:os) { 'suse' }

          it_should_behave_like "rpm-based systems"
        end

        context  "when the os is redhat" do
          let(:os) { 'redhat' }

          it_should_behave_like "rpm-based systems"
        end

        context  "when the os is oracle" do
          let(:os) { 'oracle' }

          it_should_behave_like "rpm-based systems"
        end
      end

      describe "#register_node!" do
        context "when neither node nor client has been previously registered" do
          it "registers the runner with the chef server" do
            expect(local_knife).to receive(:bootstrap).with(install_config.username, install_config.password, install_config.ssh_identity_file, install_config.sudo, install_config.client_config, install_config.port)

            installer.register_node!
          end
        end
      end

      describe "#configure_node" do
        it "runs the remote configuration command from the correct location" do
          expect(installer).to receive(:run_command).
            with("sh -c 'cd #{temp_path}/installer && ./install-build-node.sh'")
          installer.configure_node
        end
      end

      describe "#configure_job_runner" do
        let(:public_key) { 'openssh_public_key_string' }
        let(:sane_paths) { 'PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' }
        let(:ohai_hash) do
          {
            "platform" => "centos",
            "platform_family" => "rhel",
            "platform_version" =>"7.2.1511",
            "os" => "linux"
          }
        end

        it "registers the runner with the local delivery instance" do
          expect(local_knife).to receive(:gather_node_os_info_hash).and_return(ohai_hash)
          expect(install_config).to receive(:fqdn).and_return("dazzle-runner-1.local.dev")
          ohai_hash['hostname'] = "dazzle-runner-1.local.dev"
          expect(client).to receive(:create_runner).with(ohai_hash).and_return(public_key)
          expect(installer).to receive(:run_command).with("#{sane_paths} chef-apply #{temp_path}/installer/install-job-runner.rb 'openssh_public_key_string'")

          installer.configure_job_runner
        end
      end

      describe "#cleanup" do
        it "runs removes the installation directory upon completion" do
          expect(installer).to receive(:run_command).with("rm -rf #{temp_path}/")
          installer.cleanup
        end
      end

      describe "#output_error" do
        it "passes the error message to puts_error with no extra info" do
          expect(installer).to receive(:puts_error).with("oopsie", nil)
          installer.output_error(Runner::Exceptions::BaseError.new("oopsie"))
        end

        it "passes the associated message and remediation steps to puts_error" do
          expect(installer).to receive(:puts_error).with("oopsie", "Fix it.")
          installer.output_error(Runner::Exceptions::BaseError.new("oopsie", "Fix it."))
        end

        it "writes the exception class and message to the log as a fatal error" do
          expect(logger).to receive(:fatal).with(/.*BaseError.*oopsie.*/)
          installer.output_error(Runner::Exceptions::BaseError.new("oopsie", "Fix it."))
        end
      end

      describe "#fetch_certificate" do
        let(:fqdn) { "server.example.com" }
        let(:cert_filename) { "server_example_com" }

        it "fetches the server SSL certificate" do
          expect(installer).to receive(:run_command).with(
            "/opt/chefdk/bin/knife ssl fetch --server https://#{fqdn} --config-option trusted_certs_dir=/etc/chef/trusted_certs"
          )
          installer.fetch_certificate(fqdn)
        end
      end

      describe "#copy_file" do
        let(:source) { "local/source" }
        let(:destination) { "remote/destination" }

        it "records the copied file destination on success" do
          expect(connection).to receive(:copy_file).with(source, destination)
          installer.copy_file(source, destination)
          expect(installer.copied_files).to include(destination)
        end

        it "does not record the copied file destination on failure" do
          expect(connection).to receive(:copy_file).
              and_raise(Runner::Exceptions::RemoteCopyFailed)
          expect{ installer.copy_file(source, destination) }.
              to raise_error(Runner::Exceptions::RemoteCopyFailed)
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
              with("rm #{temp_path}/installer/keepme")
          installer.remove_sensitive_files
        end

        it "attempts to remotely delete only sensitive files" do
          %w{safe_1 sensitive_1 sensitive_2}.each { |actual| file_list << actual }
          expect(installer).to receive(:run_command).with("rm sensitive_1")
          expect(installer).to receive(:run_command).with("rm sensitive_2")
          installer.remove_sensitive_files
        end

        describe "displays manual removal instructions" do
          before(:each) do
            %w{safe_1 sensitive_1}.each { |actual| file_list << actual }
          end

          [Runner::Exceptions::RemoteExecutionFailed,
           Runner::Exceptions::RemoteConnectionFailed].each do |err|
            it "when the  error #{err} is received" do
              expect(installer).to receive(:run_command).and_raise(err)
              ["I was unable to remove the following sensitive file from the runner:",
               "",
               " * sensitive_1",
               "",
               "Please log into #{install_config.fqdn} and remove it."].each do |line|
                 expect(installer).to receive(:puts).with(line)
               end
               installer.remove_sensitive_files
            end
          end

          describe "and uses correct subject/object form when" do
            before(:each) do
              allow(installer).to receive(:run_command).with(/rm/).
                at_least(1).times.
                and_raise(Runner::Exceptions::RemoteExecutionFailed)
            end

            it "a single file can't be removed" do
              file_list << "sensitive_2"
              expect(installer).to receive(:puts).with(/sensitive files/)
              expect(installer).to receive(:puts).with(/ \* /).exactly(2).times
              expect(installer).to receive(:puts).with(/remove them/)
              installer.remove_sensitive_files
            end

            it "multiple files cannot be removed" do
              expect(installer).to receive(:puts).with(/sensitive file/)
              expect(installer).to receive(:puts).with(/ \*/).once
              expect(installer).to receive(:puts).with(/remove it/)
              installer.remove_sensitive_files
            end
          end
        end
      end # end remove_sensitive_files
    end
  end
end
