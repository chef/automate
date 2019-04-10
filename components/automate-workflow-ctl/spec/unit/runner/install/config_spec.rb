require 'spec_helper.rb'

require 'runner/install/config'
require 'runner/exceptions'

require 'ctl-helpers/prompt'

describe Runner::Install::Config do
  def mock_file_state(filename, opts)
    opts.each do |k, v|
      allow(File).to receive(k).with(filename).and_return(v)
    end
  end

  let(:args) { [] }
  let(:config) {
    described_class.new(args, "description")
  }

  describe "#validate_installer_options" do
    before do
      config.options[:installer] = "installer"
      config.options[:chefdk_version] = "version"
    end

    it "raises an error when installer and chefdk_version are specified" do
      expect{config.validate_installer_options}.to raise_error(Runner::Exceptions::BadArgumentError)
    end
  end

  describe "#verify_file!" do
    let(:filename) { "/some/file/path" }
    it "raises an error when the file can't be read" do
      mock_file_state(filename, exist?: true, readable?: false)
      expect{config.verify_file!(filename)}
        .to raise_error Runner::Exceptions::BadArgumentError
    end

    context "when the file doesn't exist" do
      it "raises an error" do
        expect{config.verify_file!(filename)}
          .to raise_error Runner::Exceptions::BadArgumentError
      end

      context "when a custom missing exception is passed" do
        let(:exception_object) { Runner::Exceptions::CustomCertfileNotFound.new(filename) }

        it "raises the custom exception" do
          expect{config.verify_file!(filename,
                                     exception_object)}
            .to raise_error Runner::Exceptions::CustomCertfileNotFound
        end
      end
    end

    context "when the file is valid" do
      it "returns the filename" do
        mock_file_state(filename, exist?: true, readable?: true)
        expect(config.verify_file!(filename)).to eq(filename)
      end
    end
  end

  describe "#validate_port!" do
    context "when the port is invalid" do
      [0, 65536, "hello"].each do |port|
        it "rejects the invalid port '#{port}' with a BadArgumenterror" do
          expect{config.validate_port!(port)}.to raise_error Runner::Exceptions::BadArgumentError
        end
      end
    end

    context "when the port is valid" do
      it "returns the port" do
        expect(config.validate_port!(22)).to eq(22)
      end
    end
  end

  context "#validate!" do
    shared_examples_for "missing argument" do
      it "throws an error" do
        expect { config.validate! }.to raise_error(Runner::Exceptions::BadArgumentError)
      end
    end

    context "when fqdn is not passed" do
      it_behaves_like "missing argument"
    end

    context "when fqdn is passed" do
      let(:fqdn) { "some_fqdn" }

      context "when username is passed" do
        let(:username) { "some_username" }
        let(:args) { [fqdn, username, "--password", "password"] }

        it "assigns a value for fqdn" do
          config.validate!
          expect(config.fqdn).to eq(fqdn)
        end

        it "assigns a value for username" do
          config.validate!
          expect(config.username).to eq(username)
        end

        context "when password flag is passed in but password is not" do
          let(:password) { "password" }
          let(:args) { [fqdn, username, "--password"] }

          it "prompts for password" do
            expect(CtlHelpers::Prompt).to receive(:get_user_response).with(false).and_return(password)
            config.validate!
            expect(config.password).to eq(password)
          end
        end
      end

      context "when username is not passed" do
        it_behaves_like "missing argument"
      end
    end
  end

  context "#confirm_config" do
    let(:sudo) { true }
    let(:node_registration_status) { "node_exists" }
    it "records sudo" do
      expect(config).to receive(:display_config).with(node_registration_status)
      expect(config).to receive(:prompt_for_config)
      config.confirm_config(node_registration_status)
    end
  end
end
