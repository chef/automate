require 'spec_helper.rb'
require 'build-node/config'

require 'ctl-helpers/prompt'
require 'build-node/exceptions'

describe BuildNode::Config do
  def mock_password_response(response)
    expect(CtlHelpers::Prompt).to receive(:get_user_response).with(false).and_return(response)
  end

  def mock_user_response(response)
    expect(CtlHelpers::Prompt).to receive(:get_user_response).and_return(response)
  end

  def mock_file_state(filename, opts)
    opts.each do |k, v|
      allow(File).to receive(k).with(filename).and_return(v)
    end
  end

  let(:options) {
    {}
  }

  let(:config) {
    described_class.new(options)
  }

  describe "parse_args!" do
    let(:stdout) { double("stdout") }

    it "raises an error when it receives a command line flag it doesn't know about" do
      expect { described_class.parse_args!(["--hello", "world"], stdout) }.to raise_error(BuildNode::Exceptions::BadArgumentError)
    end

    [:installer, :fqdn, :port, :password, :username, :"job-dispatch-version",
     :"admin-user", :"admin-token"].each do |flag|
      name = flag.to_s.gsub("-", "_")
      it "returns the provided value for :#{name} when --#{flag} is given" do
        opts = described_class.parse_args!(["--#{flag}", "somevalue"], stdout)
        expect(opts[name.to_sym]).to eq("somevalue")
      end

      it "raises an error when --#{flag} is provided with no value" do
        expect { described_class.parse_args!(["--#{flag}"], stdout) }.to raise_error(BuildNode::Exceptions::BadArgumentError)
      end
    end

    it "returns the provided value for :ssh_identity_file when --ssh-identity-file is given" do
      opts = described_class.parse_args!(["--ssh-identity-file", "somevalue"], stdout)
      expect(opts[:ssh_identity_file]).to eq("somevalue")
    end
  end

  describe "#set_installer" do
    it "raises an error when the installer does not exist" do
      mock_file_state("/bad/file/here", exist?: false, readable?: true)
      expect{config.set_installer("/bad/file/here")}.to raise_error BuildNode::Exceptions::BadArgumentError
    end

    it "raises an error when :installer is provided but can't be read" do
      mock_file_state("/bad/file/here", exist?: true, readable?: false)
      expect{config.set_installer("/bad/file/here")}.to raise_error BuildNode::Exceptions::BadArgumentError
    end

    it "sets installer to the provided input when :installer option is not given" do
      mock_file_state("/a/file", exist?: true, readable?: true)
      mock_user_response("/a/file")
      config.set_installer(nil)
      expect(config.installer).to eq("/a/file")
    end
  end

  describe "#set_password" do
    it "sets password when it is provided" do
      config.set_password("password")
      expect(config.password).to eq("password")
    end

    it "prompts for and retains password when it is not provided" do
      mock_password_response("password")
      config.set_password(nil)
      expect(config.password).to eq("password")
    end

    it "displays a reminder to use 'none' if a key is provided and passwordless sudo is enabled" do
      mock_file_state("an/identity/key", exist?: true, readable?: true)
      config.set_ssh_identity_file("an/identity/key", true)
      expect(config).to receive(:prompt_user).with(/'none'/, false, :none)
      config.set_password(nil)
    end

    context "when an empty string is given at the prompt and a key is given" do
      it "is set to nil" do
        mock_file_state("an/identity/key", exist?: true, readable?: true)
        config.set_ssh_identity_file("an/identity/key", true)
        mock_user_response("\n")
        config.set_password(nil)
        expect(config.password).to eq(nil)
      end
    end
  end

  describe "#set_username" do
    it "sets username when it is provided passed through" do
      config.set_username("englebert")
      expect(config.username).to eq("englebert")
    end

    it "prompts for and retains username when it is not provided" do
      mock_user_response("englebert")
      config.set_username(nil)
      expect(config.username).to eq("englebert")
    end
  end

  describe "#set_fqdn" do
    it "sets fqdn when :fqdn is passed through" do
      config.set_fqdn("example.com")
      expect(config.fqdn).to eq("example.com")
    end

    it "sets fqdn to the provided input when :fqdn option is not given" do
      mock_user_response("example.com")
      config.set_fqdn(nil)
      expect(config.fqdn).to eq("example.com")
    end
  end

  describe "#set_port" do
    it "sets port when :port is passed through" do
      config.set_port(122)
      expect(config.port).to eq(122)
    end

    it "uses the default port of 22 when port is not passed through" do
      config.set_port(nil)
      expect(config.port).to eq(22)
    end

    context "and the port is invalid" do
      [0, 65536, "hello"].each do |port|
        it "rejects the invalid port '#{port}' with a BadArgumenterror" do
          expect{config.set_port(port)}.to raise_error BuildNode::Exceptions::BadArgumentError
        end
      end
    end
  end

  describe "#set_job_dispatch_version" do
    it "sets job_dispatch_version when :version is passed through and queries user for admin user name and token" do
      config.set_job_dispatch_version("v2")
      expect(config.job_dispatch_version).to eq("v2")
    end

    it "uses the default version of v1 when job_dispatch_version is not passed through" do
      config.set_job_dispatch_version(nil)
      expect(config.job_dispatch_version).to eq("v1")
    end

    context "when the version is invalid" do
      ["1", "v1.0", "blah", "v3"].each do |ver|
        it "rejects the invalid version '#{ver}' with a BadArgumenterror" do
          expect{config.set_job_dispatch_version(ver)}.to raise_error BuildNode::Exceptions::BadArgumentError
        end
      end
    end
  end

  describe "#set_admin_user" do
    it "sets admin_user when :admin_user is passed through" do
      config.set_admin_user("name_is_admin")
      expect(config.admin_user).to eq("name_is_admin")
    end

    it "queries the user for admin_user when :admin_user is not passed through but job dispatch version is v2" do
      config.set_job_dispatch_version("v2")
      mock_user_response("admin_user")
      config.set_admin_user(nil)
      expect(config.admin_user).to eq("admin_user")
    end
  end

  describe "#set_admin_token" do
    it "sets admin_token when :admin_token is passed through" do
      config.set_admin_token("token_is_this_one")
      expect(config.admin_token).to eq("token_is_this_one")
    end

    it "queries the user for admin_token when :admin_token is not passed through but job dispatch version is v2" do
      config.set_job_dispatch_version("v2")
      mock_password_response("token_is_this_one")
      config.set_admin_token(nil)
      expect(config.admin_token).to eq("token_is_this_one")
    end
  end

  describe "#set_enterprise" do
    it "sets enterprise when :enterprise is passed through" do
      config.set_enterprise("enterprise")
      expect(config.enterprise).to eq("enterprise")
    end

    it "queries the user for enterprise when :enterprise is not passed through but job dispatch version is v2" do
      config.set_job_dispatch_version("v2")
      mock_user_response("this_is_the_enterprise")
      config.set_enterprise(nil)
      expect(config.enterprise).to eq("this_is_the_enterprise")
    end
  end

  describe "#set_ssh_identity_file" do
    it "sets identity file when it is provided" do
      mock_file_state("an/identity/key", exist?: true, readable?: true)
      config.set_ssh_identity_file("an/identity/key", true)
      expect(config.ssh_identity_file).to eq("an/identity/key")
    end

    it "prompts for and retains file when it is not provided" do
      mock_file_state("an/identity/key", exist?: true, readable?: true)
      mock_user_response("an/identity/key")
      config.set_ssh_identity_file(nil, true)
      expect(config.ssh_identity_file).to eq("an/identity/key")
    end

    it "sets ssh_identity_file when :ssh_identity_file is passed through" do
      mock_file_state("an/identity/key", exist?: true, readable?: true)
      config.set_ssh_identity_file("an/identity/key", true)
      expect(config.ssh_identity_file).to eq("an/identity/key")
    end

    it "raises an error when the identity file does not exist" do
      mock_file_state("/bad/file/here", exist?: false)
      expect { config.set_ssh_identity_file("/bad/file/here", true) }.
        to raise_error BuildNode::Exceptions::BadArgumentError
    end

    it "raises an error when the identity file exists but is not readable" do
      mock_file_state("/bad/file/here", exist?: true, readable?: false)
      expect { config.set_ssh_identity_file("/bad/file/here", true) }.
        to raise_error BuildNode::Exceptions::BadArgumentError
    end

    context "when an empty string is given at the prompt" do
      it "is set to nil" do
        mock_user_response("\n")
        config.set_ssh_identity_file(nil, true)
        expect(config.ssh_identity_file).to eq(nil)
      end
    end
  end

  context "#validate_and_prompt!" do
    # Because we test the individual 'get_' function behaviors
    # on their own, this test is limited only to ensuring that
    # we call all of them when we're supposed to.
    it "queries for all supported options" do
      [:installer, :fqdn, :username, :password, :port].each do |key|
        expect(config).to receive("set_#{key}".to_sym).with(nil).and_return "roger"
      end
      config.validate_and_prompt!
    end
  end

  context "#overwrite_registration?" do
    it "prompts and returns that value when not set" do
      answer = true
      expect(CtlHelpers::Prompt).to receive(:yes_no_prompt).and_return(answer)
      expect(config.overwrite_registration?).to equal(answer)
    end

    it "pulls false from flags when set" do
      config = described_class.new(
          described_class.parse_args!(["--no-overwrite-registration"], double("stdout")))
      expect(config.overwrite_registration?).to equal(false)
    end

    it "pulls true from flags when set" do
      config = described_class.new(
          described_class.parse_args!(["--overwrite-registration"], double("stdout")))
      expect(config.overwrite_registration?).to equal(true)
    end
  end
end
