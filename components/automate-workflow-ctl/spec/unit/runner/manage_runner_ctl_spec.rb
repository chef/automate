require 'spec_helper'

require 'runner/manage_runner_ctl'

require 'runner/install/config'
require 'runner/exceptions'
require 'ctl-helpers/prompt'
require 'json'

describe Runner::ManageRunnerCtl do
  let(:config) { Runner::Install::Config.new([], "desc") }

  let(:client) { Runner::ManageRunnerCtl.new(config) }

  # delivery-ctl configuration values
  let(:fqdn) { 'build-node' }
  let(:enterprise) { 'enterprise' }

  let(:http_client) { instance_double(HTTPClient, ssl_config: ssl_config) }
  let(:ssl_config) { instance_double(HTTPClient::SSLConfig) }

  let(:expected_headers) do
    { "content-type" => "application/json" }
  end

  before(:each) do
    config.instance_variable_set(:@fqdn, fqdn)
    config.instance_variable_set(:@enterprise, enterprise)

    allow(CtlHelpers::DeliveryConfig)
      .to receive(:delivery_fqdn).and_return("delivery.fqdn")
  end

  describe "#initialize" do
    context "when the enterprise is passed via the config" do
      it "returns the configured enterprise" do
        expect(client.enterprise).to eq("enterprise")
      end
    end

    context "when the enterprise is not passed via the config" do
      let(:enterprise) { nil }

      it "returns the empty string" do
        expect(client.enterprise).to eq("")
      end
    end
  end

  describe "#create_runner" do
    let(:payload_hash) do
      {
        "platform" => "centos",
        "platform_family" => "rhel",
        "platform_version" =>"7.2.1511",
        "os" => "linux",
        "hostname" => "some.host"
      }
    end
    let(:status) {double("Shellout Status")}
    let(:shellout) { instance_double(Mixlib::ShellOut, run_command: status) }

    before do
      expect(Mixlib::ShellOut).to receive(:new).and_return(shellout)
    end

    context "runner-ctl is successful" do
      it "returns the stdout provided by the shellout command" do
        expect(status).to receive(:error?).and_return(false)
        expect(status).to receive(:stdout).and_return("stdout")
        expect(client.create_runner(payload_hash)).to eq("stdout")
      end
    end

    context "runner-ctl fails" do
      it "returns the stdout provided by the shellout command" do
        expect(status).to receive(:error?).and_return(true)
        expect(status).to receive(:stdout).and_return("stdout")
        expect(status).to receive(:stderr).and_return("stderr")

        expect{client.create_runner(payload_hash)}.to raise_error(Runner::Exceptions::RunnerCtlFailed, "\nThe runner could not be created because of the following error:\n\n stdout\n stderr\n")
      end
    end
  end

  describe "#delete_runner" do
    let(:payload_hash) do
      {
        "hostname" => "some.host"
      }
    end
    let(:status) {double("Shellout Status")}
    let(:shellout) { instance_double(Mixlib::ShellOut, run_command: status) }

    before do
      expect(Mixlib::ShellOut).to receive(:new).and_return(shellout)
    end

    context "runner-ctl is successful" do
      it "returns the stdout provided by the shellout command" do
        expect(status).to receive(:error?).and_return(false)
        expect(status).to receive(:stdout).and_return("stdout")
        expect(client.delete_runner(payload_hash)).to eq("stdout")
      end
    end

    context "runner-ctl fails" do
      it "returns the stdout provided by the shellout command" do
        expect(status).to receive(:error?).and_return(true)
        expect(status).to receive(:stdout).and_return("stdout")
        expect(status).to receive(:stderr).and_return("stderr")

        expect{client.delete_runner(payload_hash)}.to raise_error(Runner::Exceptions::RunnerCtlFailed, "\nThe runner could not be deleted because of the following error:\n\n stdout\n stderr\n")
      end
    end
  end
end
