require 'spec_helper'

require 'build-node/config'
require 'build-node/exceptions'
require 'build-node/local_delivery'
require 'ctl-helpers/prompt'
require 'json'

describe BuildNode::LocalDelivery do
  let(:local_delivery) { BuildNode::LocalDelivery.new(config) }

  # mock objects
  let(:config) { BuildNode::Config.new({}) }

  # delivery-ctl configuration values
  let(:fqdn) { 'build-node' }

  let(:api_client) { double(HTTPClient) }
  let(:ssl_config) { double(HTTPClient::SSLConfig) }

  before(:each) do
    config.set_fqdn(fqdn)
    config.set_admin_user("admin")
    config.set_admin_token("token")
    config.set_enterprise("enterprise")
    allow(CtlHelpers::DeliveryConfig).to receive(:delivery_fqdn).and_return("delivery.fqdn")
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
    let(:expected_headers) do
      { "content-type" => "application/json",
        "chef-delivery-user" => "admin",
        "chef-delivery-token" => "token" }
    end
    let(:expected_payload) { JSON.generate(payload_hash) }
    let(:expected_endpoint) { "https://delivery.fqdn/workflow/api/v0/e/enterprise/runners" }

    it "sends an authenticated POST request using the token gathered from user input" do
      expect(HTTPClient).to receive(:new).with(default_header: expected_headers).and_return(api_client)
      expect(api_client).to receive(:ssl_config).and_return(ssl_config).twice
      expect(ssl_config).to receive(:add_trust_ca).with("/var/opt/delivery/nginx/ca/delivery.fqdn.crt")
      expect(ssl_config).to receive(:load_trust_ca).with(no_args)
      expect(api_client).to receive(:post).with(expected_endpoint, expected_payload)
        .and_return(double('response', status: 200, body: "{\"health\": {}, \"hostname\": \"build-node\", \"id\": \"77122b00-c8d2-4f1a-9648-1fad15f6cb52\", \"openssh_public_key\": \"ssh-rsa BLAH job_runner@build-node\\n\"}"))
      actual = local_delivery.create_runner(payload_hash)
      expect(actual['openssh_public_key']).to eq("ssh-rsa BLAH job_runner@build-node\n")
    end

    describe "when delivery_fqdn is configured as an array" do
      it "posts the request to the first FQDN of the array" do
        expect(CtlHelpers::DeliveryConfig).to receive(:delivery_fqdn).and_return(["delivery.fqdn", "second.delivery.fqdn"])
        expect(HTTPClient).to receive(:new).with(default_header: expected_headers).and_return(api_client)
        expect(api_client).to receive(:ssl_config).and_return(ssl_config).twice
        expect(ssl_config).to receive(:add_trust_ca).with("/var/opt/delivery/nginx/ca/delivery.fqdn.crt")
        expect(ssl_config).to receive(:load_trust_ca).with(no_args)
        expect(api_client).to receive(:post).with(expected_endpoint, expected_payload)
          .and_return(double('response', status: 200, body: "{\"health\": {}, \"hostname\": \"build-node\", \"id\": \"77122b00-c8d2-4f1a-9648-1fad15f6cb52\", \"openssh_public_key\": \"ssh-rsa BLAH job_runner@build-node\\n\"}"))

        local_delivery.create_runner(payload_hash)
      end
    end
  end
end
