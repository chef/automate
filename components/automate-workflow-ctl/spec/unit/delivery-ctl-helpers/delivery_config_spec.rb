require 'spec_helper.rb'

require 'ctl-helpers/delivery_config'
require 'ctl-helpers/exceptions'

describe CtlHelpers::DeliveryConfig do

  # delivery.rb configuration values
  let(:delivery_fqdn) { 'delivery-server.example.com' }
  let(:delivery_user) { 'delivery-user' }
  let(:delivery_user_key) { 'delivery-user.pem' }
  let(:chef_server_url) { 'https://chef-server/organizations/delivery-org' }
  let(:chef_server_fqdn) { 'chef-server' }

  let(:default_content) {
<<EOC
delivery_fqdn '#{delivery_fqdn}'
delivery['chef_username'] = '#{delivery_user}'
delivery['chef_private_key'] = '#{delivery_user_key}'
delivery['chef_server'] = '#{chef_server_url}'
delivery['chef_server_fqdn'] = '#{chef_server_fqdn}'
EOC
  }

  DeliveryConfig = CtlHelpers::DeliveryConfig
  before(:each) do
    # Reset the configuration between tests, otherwise it will load
    # previous configuration state.
    DeliveryConfig.reset
  end

  describe ".value_defaulted?" do
    it "returns true when the looked up value was auto-created and has no nested elements beneath it"  do
      DeliveryConfig[:test]
      expect(DeliveryConfig.value_defaulted?(DeliveryConfig[:test])).to eq(true)
    end

    it "returns false when the looked up value was auto-created and has nested elements beneath it" do
      DeliveryConfig[:test][:answer]
      expect(DeliveryConfig.value_defaulted?(DeliveryConfig[:test])).to eq(false)
    end

    it "returns false when the looked up value was not auto-created" do
      DeliveryConfig[:test] = 1
      expect(DeliveryConfig.value_defaulted?(DeliveryConfig[:test])).to eq(false)
    end

    it "returns false when the looked up value is autocreated then replaced with a set value" do
      DeliveryConfig[:test]
      DeliveryConfig[:test] = 1
      expect(DeliveryConfig.value_defaulted?(DeliveryConfig[:test])).to eq(false)
    end
  end

  describe "#validate!" do
    it "successfully loads when valid configuration options are present that we don't declare" do
      default_content << "insights['enabled'] = true"
      with_delivery_rb(default_content) do
        DeliveryConfig.validate!
      end
    end

    it "successfully loads when valid nested configuration options options are present" do
      default_content<< "delivery['testnest']['unknown']['as']['far']['as']['we']['want'] = true"
      with_delivery_rb(default_content) do
        DeliveryConfig.validate!
      end
    end

    it "correctly returns a set nested value no matter how it's set or accessed" do
      default_content << "delivery.deep['nested'].value = 'hello'"
      with_delivery_rb(default_content) do
        DeliveryConfig.validate!
        expect(DeliveryConfig['delivery']['deep']['nested']['value']).to eq('hello')
        expect(DeliveryConfig.delivery.deep.nested.value).to eq('hello')
        expect(DeliveryConfig.delivery['deep'].nested[:value]).to eq('hello')
      end
    end

    it "raises a ConfigurationError when the delivery config does not exist" do
      error_msg = "Missing required configuration file '/etc/delivery/delivery.rb'"
      expect(DeliveryConfig).to receive(:from_file).
        and_raise(Errno::ENOENT, error_msg)
      expect{ DeliveryConfig.validate! }.
        to raise_error(CtlHelpers::Exceptions::ConfigurationError, /#{error_msg}/)
    end

    it "raises a ConfigurationError when the delivery config cannot be read" do
      error_msg = "Unreadable file '/etc/delivery/delivery.rb'"
      expect(DeliveryConfig).to receive(:from_file).
        and_raise(IOError, error_msg)
      expect{ DeliveryConfig.validate! }.
        to raise_error(CtlHelpers::Exceptions::ConfigurationError, /#{error_msg}/)
    end

    it "raises a ConfigurationError when the delivery config is missing :delivery_fqdn" do
      content = <<-EOC
delivery['chef_username'] = "#{delivery_user}"
delivery['chef_private_key'] = "#{delivery_user_key}"
delivery['chef_server'] = "#{chef_server_url}"
delivery['chef_server_fqdn'] = "#{chef_server_fqdn}"
EOC
      with_delivery_rb(content) do
        expect{ DeliveryConfig.validate! }.to raise_error(
          CtlHelpers::Exceptions::ConfigurationError,
          /Missing configuration option 'delivery_fqdn'/
        )
      end
    end

    it "raises a ConfigurationError when the delivery config is missing delivery['chef_username']" do
      content = <<-EOC
delivery_fqdn "#{delivery_fqdn}"
delivery['chef_private_key'] = "#{delivery_user_key}"
delivery['chef_server'] = "#{chef_server_url}"
delivery['chef_server_fqdn'] = "#{chef_server_fqdn}"
EOC
      with_delivery_rb(content) do
        expect{ DeliveryConfig.validate! }.to raise_error(
          CtlHelpers::Exceptions::ConfigurationError,
          /Missing configuration option 'delivery\['chef_username'\]'/
        )
      end
    end

    it "raises a ConfigurationError when the delivery config is missing delivery['chef_private_key']" do
      content = <<-EOC
delivery_fqdn "#{delivery_fqdn}"
delivery['chef_username'] = "#{delivery_user}"
delivery['chef_server'] = "#{chef_server_url}"
delivery['chef_server_fqdn'] = "#{chef_server_fqdn}"
EOC
      with_delivery_rb(content) do
        expect{ DeliveryConfig.validate! }.to raise_error(
          CtlHelpers::Exceptions::ConfigurationError,
          /Missing configuration option 'delivery\['chef_private_key'\]'/
        )
      end
    end

    it "raises a ConfigurationError when the delivery config is missing delivery['chef_server']" do
      content = <<-EOC
delivery_fqdn "#{delivery_fqdn}"
delivery['chef_username'] = "#{delivery_user}"
delivery['chef_private_key'] = "#{delivery_user_key}"
delivery['chef_server_fqdn'] = "#{chef_server_fqdn}"
EOC
      with_delivery_rb(content) do
        expect{ DeliveryConfig.validate! }.to raise_error(
          CtlHelpers::Exceptions::ConfigurationError,
          /Missing configuration option 'delivery\['chef_server'\]'/
        )
      end
    end
  end

end
