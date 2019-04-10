require 'httpclient'
require 'json'

require 'build-node/exceptions'

module BuildNode
  # Wraps API calls to the local Delivery Server
  class LocalDelivery
    def initialize(config)
      @config = config
    end

    def create_runner(runner_info)
      client = HTTPClient.new(default_header: {
        'content-type' => 'application/json',
        'chef-delivery-user' => @config.admin_user,
        'chef-delivery-token' => @config.admin_token
      })
      client.ssl_config.add_trust_ca("/var/opt/delivery/nginx/ca/#{delivery_fqdn}.crt")
      client.ssl_config.load_trust_ca

      endpoint = "https://#{delivery_fqdn}/workflow/api/v0/e/#{@config.enterprise}/runners"
      res = client.post(endpoint, JSON.generate(runner_info))

      unless res.status == 200
        raise BuildNode::Exceptions::DeliveryAPIRequestFailed.new("Automate returned status #{res.status}")
      end

      JSON.parse(res.body)
    end

    private

    def delivery_fqdn
      [CtlHelpers::DeliveryConfig.delivery_fqdn].flatten.first
    end
  end
end
