require 'hashie'
require 'json'

# this request type is used to test client authorization
class AutomateClientApiRequest < Inspec.resource(1)
  name 'automate_client_api_request'
  desc 'Simple wrapper around the http inspec resource with some presets for Automate 2.0'
  example "
    describe automate_client_api_request('/api/v0/_status') do
      its('http_status') { should cmp 200 }
      its('raw_response_body') { should cmp 'pong' }
    end
  "

  attr_accessor :http, :api_token

  def initialize(endpoint, api_token, http_method: 'GET', request_body: nil, request_headers: {}, request_params: nil)
    endpoint = "/#{endpoint}" unless endpoint.start_with?('/')
    url = "https://#{target_hostname}#{endpoint}"

    @api_token = api_token

    request_headers['x-data-collector-token'] = api_token

    @http = inspec.http(
      url,
      default_opts.merge({
        data: request_body,
        headers: request_headers,
        method: http_method,
        params: request_params
      })
    )
  end

  def http_status
    http.status
  end

  def raw_response_body
    http.body
  end

  def parsed_response_body
    return @response_body_json ||= begin
      JSON.parse(http.body, symbolize_names: true)
    end
  end

  def response_headers
    Hashie::Mash.new(response_headers)
  end

  private

  def default_opts
    {
      ssl_verify: do_ssl_verify?,
      enable_remote_worker: false
    }
  end

  def target_hostname
    if inspec.backend.respond_to?(:hostname)
      inspec.backend.hostname
    else
      "localhost"
    end
  end

  # Don't verify SSL for local deployments
  # We also don't do it when running against AWS, and we specify IP addresses
  #   for our targets there.
  def do_ssl_verify?
    if target_hostname == "localhost" ||
       target_hostname == "a2-dev.test" ||
       target_hostname =~ /\d+\.\d+\.\d+\.\d+/
      return false
    end
    return true
  end
end
