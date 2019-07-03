require 'hashie'
require 'json'
require 'pp'

class AutomateApiRequest < Inspec.resource(1)
  name 'automate_api_request'
  desc 'Simple wrapper around the http inspec resource with some presets for Automate 2.0'
  example "
    describe automate_api_request('/api/v0/_status') do
      its('http_status') { should cmp 200 }
      its('raw_response_body') { should cmp 'pong' }
    end
  "

  attr_accessor :user, :pass

  def initialize(endpoint, http_method: 'GET', request_body: nil, request_headers: {}, request_params: nil,
                 user: 'admin',
                 pass: ENV['AUTOMATE_API_DEFAULT_PASSWORD'] || 'chefautomate')
    endpoint = "/#{endpoint}" unless endpoint.start_with?('/')

    @url = "https://#{target_hostname}#{endpoint}"
    @user = user
    @pass = pass
    @request_headers = request_headers
    @request_body = request_body
    @request_params = request_params
    @http_method = http_method

    # Invalidate @@id_token class variable if the user or password has changed.
    if (defined? @@previous_user).nil?
      @@previous_user = user
    end

    if (defined? @@previous_pass).nil?
      @@previous_pass = pass
    end

    if @@previous_user != user
      @@previous_user = user
      @@id_token = nil
    end

    if @@previous_pass != pass
      @@previous_pass = pass
      @@id_token = nil
    end
  end

  def http
    @http ||= inspec.http(
      @url,
      default_opts.merge({
        data: @request_body,
        headers: request_headers,
        method: @http_method,
        params: @request_params
      })
    )
  end

  def request_headers
    @request_headers['Authorization'] = "bearer #{id_token}"
    @request_headers
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

  def id_token
    @@id_token ||= begin
      # init auth process
      resp = request("/dex/auth?client_id=automate-api&scope=openid+profile+email+offline_access+groups+federated:id&response_type=code+id_token&state=inspec&nonce=yeahnotreally&redirect_uri=urn:ietf:wg:oauth:2.0:oob")
      req = if loc = resp.headers.location
              # one connector only (local) => we're being redirected
              loc.match(%r'\?req=(.+)$')[1]
            else
              # pick form URL with request ID from connector selection
              resp.body.match(%r'"/dex/auth/local\?req=([^"]+)"')[1]
            end
      # fill form, send req along
      params = { login: user, password: pass, req: req }

      # Note: This happens to be the same URL: GET gives you the input boxes,
      #       POST sends the form. So, we don't bother GETing the boxes first.
      resp = request('/dex/auth/local', method: 'POST', params: params)
      # approval redirect, follow
      if resp.status != 303
        raise "AUTHN FAILED: {login: #{user}, pass: #{pass}}"
      end
      resp = request(resp.headers.location)
      # read code from OOB response
      code = resp.body.match(%r'value="([a-z0-9]+)" />')[1]
      resp = request('/dex/token',
                     method: 'POST',
                     headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
                     auth: { user: 'automate-api',
                             pass: '' },
                     data: form_data(grant_type: 'authorization_code',
                                     redirect_uri: 'urn:ietf:wg:oauth:2.0:oob',
                                     code: code))

      resp = JSON.parse(resp.body, symbolize_names: true)
      resp[:id_token]
    end
  end

  def form_data(kv = {})
    data = ""
    kv.each do |key, val|
      data << "#{key}=#{val}&"
    end

    data
  end

  def request(path, opts = {})
    path = "/#{path}" unless path.start_with?('/')
    url = "https://#{target_hostname}#{path}"
    req = inspec.http(url, default_opts.merge(opts))
    req.status # trigger request
    req        # return response
  end

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
