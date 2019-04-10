require 'uri'
require 'json'
require 'net/http'

module DataGenerator
  class ElasticsearchInsertClient
    def initialize
      url = ENV['ELASTICSEARCH_URL'] || 'http://localhost:9200'
      @uri = URI.parse(url)
      @http = Net::HTTP.new(@uri.host, @uri.port)
      if @uri.scheme == 'https'
        @http.verify_mode = OpenSSL::SSL::VERIFY_NONE
        @http.use_ssl = true
      end

      template = ::File.read('../mappings/node-state.json')

      @http.put('_template/node-state', template, headers)

      converge_template = ::File.read('../mappings/converge-history.json')

      @http.put('_template/converge-history', converge_template, headers)

      @buffer = StringIO.new
    end

    # Write bulk insert instructions into the buffer
    def create(opts)
      operation = {create: { _index: opts[:index], _type: opts[:type] }}
      operation[:create][:_id] = opts[:id] if opts[:id]
      @buffer.puts operation.to_json
      @buffer.puts opts[:body].to_json
    end

    def write
      response = @http.post('_bulk', @buffer.string, headers)

      response.value # This API throws an exception if the response isn't in the 2xx range
      @buffer = StringIO.new
    end


    private

    def headers
      headers = { 'Content-Type' => 'application/json' }
      headers
    end
  end
end
