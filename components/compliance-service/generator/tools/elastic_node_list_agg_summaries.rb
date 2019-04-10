#!/opt/chefdk/embedded/bin/ruby
# encoding: utf-8

# Usage:
# ./script.rb 'insights-2017.03.02'
# ./script.rb 'insights-*'

require 'net/http'
require 'openssl'
require 'yaml'
require 'json'
require 'colorize'
require 'pp'

require_relative '../lib/elastic/scans'

BODY = '{
  "size": 10000,
  "query": {
    "ids" : {
      "type" : "inspec_summary",
      "values" : ARRAY_HERE
    }
  }
}'

es_index = ARGV.first
fail "ERROR: you must provide the index to query as argument!" unless es_index

dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))
report_ids = Scans.report_ids(dest, es_index)

@http = Scans.prep_http(dest)
path = dest['target_opts']['path']

request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took," +
  'hits.hits._source.node_uuid,' +
  'hits.hits._source.environment,' +
  'hits.hits._source.platform,' +
  'hits.hits._source.controls.failed,' +
  'hits.hits._source.node_name,' +
  'hits.hits._source.end_time,' +
  '')
Scans.add_header(request, dest['target_opts']['header'])
my_body = BODY.gsub('ARRAY_HERE',report_ids.to_s)
#puts my_body
request.body = my_body
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Node list aggregation response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"

pp elastic_out
