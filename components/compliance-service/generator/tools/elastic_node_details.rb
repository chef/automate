#!/opt/chefdk/embedded/bin/ruby
# encoding: utf-8

# Usage:
# ./script.rb 'compliance-2017.03.02' 4360a1c0-8744-4212-a385-56fcf6224ec4
# ./script.rb 'compliance-*' 4360a1c0-8744-4212-a385-56fcf6224ec4

require 'net/http'
require 'openssl'
require 'yaml'
require 'json'
require 'colorize'
require 'pp'

require_relative '../lib/elastic/scans'

BODY1 = '{
  "size": 1,
  "query": {
    "bool": {
      "must": [
        {
          "type": {
            "value": "inspec_summary"
          }
        },
        {
          "term": {
            "node_uuid": "NODE_UUID"
          }
        }
      ]
    }
  },
  "sort":{"end_time":{"order":"desc"}}
}'

es_index = ARGV[0]
node_uuid = ARGV[1]
fail "ERROR: you must provide index and node uuid arguments!" unless es_index && node_uuid

dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))

@http = Scans.prep_http(dest)
path = dest['target_opts']['path']

request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took," +
  'hits.hits._source,' +
  'hits.hits._id,' +
  '')
Scans.add_header(request, dest['target_opts']['header'])
request.body = BODY1.gsub('NODE_UUID', node_uuid)

response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Node details response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"
pp elastic_out
puts '-------------------------------------'

# get more details from inspec_report
BODY2 = '{
  "size": 1,
  "query": {
    "ids" : {
      "type" : "inspec_report",
      "values" : "REPORT_ID_HERE"
    }
  }
}'

request = Net::HTTP::Get.new("#{path}/#{es_index}/_search?filter_path=hits.hits._source")
request.body = BODY2.gsub('REPORT_ID_HERE', elastic_out['hits'].first[1].first['_id'])
Scans.add_header(request, dest['target_opts']['header'])
response = @http.request(request)
elastic_out = JSON.parse(response.body)
pp elastic_out
