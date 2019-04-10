#!/opt/chefdk/embedded/bin/ruby
# encoding: utf-8

# Usage:
# ./script.rb 'insights-2017.03.18'
# ./script.rb 'insights-*'

require 'net/http'
require 'openssl'
require 'yaml'
require 'json'
require 'colorize'
require 'pp'

require_relative '../lib/elastic/scans'

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!! REQUIRES MAPPING TO HAVE THE profiles_min.controls ARRAY AS "type": "nested" !!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# document type is filtered in the URL (... #{es_index}/inspec_report/_search)
# get all nodes that failed a specific control id
target_control = 'sysctl-28'
BODY = '{
  "size": 20,
  "_source": ["node_uuid", "node_name"],
  "query": {
    "nested": {
      "path": "profiles_min.controls",
      "query": {
        "bool": {
          "must": [
            {
              "terms": {
                "_id": ARRAY_HERE
              }
            },
            {
              "term": {
                "profiles_min.controls.id": "CONTROL_HERE"
              }
            },
            {
              "term": {
                "profiles_min.controls.status": "failed"
              }
            }
          ]
        }
      }
    }
  }
}'

es_index = ARGV.first
fail "ERROR: you must provide the index to query as argument!" unless es_index

dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))
report_ids = Scans.report_ids(dest, es_index)

@http = Scans.prep_http(dest)
path = dest['target_opts']['path']
request = Net::HTTP::Post.new("#{path}/#{es_index}/inspec_report/_search?filter_path=took,hits.total,hits.hits._id,hits.hits._source")
Scans.add_header(request, dest['target_opts']['header'])
my_body = BODY.gsub('ARRAY_HERE',report_ids.to_s).gsub('CONTROL_HERE',target_control)
#puts my_body

request.body = my_body
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_controls = JSON.parse(response.body)
puts "*** Nodes for a failed control('#{target_control}') response #{response.code} #{response.message} in "+(elastic_controls['took']/1000.0).to_s.yellow+" seconds"
pp elastic_controls
