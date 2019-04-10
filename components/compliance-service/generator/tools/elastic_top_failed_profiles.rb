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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!! REQUIRES MAPPING TO HAVE THE PROFILES ARRAY AS "type": "nested" !!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
BODY1 = '{
  "size": 0,
  "query": {
    "ids" : {
      "type" : "inspec_summary",
      "values" : ARRAY_HERE
    }
  },
  "aggs" : {
    "profiles" : {
      "nested" : {
        "path" : "profiles_sums"
      },
      "aggs": {
        "totals": {
          "terms": {
            "field": "profiles_sums.profile",
            "order": { "failed_nodes": "desc" }
          },
          "aggs": {
            "failed_nodes": {
              "filter": {
                "range": { "profiles_sums.controls.failed.total": { "gt": 0 } }
              }
            }
          }
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

request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took,aggregations.profiles.totals.buckets.key,aggregations.profiles.totals.buckets.failed_nodes")
Scans.add_header(request, dest['target_opts']['header'])

request.body = BODY1.gsub('ARRAY_HERE',report_ids.to_s)
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
# extract the profile ids from the keys
profile_ids = elastic_out['aggregations']['profiles']['totals']['buckets'].map { |buck|
  buck['key'].split('|').last
}
puts "*** Overall compliance response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"

BODY2 = '{
  "size": 10000,
  "query": {
    "ids" : {
      "type" : "inspec_profile",
      "values" : ARRAY_HERE
    }
  },
  "_source": "title"
}'

request = Net::HTTP::Post.new("#{path}/compliance-profiles/_search?filter_path=took,hits.hits._source,hits.hits._id")
Scans.add_header(request, dest['target_opts']['header'])
my_body = BODY2.gsub('ARRAY_HERE',profile_ids.to_s)
#puts my_body
request.body = my_body
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
profiles_extras = JSON.parse(response.body)['hits']['hits']
pp elastic_out['aggregations']['profiles']['totals']['buckets'].map { |bucket|
  profile = profiles_extras.find { |p| p['_id'] == bucket['key'].split('|').last}
  bucket['title'] = profile['_source']['title']
  bucket
}
