#!/opt/chefdk/embedded/bin/ruby
# encoding: utf-8

# Usage:
# ./script.rb 'compliance-2017.03.18'
# ./script.rb 'compliance-*'

require 'net/http'
require 'openssl'
require 'yaml'
require 'json'
require 'colorize'
require 'pp'

require_relative '../lib/elastic/scans'

### This version only needs the summary per profile totals, but could not get the order to work by failures because of
### ElasticSearch BUG for order: https://github.com/elastic/elasticsearch/issues/16838#issuecomment-190210463
# BODY = '{
#   "size": 0,
#   "query": {
#     "terms": {
#       "_id": ARRAY_HERE
#     }
#   },
#   "aggs": {
#     "environments": {
#       "terms": {
#         "field": "environment",
#         "size": 20
#       },
#       "aggs": {
#         "profiles": {
#            "nested": {
#              "path": "profiles_sums"
#            },
#            "aggs": {
#              "failures": {
#                "sum": {
#                  "field": "profiles_sums.controls.failed.total"
#                 }
#              }
#           }
#         }
#       }
#     }
#   }
# }'

BODY = '{
  "size": 0,
  "query": {
    "ids" : {
      "type" : "inspec_summary",
      "values" : ARRAY_HERE
    }
  },
  "aggs": {
    "environments": {
      "terms": {
        "field": "environment",
        "order": { "failed_nodes": "desc" }
      },
      "aggs": {
        "failed_nodes": {
          "filter": {
            "term": {
              "status": "failed"
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
request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took,aggregations.environments.buckets.key,aggregations.environments.buckets.failed_nodes")
Scans.add_header(request, dest['target_opts']['header'])

request.body = BODY.gsub('ARRAY_HERE',report_ids.to_s)
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Failures by environment response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"

pp elastic_out
