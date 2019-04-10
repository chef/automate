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

# count a failed control only once:
# {
#   "size": 0,
#   "query": {
#     "ids" : {
#       "type" : "inspec_report",
#       "values" : ["44024b50-2e0d-42fa-a57c-25e05e48a1b5","bb93e1b2-36d6-439e-ac70-a41504242605", "3ca95021-84c1-43a6-a2e7-be10edcb238d"]
#     }
#   },
#   "aggs" : {
#     "controls-agg" : {
#       "nested" : { "path" : "profiles_min.controls" },
#       "aggs": {
#         "failed-agg": {
#           "filter": { "term": { "profiles_min.controls.status": "failed" } },
#           "aggs": {
#             "id-agg": { "cardinality": { "field": "profiles_min.controls.id" } }
#           }
#         }
#       }
#     }
#   }
# }


# this in one go with filter aggregation:
BODY_TEMPLATE = '{
  "size": 0,
  "query": {
    "type": {
      "value": "inspec_report"
    }
  },
  "aggs" : { AGGS_HERE }
}'

AGG_TEMPLATE = '
    "DATE_HERE" : {
      "filter" : {  "terms": { "_id": ARRAY_HERE } },
      "aggs": {
        "controls-agg" : {
          "nested" : { "path" : "profiles_min.controls" },
          "aggs": {
            "failed-agg": {
              "filter": { "term": { "profiles_min.controls.status": "failed" } },
              "aggs": {
                "id-agg": { "cardinality": { "field": "profiles_min.controls.id" } }
              }
            },
            "passed-agg": {
              "filter": { "term": { "profiles_min.controls.status": "passed" } },
              "aggs": {
                "id-agg": { "cardinality": { "field": "profiles_min.controls.id" } }
              }
            },
            "skipped-agg": {
              "filter": { "term": { "profiles_min.controls.status": "skipped" } },
              "aggs": {
                "id-agg": { "cardinality": { "field": "profiles_min.controls.id" } }
              }
            }
          }
        }
      }
    }'
@es_index = ARGV.first
fail "ERROR: you must provide the index to query as argument!" unless @es_index

@dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))

# trend graph for 60 days with intervals of 5
report_ids = Scans.report_ids_buckets(@dest, @es_index, 60, 10)

aggs = ''
report_ids.each { |key, value|
  aggs += ',' if aggs != ''
  aggs += AGG_TEMPLATE.gsub('DATE_HERE',key).gsub('ARRAY_HERE',value.to_s)
}

@http = Scans.prep_http(@dest)
path = @dest['target_opts']['path']

request = Net::HTTP::Post.new("#{path}/#{@es_index}/_search?filter_path=took,aggregations")
Scans.add_header(request, @dest['target_opts']['header'])

request.body = BODY_TEMPLATE.gsub('AGGS_HERE',aggs)

#print request.body

response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Global compliancy controls response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"
pp elastic_out['aggregations'].sort
