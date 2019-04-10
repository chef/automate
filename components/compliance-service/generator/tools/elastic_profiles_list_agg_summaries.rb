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
#!!!!!!!!!!!! REQUIRES MAPPING TO HAVE THE profiles_sums ARRAY AS "type": "nested" !!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
BODY = '{
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
            "size": 10,
            "field": "profiles_sums.profile"
          },
          "aggs": {
            "failures": {
              "sum": {
                "field": "profiles_sums.controls.failed.total"
              }
            },
            "passed": {
              "sum": {
                "field": "profiles_sums.controls.passed.total"
              }
            },
            "skipped": {
              "sum": {
                "field": "profiles_sums.controls.skipped.total"
              }
            },
            "minors": {
              "sum": {
                "field": "profiles_sums.controls.failed.minor"
              }
            },
            "majors": {
              "sum": {
                "field": "profiles_sums.controls.failed.major"
              }
            },
            "criticals": {
              "sum": {
                "field": "profiles_sums.controls.failed.critical"
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

request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took,aggregations.profiles.totals.buckets.key,aggregations.profiles.totals.buckets")
Scans.add_header(request, dest['target_opts']['header'])

request.body = BODY.gsub('ARRAY_HERE',report_ids.to_s)
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Profiles list aggregation response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"

pp elastic_out
