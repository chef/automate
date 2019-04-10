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

# aggregation model, looses the precious score for relevance
# BODY = '{
#   "size": 0,
#   "query": {
#     "bool": {
#       "must": [
#         {
#           "type": {
#             "value": "inspec_summary"
#           }
#         },
#         {
#           "match" : {
#             "platform.name.engram" : "TEXT_HERE"
#           }
#         }
#       ]
#     }
#   },
#   "aggs": {
#     "platforms": {
#       "terms": {
#         "field": "platform.name",
#         "size": 10
#       }
#     }
#   }
# }'
BODY = '{
  "size": 0,
  "query": {
    "bool": {
      "must": [
        {
          "type": {
            "value": "inspec_summary"
          }
        },
        {
          "match" : {
            "platform.name.engram" : "TEXT_HERE"
          }
        }
      ]
    }
  },
  "aggs": {
    "platforms": {
      "terms": {
        "field": "platform.name",
        "size": 100
      },
      "aggs": {
        "distinct": {
          "top_hits": {
            "size": 1,
            "_source": false
          }
        }
      }
    }
  }
}'

# autocomplete solution, works only when matching at the beginning
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!! REQUIRES MAPPING TO HAVE THE SUGGESTIONS WORK !!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# POST _suggest
# BODY = '{
# 	"platform_autocompletion" : {
# 	  "text" : "c",
# 	  "completion" : {
# 	    "size": 20,
#         "field" : "platform_suggest"
# 	  }
# 	}
# }'

es_index = ARGV.first
text = ARGV[1]
fail "ERROR: you must provide the index to query as argument!" unless es_index
fail "ERROR: you must provide the text to query as argument!" unless text

dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))

@http = Scans.prep_http(dest)
path = dest['target_opts']['path']

request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took,aggregations.platforms.buckets.key,aggregations.platforms.buckets.distinct.hits.hits._score")
Scans.add_header(request, dest['target_opts']['header'])
my_body = BODY.gsub('TEXT_HERE',text)
#puts my_body
request.body = my_body
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Platforms suggestions response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"

pp elastic_out
