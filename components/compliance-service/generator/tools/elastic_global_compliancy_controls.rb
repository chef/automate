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

BODY = '{
  "size": 0,
  "query": {
    "ids" : {
      "type" : "inspec_summary",
      "values" : ARRAY_HERE
    }
  },
  "aggs": {
    "failed_total": {
      "sum": {
        "field": "controls.failed.total"
      }
    },
    "skipped_total": {
      "sum": {
        "field": "controls.skipped.total"
      }
    },
    "passed_total": {
      "sum": {
        "field": "controls.passed.total"
      }
    },
    "failed_minor": {
      "sum": {
        "field": "controls.failed.minor"
      }
    },
    "failed_major": {
      "sum": {
        "field": "controls.failed.major"
      }
    },
    "failed_critical": {
      "sum": {
        "field": "controls.failed.critical"
      }
    }
  }
}'

# This version does not require the summary enriched data
# Failed control count is minor+major+critical+undefined
# BODY = '{
#   "size": 0,
#   "query": {
#     "terms": {
#       "_id": ARRAY_HERE
#     }
#   },
#   "aggs" : {
#     "profiles" : {
#       "nested" : {
#         "path" : "profiles.controls"
#       },
#       "aggs": {
#         "minor": {
#           "filter": {
#             "bool": {
#               "must": [
#                 {
#                   "range": {
#                     "profiles.controls.impact": {
#                       "lt": 0.4
#                     }
#                   }
#                 },
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "failed"
#                   }
#                 }
#               ]
#             }
#           }
#         },
#         "major": {
#           "filter": {
#             "bool": {
#               "must": [
#                 {
#                   "range": {
#                     "profiles.controls.impact": {
#                       "gte": 0.4,
#                       "lt": 0.7
#                     }
#                   }
#                 },
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "failed"
#                   }
#                 }
#               ]
#             }
#           }
#         },
#         "critical": {
#           "filter": {
#             "bool": {
#               "must": [
#                 {
#                   "range": {
#                     "profiles.controls.impact": {
#                       "gte": 0.7
#                     }
#                   }
#                 },
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "failed"
#                   }
#                 }
#               ]
#             }
#           }
#         },
#         "undefined": {
#           "filter": {
#             "bool": {
#               "must": [
#                 {
#                   "missing" : {"field": "profiles.controls.impact"}
#                 },
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "failed"
#                   }
#                 }
#               ]
#             }
#           }
#         },
#         "skipped_controls": {
#           "filter": {
#             "bool": {
#               "must": [
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "skipped"
#                   }
#                 }
#               ],
#               "must_not": [
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "failed"
#                   }
#                 }
#               ]
#             }
#           }
#         },
#         "passed_controls": {
#           "filter": {
#             "bool": {
#               "must": [
#                 {
#                   "term": {
#                     "profiles.controls.results.status": "passed"
#                   }
#                 }
#               ],
#               "must_not": [
#                 {
#                   "terms": {
#                     "profiles.controls.results.status": ["failed", "skipped"]
#                   }
#                 }
#               ]
#             }
#           }
#         }
#       }
#     }
#   }
# }'



es_index = ARGV.first
fail "ERROR: you must provide the index to query as argument!" unless es_index

dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))
report_ids = Scans.report_ids(dest, es_index)

@http = Scans.prep_http(dest)
path = dest['target_opts']['path']
request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took,aggregations")
Scans.add_header(request, dest['target_opts']['header'])

request.body = BODY.gsub('ARRAY_HERE',report_ids.to_s)
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_out = JSON.parse(response.body)
puts "*** Global compliance controls response #{response.code} #{response.message} in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"

pp elastic_out
