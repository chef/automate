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
#!!!!!!!!!!!! REQUIRES MAPPING TO HAVE THE profiles_min ARRAY AS "type": "nested" !!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

target_profile = 'linux-baseline'
target_profile_hash = 'b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015'

# document type is filtered in the URL (... #{es_index}/inspec_report/_search)
BODY = '{
  "size": 0,
  "query": {
    "nested": {
      "path": "profiles_min",
      "query": {
        "bool": {
          "must": [{
            "ids": {
              "type": "inspec_report",
              "values": ARRAY_HERE
            }
          },
            {
              "term": {
                "profiles_min.sha256": "PROFILE_HERE"
              }
            }
          ]
        }
      }
    }
  },
  "aggs": {
    "profiles": {
      "nested": {
        "path": "profiles_min"
      },
      "aggs": {
        "profiles_filter": {
          "filter": {
            "term": {
              "profiles_min.sha256": "PROFILE_HERE"
            }
          },
          "aggs": {
            "controls": {
              "nested": {
                "path": "profiles_min.controls"
              },
              "aggs": {
                "totals": {
                  "terms": {
                    "field": "profiles_min.controls.id",
                    "size": 10
                  },
                  "aggs": {
                    "passed": {
                      "filter": {
                        "term": {
                          "profiles_min.controls.status": "passed"
                        }
                      }
                    },
                    "failed": {
                      "filter": {
                        "term": {
                          "profiles_min.controls.status": "failed"
                        }
                      }
                    },
                    "skipped": {
                      "filter": {
                        "term": {
                          "profiles_min.controls.status": "skipped"
                        }
                      }
                    }
                  }
                },
                "passed": {
                  "filter": {
                    "term": {
                      "profiles_min.controls.status": "passed"
                    }
                  }
                },
                "failed": {
                  "filter": {
                    "term": {
                      "profiles_min.controls.status": "failed"
                    }
                  }
                },
                "skipped": {
                  "filter": {
                    "term": {
                      "profiles_min.controls.status": "skipped"
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}'

# {
#   "size": 0,
#   "query" : {
# 	    "nested" : {
# 	        "path" : "profiles_min",
# 	        "query" : {
# 	            "bool": {
# 			      "must": [
# 			        {
# 			          "terms": {
# 			            "_id": ["94266298-0c5c-49db-bc86-5042e956c833"]
# 			          }
# 			        },
# 			        {
# 			          "term": {
# 			            "profiles_min.name": "linux-baseline"
# 			          }
# 			        }
# 			      ]
# 			    }
#     		},
#     		"inner_hits" : {}
#         }
#     },
#   "aggs" : {
#     "profiles" : {
#       "nested" : {
#         "path" : "profiles_min.controls"
#       },
#       "aggs": {
#             "totals": {
#               "terms": {
#                 "field": "profiles_min.controls.id",
#                 "size": 10000
#               },
#               "aggs":{
#                       "passed": {
#                         "filter":{
#                           "term": {
#                             "profiles_min.controls.status" : "passed"
#                           }
#                         }
#                       },
#                       "failed": {
#                         "filter":{
#                           "term": {
#                             "profiles_min.controls.status" : "failed"
#                           }
#                         }
#                       },
#                       "skipped": {
#                         "filter":{
#                           "term": {
#                             "profiles_min.controls.status" : "skipped"
#                           }
#                         }
#                       }
#                     }
#
#             }
#       }
#     }
#   }
# }

es_index = ARGV.first
fail "ERROR: you must provide the index to query as argument!" unless es_index

dest = YAML.load_file(File.join(__dir__, '..', 'reports_dest.yml'))
report_ids = Scans.report_ids(dest, es_index)

@http = Scans.prep_http(dest)
path = dest['target_opts']['path']
request = Net::HTTP::Post.new("#{path}/#{es_index}/inspec_report/_search?filter_path=took,aggregations")
Scans.add_header(request, dest['target_opts']['header'])
my_body = BODY.gsub('ARRAY_HERE',report_ids.to_s).gsub('PROFILE_HERE',target_profile_hash)
#puts my_body

request.body = my_body
response = @http.request(request)
fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
elastic_controls = JSON.parse(response.body)
puts "*** Control list within a profile response #{response.code} #{response.message} in "+(elastic_controls['took']/1000.0).to_s.yellow+" seconds"
pp elastic_controls


# get more details from inspec_report
request = Net::HTTP::Get.new("#{path}/compliance-profiles/inspec_profile/#{target_profile_hash}")
Scans.add_header(request, dest['target_opts']['header'])
response = @http.request(request)
elastic_profile = JSON.parse(response.body)
#pp elastic_profile
extra_controls = elastic_controls['aggregations']['profiles']['profiles_filter']['controls']['totals']['buckets'].map { |control|
  pcontrol = elastic_profile['_source']['controls'].find{ |c| c['id'] == control['key']}
  if pcontrol
    control['impact'] = pcontrol['impact']
    control['title'] = pcontrol['title']
  end
  control
}
pp extra_controls
puts "---------------------------------------------
Total failed: #{elastic_controls['aggregations']['profiles']['profiles_filter']['controls']['failed']['doc_count']}
Total passed: #{elastic_controls['aggregations']['profiles']['profiles_filter']['controls']['passed']['doc_count']}
Total skipped: #{elastic_controls['aggregations']['profiles']['profiles_filter']['controls']['skipped']['doc_count']}
Version: #{elastic_profile['_source']['version']}
Profile: #{elastic_profile['_source']['title']}
License: #{elastic_profile['_source']['license']}
Supports: #{elastic_profile['_source']['supports']}
"
