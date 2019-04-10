# encoding: utf-8
# author: Alex Pop

require 'net/http'
require 'openssl'
require 'yaml'
require 'json'
require 'colorize'

class Scans
  BODY = '{
    "query": {
      "bool": {
        "must": [
          {
            "type": {
              "value": "inspec_summary"
            }
          },
          {
            "range": {
              "end_time": {
                TIME_RANGE_HERE
              }
            }
          }
        ]
      }
    },
    "aggs" : {
      "distinct_nodes" : {
        "terms" : {
          "field" : "node_uuid",
          "size": 10000
        },
        "aggs": {
          "only_one_post": {
            "top_hits": {
              "size": 1,
              "sort":{"end_time":{"order":"desc"}},
              "_source": false
            }
          }
        }
      }
    },
    "size": 0
  }'

# To add environment filter in query
# {
#   "term": { "environment": "DevSec Prod Alpha" }
# }

  def self.report_ids(dest, es_index, start_time=nil, end_time='2021-03-30')
    fail "dest missing target_opts->host" unless dest['target_opts'] && dest['target_opts']['host']

    @http = prep_http(dest)
    path = dest['target_opts']['path']

    request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=took,hits.total,aggregations.distinct_nodes.buckets.key,aggregations.distinct_nodes.buckets.only_one_post.hits.hits._id")
    add_header(request, dest['target_opts']['header'])

    time_range = '"lte": "'+end_time+'"'
    time_range = time_range + ', "gt": "'+start_time+'"' if start_time
    request.body = BODY.gsub('TIME_RANGE_HERE',time_range)

    # puts BODY.gsub('TIME_RANGE_HERE',time_range)

    response = @http.request(request)
    fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
    elastic_out = JSON.parse(response.body)
    if elastic_out['aggregations'].nil?
      puts "*** Scans.report_ids response #{response.code} #{response.message}, returned "+'0'.red+" reports in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"
      return []
    end
    report_ids = elastic_out['aggregations']['distinct_nodes']['buckets'].map { |bucket|
      bucket['only_one_post']['hits']['hits'].first['_id']
    }
    puts "*** Scans.report_ids response #{response.code} #{response.message}, returned "+report_ids.length.to_s.green+" reports in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"
    report_ids
  end

  # adds all the header to the request
  def self.add_header(request, header)
    unless header.nil?
      header.each { | key, value |
        request.add_field(key, value)
      }
    end
  end

  def self.prep_http(dest)
    http = Net::HTTP.new(
      dest['target_opts']['host'],
      dest['target_opts']['port']
    )

    if dest['target_opts']['ssl']
      http.use_ssl = true
      http.verify_mode = OpenSSL::SSL::VERIFY_NONE
    end
    http
  end



  # BODY_BUCKETS = '{
  #   "size": 0,
  #   "query": {
  #     "bool": {
  #       "must": [
  #         {
  #           "type": { "value": "inspec_summary" }
  #         },
  #         {
  #           "range": { "end_time": { "gte": "now-DAYS_BACK_HEREd" } }
  #         }
  #       ]
  #     }
  #   },
  #   "aggs" : {
  #     "over_time" : {
  #       "date_histogram" : {
  #         "field" : "end_time",
  #         "interval" : "DAYS_INTERVAL_HEREd",
  #         "format" : "yyyy-MM-dd-HH-mm-ss",
  #         "min_doc_count": 0,
  #         "extended_bounds": {
  #           "min": MIN_EPOCH_HERE,
  #           "max": MAX_EPOCH_HERE
  #         }
  #       },
  #       "aggs" : {
  #         "distinct_nodes" : {
  #           "terms" : {
  #             "field" : "node_uuid",
  #             "size": 10000
  #           },
  #           "aggs": {
  #             "only_one_post": {
  #               "top_hits": {
  #                 "size": 1,
  #                 "sort":{"end_time":{"order":"desc"}},
  #                 "_source": false
  #               }
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  # }'
  # To add environment filter in query
  # {
  #   "term": { "environment": "DevSec Prod Alpha" }
  # }



  BODY_BUCKETS = '{
      "size": 0,
      "query": {
        "bool": {
          "must": [
            {
              "type": { "value": "inspec_summary" }
            }
          ]
        }
      },
      "aggs" : {
        "range": {
           "date_range": {
              "field": "end_time",
              "format": "yyyy-MM-dd-HH-mm-ss",
              "ranges": [
                { "to": "2017-04-20-00-00-00" },
                { "to": "2017-04-20-00-00-00||-864000s" },
                { "to": "2017-04-20-00-00-00||-1728000s" },
                { "to": "2017-04-20-00-00-00||-2592000s" },
                { "to": "2017-04-20-00-00-00||-3456000s" },
                { "to": "2017-04-20-00-00-00||-4320000s" }
              ]
           },
           "aggs" : {
             "distinct_nodes" : {
                "terms" : {
                  "field" : "node_uuid",
                  "size": 10000
                },
                "aggs": {
                  "only_one_post": {
                    "top_hits": {
                      "size": 1,
                      "sort":{"end_time":{"order":"desc"}},
                      "_source": false
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



  def self.report_ids_buckets(dest, es_index, start_time, end_time, interval_sec)
    fail "dest missing target_opts->host" unless dest['target_opts'] && dest['target_opts']['host']

    @http = prep_http(dest)
    path = dest['target_opts']['path']

    request = Net::HTTP::Post.new("#{path}/#{es_index}/_search?filter_path=aggregations.range.buckets.key,aggregations.range.buckets.distinct_nodes.buckets.only_one_post.hits.hits._id")
    add_header(request, dest['target_opts']['header'])
    mybody = BODY_BUCKETS.gsub('DAYS_BACK_HERE',days_back.to_s).gsub('DAYS_INTERVAL_HERE',interval_days.to_s)
    # without these bounds, ElasticSearch will return buckets only between periods where documents have been found
    mybody = mybody.gsub('MAX_EPOCH_HERE',(Time.now.to_i*1000).to_s).gsub('MIN_EPOCH_HERE',((Time.now.to_i - days_back*24*60*60)*1000).to_s)
    request.body = mybody
    response = @http.request(request)
    fail "Elastic response #{response.code} #{response.message}" unless response.code == '200'
    elastic_out = JSON.parse(response.body)
    if elastic_out['aggregations'].nil?
      puts "*** Scans.report_ids response #{response.code} #{response.message}, returned "+'0'.red+" reports in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"
      return []
    end

    print request.body
    # see the response for the histogram aggregation search
    pp elastic_out

    report_ids_count = 0
    report_ids_hash = {}
    elastic_out['aggregations']['over_time']['buckets'].each { |bucket1|
      if bucket1["distinct_nodes"].nil?
        report_ids = ["no-report-ids-found-for-this-date-window"]
      else
        report_ids = bucket1["distinct_nodes"]["buckets"].map { |bucket2|
          report_ids_count+=1
          bucket2['only_one_post']['hits']['hits'].first['_id']
        }
      end
      report_ids_hash[bucket1["key_as_string"]] = report_ids
    }

    puts "*** Scans.report_ids response #{response.code} #{response.message}, returned "+report_ids_count.to_s.green+" reports across "+report_ids_hash.length.to_s.green+" ranges in "+(elastic_out['took']/1000.0).to_s.yellow+" seconds"
    report_ids_hash
  end
end
