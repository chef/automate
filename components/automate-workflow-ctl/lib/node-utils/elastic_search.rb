require "elasticsearch"
require "date"
require "chef/mixin/deep_merge"

require "node-utils/chef_server"
require "node-utils/config"

module NodeUtils
  module ElasticSearch
    def add_chef_server_status!(node_json)
      # ChefServer.nodes is nil, this means we were not able to get information
      # from Chef Server therefore we should not add any status.
      return if ChefServer.nodes.nil?

      node_found = ChefServer.nodes.find do |n|
        if n["name"] == node_json["name"]
          # We need to check the org name too once we have a node name match.
          org_info = ChefServer.orgs.find { |o| o["id"] == n["org_id"] }

          !org_info.nil? && org_info["name"] == node_json["organization_name"]
        else
          false
        end
      end

      node_json["chef_server_status"] = node_found ? "present" : "missing"

      node_json
    end
    module_function :add_chef_server_status!

    def update_status_for_billing!(node_json, threshold_time)
      # If a node is running the liveness agent, the agent sets the timestamp of the last
      # successful ping or converge in node_ping.
      if node_json["status"] == "missing" && node_json["node_ping"]
        node_ping = DateTime.parse(node_json["node_ping"]).to_time
        if node_ping > threshold_time
          node_json["status"] = "live"
        end
      # If a node is running agentless scan, it does not run the liveness agent, and we
      # do not have node_ping. We instead compare the end time of the last scan to the
      # threshold time.
      elsif node_json["status"] == "scan-passed" || node_json["status"] == "scan-failed"
        checkin_time = DateTime.parse(node_json["checkin"]).to_time
        if checkin_time < threshold_time
          # For billing purposes, a node that has not been scanned within the threshold time
          # will be marked "scan-unreachable".
          # Alternatives are "scan-passed", "scan-failed", "scan-skipped".
          node_json["status"] = "scan-unreachable"
        end
      end
    end
    module_function :update_status_for_billing!

    def client(options = {})
      es_options = Chef::Mixin::DeepMerge.deep_merge(
        options,
        {
          host: "http://localhost:8080/elasticsearch",
          retry_on_failure: 3,
          reload_on_failure: true,
          transport_options: {
            ssl: {
              verify: false,
            },
            request: {
              timeout: 300,
            },
          },
        }
      )

      Elasticsearch::Client.new(es_options)
    end
    module_function :client

    def nodes(client_options: {})

      # node billing:
      # the `success`, `failure`, `missing` status of a node is used for the dashboard
      # feature of Automate (viz), and only corresponds to chef-client runs (or lack thereof).
      # this field cannot be reliably used for billing. For billing, we must also compare
      # the `node_ping` time to the node_missing_threshold_mins value in the config to
      # determine whether a liveness agent has pinged outside of a chef-client run.
      current_time = Time.now
      threshold_time = current_time - (Config.missing_threshold_mins * 60)

      search_body = {
        query: {
          bool: {
            must: { term: { exists: "true" } },
          },
        },
        _source: %w{
          name fqdn checkin node_ping status chef_version
          platform platform_version platform_family
          organization_name @timestamp ec2.*
        },
      }

      hits = search_with_scroll("node-state", search_body, client_options)

      hits.map do |hit|
        node_json = hit["_source"]
        node_json["uuid"] = hit["_id"]
        add_chef_server_status!(node_json)
        update_status_for_billing!(node_json, threshold_time)
        node_json
      end
    end
    module_function :nodes

    def agentless_nodes(client_options: {})

      # node billing for agentless scan nodes:
      # The liveness agent is installed via the chef-server and is not present on agentless
      # scan nodes. For billing, we check if the `node_checkin` time falls within the past hour
      # and if the node was successfully scanned.
      # If so, we mark the node as "live" and hence billable for the hour.
      current_time = Time.now
      threshold_time = current_time - 3600

      search_body = {
        query: {
          bool: {
            must: { exists: { field: "job_uuid" } },
          },
        },
        _source: %w{
          node_name node_uuid end_time status
          @timestamp
        },
      }

      hits = search_with_scroll("compliance-latest", search_body, client_options, "inspec_summary")

      hits.map do |hit|
        node_json = hit["_source"]
        node_json["name"] = hit["_source"]["node_name"]
        node_json["checkin"] = hit["_source"]["end_time"]
        node_json["status"] = "scan-" + hit["_source"]["status"]
        node_json["uuid"] = hit["_id"]
        update_status_for_billing!(node_json, threshold_time)
        node_json
      end
    end
    module_function :agentless_nodes

    def search_with_scroll(index, search_body, client_options, type="")
      # Since this command can only run on the Automate node we can use nginx
      # reverse proxy endpoint for elasticsearch

      hits = []
      # We use #scroll api of elasticsearch so that we have a consistent list
      # of results when we paginate.
      #
      # This is the initial search. We retain the search scrollability for 5
      # minutes and we set our batch size to 100.
      search = NodeUtils::ElasticSearch.client(client_options).search(
        index: "#{index}",
        type: "#{type}",
        scroll: "5m",
        size: "100",
        body: search_body
      )

      # Add initial search hits to our total
      hits += search["hits"]["hits"]

      loop do
        search = NodeUtils::ElasticSearch.client.scroll(
          body: {
            scroll_id: search["_scroll_id"],
            scroll: "5m"
          }
        )

        # if we received a scroll return but with empty results, we're done.
        break if search["hits"]["hits"].empty?

        hits += search["hits"]["hits"]
      end
      hits
    end
    module_function :search_with_scroll
  end
end
