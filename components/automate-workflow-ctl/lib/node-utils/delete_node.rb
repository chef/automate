require "ctl-helpers/exceptions"
require "ctl-helpers/prompt"

require "node-utils/elastic_search"

require "highline/import"

module NodeUtils
  class DeleteNode
    class << self
      def delete(config = {})
        NodeUtils::DeleteNode.new(config).delete
      end
    end

    attr_reader :uuid
    attr_reader :name
    attr_reader :org
    attr_reader :chef_server_fqdn
    attr_reader :es_client
    attr_reader :highline
    attr_reader :node
    attr_reader :batch_size

    def initialize(
                   uuid: nil,
                   name: nil,
                   org: nil,
                   chef_server_fqdn: nil,
                   node_data: true,
                   compliance_data: false,
                   force: false,
                   purge: false,
                   batch_size: 1000,
                   request_timeout: 300 # seconds
                  )

      @uuid             = uuid
      @name             = name
      @org              = org
      @chef_server_fqdn = chef_server_fqdn
      @node_data        = node_data
      @compliance_data  = compliance_data
      @force            = force
      @purge            = purge
      @batch_size       = batch_size
      @es_client        = NodeUtils::ElasticSearch.client({
        transport_options: { request: { timeout: request_timeout } },
      })
      HighLine.use_color = $stdout.tty?
      @highline = HighLine.new
      @node = nil
    end

    #
    # Find and delete all Elasticsearch documents that are associated with the
    # configured node.
    #
    # 1) Determine the node UUID. If we've got it verify that it exists or exit.
    #    If we don't have it, use any combination of the name, org, or
    #    Chef server FQDN to find matches. If we've got more than one match
    #    error out and display the UUIDs so that the user can try again.
    # 2) Prompt the user for permission to delete the matching records.
    # 3) Delete matching documents.
    # 4) Replace the node-state document with a shim if we're configured to do so.
    # 5) Purge the node-state document entirely if we're configured to do so.
    #
    def delete
      determine_node_uuid!

      requests = []
      requests += delete_insights_bulk_requests if delete_node_data?
      requests += delete_compliance_bulk_requests if delete_compliance_data?

      requests <<
        if purge_node?
          purge_node_from_node_state_bulk_request
        elsif delete_node_data?
          create_shim_node_in_node_state_bulk_request
        end

      msg = "Delete #{[(requests.count - 1), 0].max} records associated with node " \
            "'#{node["_source"]["name"]} #{node["_id"]}'."
      if purge_node?
        msg << "\nPurging '#{node["_source"]["name"]} #{node["_id"]}' from the node " \
               "state database may cause unforseen issues."
      end
      msg << "\nDo you wish to proceed? (yes/no)"

      return unless force? || CtlHelpers::Prompt.yes_no_prompt(msg)

      requests.compact!
      requests.each_slice(batch_size.to_i) { |r| es_client.bulk(body: r) }
    end

    def force?
      @force
    end

    def delete_node_data?
      @node_data
    end

    def delete_compliance_data?
      @compliance_data
    end

    def purge_node?
      @purge
    end

    #
    # Find or set the node to use for the delete operation
    #
    def determine_node_uuid!
      matches =
        if uuid
          begin
            [es_client.get(index: "node-state", id: uuid)]
          rescue Elasticsearch::Transport::Transport::Errors::NotFound
            []
          end
        else
          requirements = [{ term: { "name.raw" => name } }]
          requirements << { term: { "organization_name.raw" => org } } if org
          requirements << { term: { "source_fqdn.raw" => chef_server_fqdn } } if chef_server_fqdn

          scroll_search("node-state", "node-state", requirements)
        end

      if matches.count == 1
        @node = matches.first
        @uuid = node["_id"]
        @name = node["_source"]["name"]
        @org = node["_source"]["organization_name"]
        @chef_server_fqdn = node["_source"]["source_fqdn"]
      elsif matches.empty?
        raise "Could not find a matching node."
      else
        highline.say(
          "Multiple nodes were found matching your request. Please specify the " \
          "UUID and try again: automate-ctl delete-node --uuid <UUID> \n\n"
        )

        highline.say(format_node_list(matches))

        raise "Too many nodes found, please delete by node UUID"
      end
    end

    def format_node_list(nodes)
      output = [
        highline.color("NAME", :bold),
        highline.color("ORG", :bold),
        highline.color("CHEF SERVER FQDN", :bold),
        highline.color("UUID", :bold),
      ]

      nodes.each do |n|
        output << n["_source"]["name"] || ""
        output << n["_source"]["organization_name"] || ""
        output << n["_source"]["source_fqdn"] || ""
        output << n["_id"] || ""
      end

      highline.list(output, :uneven_columns_across, 4)
    end

    #
    # When given an index (or indices), type(s), and filter requirements, return
    # an array of  hits
    #
    # @param index String
    #   The index or indices to query, eg: "node-state" or "insights-*"
    #
    # @param type String
    #   A comma separated list of types to select
    #
    # @param requirements [Hash,Array]
    #   A Hash or Array containing boolean filters upon which to match
    #
    # @param source Boolean
    #   Whether or not to include the _source field in the response
    #
    # @return Array<Hash>
    #   An array of bulk formatted delete/data pair requests for matching
    #   documents
    #
    def scroll_search(index, type, requirements, source = true)
      requests = []

      search = es_client.search(
        index: index,
        type: type,
        scroll: "5m",
        size: "100",
        _source: source,
        body: {
          query: {
            bool: {
              must: requirements,
            },
          },
        }
      )

      # Add initial search hits to our total
      requests += search["hits"]["hits"]

      loop do
        # note: the typical usage of the ruby client scroll API is to pass
        # `:scroll_id` along as a parameter to the `#scroll` method. the
        # result of this is that the client will make a GET request with the
        # scroll_id in the URL parameters. unfortunately, the scroll id grows
        # linearly with the nubmer of shards in your infrastructure to a
        # point where it is too large to pass as part of the URL for large
        # automate installs.
        #
        # passing the scroll_id and scroll along in the
        # body of the request gets around this because elasticsearch
        # honors the body of both GET and POST requests
        #
        # this is necessary for this request (and potentially not for others)
        # because we're using wildcard indices like `insights-*` making
        # the possible number of shards unbounded
        #
        # docs: https://www.elastic.co/guide/en/elasticsearch/reference/current/search-request-scroll.html
        search = es_client.scroll(
          body: {
            scroll_id: search["_scroll_id"],
            scroll: "5m"
          }
        )

        break if search["hits"]["hits"].empty?

        requests += search["hits"]["hits"]
      end

      requests
    end

    #
    # When given an index (or indices), type(s), and filter requirements, return
    # an array of bulk delete requests for matching documents
    #
    # @param index String
    #   The index or indices to query
    #
    # @param type String
    #   A comma separated list of types to select
    #
    # @requirements [Hash,Array]
    #   A Hash or Array containing boolean filters upon which to match
    #
    # @return Array<Hash>
    #   An array of query hits
    #
    def bulk_delete_requests(index, type, requirements)
      scroll_search(index, type, requirements, false).map do |doc|
        {
          delete: {
            "_index" => doc["_index"],
            "_id" => doc["_id"],
            "_type" => doc["_type"],
          },
        }
      end
    end

    def delete_insights_bulk_requests
      bulk_delete_requests(
        "insights-*",
        "converge,node_ping",
        { term: { "entity_uuid" => uuid } }
      )
    end

    def delete_compliance_bulk_requests
      bulk_delete_requests(
        "compliance-20*",
        "inspec_report,inspec_summary",
        { term: { "node_uuid" => uuid } }
      )
    end

    def create_shim_node_in_node_state_bulk_request
      return nil unless node

      {
        index: {
          _index: node["_index"],
          _type: node["_type"],
          _id: uuid,
          data: {
            entity_uuid: uuid,
            exists: "false",
            name: node["_source"]["name"],
            organization_name: node["_source"]["organization_name"],
            :"@timestamp" => node["_source"]["@timestamp"],
          },
        },
      }
    end

    def purge_node_from_node_state_bulk_request
      return nil unless node

      {
        delete: {
          _index: node["_index"],
          _type: node["_type"],
          _id: uuid,
        },
      }
    end
  end
end
