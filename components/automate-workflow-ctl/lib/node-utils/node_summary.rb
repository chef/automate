require "json"
require "node-utils/shell_out"
require "node-utils/elastic_search"
require "highline/import"

module NodeUtils
  # Node summary is intended to be used for billing purposes so we take care to
  # return the most accurate information we can. Here we force run the cron job
  # that marks the nodes as missing if they did not check in based on a
  # configurable time threshold.
  module NodeSummary

    NAME = "name".freeze
    FQDN = "fqdn".freeze

    def summary(format: "text", request_timeout: 300)
      NodeUtils::ShellOut.shell_out("/opt/delivery/embedded/insights/batch_jobs/cron.hourly/insights_mark_nodes_missing")

      node_info = fetch_node_info(request_timeout)
      if format == "json"
        JSON.pretty_generate(node_info)
      elsif node_info.empty?
        "Chef Automate does not have any registered nodes."
      else # text formatter
        HighLine.use_color = $stdout.tty?
        highline = HighLine.new

        output = [
          highline.color("NAME", :bold),
          highline.color("UUID", :bold),
          highline.color("STATUS", :bold),
          highline.color("LAST CHECKIN", :bold),
        ]

        node_info.each do |i|
          output << i["name"] || "N/A"
          output << i["uuid"] || "N/A"
          output << i["status"] || "N/A"
          output << i["checkin"] || "N/A"
        end

        highline.list(output, :uneven_columns_across, 4)
      end
    end
    module_function :summary

    def fetch_node_info(request_timeout)
      viz_info = fetch_viz_node_info(request_timeout)
      agentless_info = fetch_agentless_node_info(request_timeout)
      viz_fqdns = viz_info.inject({}) do |fqdns, node_info|
        fqdns[node_info[FQDN]] = 1
        fqdns
      end
      agentless_info.reject! do |node_info|
        viz_fqdns.keys.any? { |key| node_info[NAME].include?(key) }
      end
      viz_info + agentless_info
    end

    module_function :fetch_node_info

    def fetch_viz_node_info(request_timeout)
      NodeUtils::ElasticSearch.nodes(
        client_options: { request_timeout: request_timeout }
      )
    end

    module_function :fetch_viz_node_info

    def fetch_agentless_node_info(request_timeout)
      NodeUtils::ElasticSearch.agentless_nodes( client_options: { request_timeout: request_timeout } )
    end

    module_function :fetch_agentless_node_info
  end
end
