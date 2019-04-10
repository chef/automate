require "ctl-helpers/exceptions"
require "ctl-helpers/prompt"

require "highline/import"

require "net/http"
require "json"

module NodeUtils
  class DataSummary
    class << self
      def summary(config = {})
        NodeUtils::DataSummary.new(config).summary
      end
    end

    BYTE = 1
    KB = BYTE * 1024
    MB = KB * 1024
    GB = MB * 1024

    attr_reader :compliance
    attr_reader :insights
    attr_reader :node_state
    attr_reader :cluster
    attr_reader :format
    attr_reader :highline
    attr_reader :unit_measure

    def initialize(
                   compliance: false,
                   cluster: false,
                   format: "text",
                   insights: false,
                   node_state: false,
                   unit_measure: nil
                  )

      @compliance   = compliance
      @cluster      = cluster
      @format       = format
      @insights     = insights
      @node_state   = node_state
      @unit_measure = unit_measure

      HighLine.use_color = $stdout.tty?
      @highline = HighLine.new
    end

    # Aggregate data from the statistics API, filter for any unnecessary data
    # format the data and return the result.
    def summary
      data = {}
      errors = {}

      if cluster
        data["cluster"] = cluster_data
        errors.merge!(data["cluster"].select { |k, v| k == "error" })
      end

      if insights || compliance || node_state
        data["indices"] = indices_data
        errors.merge!(data["indices"].select { |k, v| k == "error" })
      end

      raise "unable to retrieve elasticsearch data: #{errors["error"]}" unless errors.empty?

      if format == "text"
        format_text(data)
      elsif format == "json"
        format_json(data)
      end
    end

    def cluster_data
      # NOTE: This is hardcoded for now but we'll want to change the URL to be
      # that of the nginx proxy (when it is added)
      cluster_uri = URI("http://localhost:7676/elasticsearch/cluster_data")
      JSON.parse(Net::HTTP.get_response(cluster_uri).body)
    end

    def indices_data
      # NOTE: This is hardcoded for now but we'll want to change the URL to be
      # that of the nginx proxy (when it is added)
      indices_uri = URI("http://localhost:7676/elasticsearch/indices_data")
      JSON.parse(Net::HTTP.get_response(indices_uri).body)
    end

    def format_text(data)
      output = []

      output << format_cluster_data(data["cluster"]) if cluster
      output << format_node_data(data["indices"]) if node_state
      output << format_insights_data(data["indices"]) if insights
      output << format_compliance_data(data["indices"]) if compliance

      output.compact.join("\n")
    end

    def format_json(data)
      if cluster
        data["cluster"] = format_cluster_data_json(data["cluster"])
      else
        data.delete("cluster") if data.key?("cluster")
      end

      if insights
        data["indices"]["insights"] = format_indices_data_json(data["indices"]["insights"])
      end

      if compliance
        data["indices"]["compliance"] = format_indices_data_json(data["indices"]["compliance"])
      end

      if node_state
        data["indices"]["node_state"] = format_indices_data_json(data["indices"]["node_state"])
      end

      %w{insights compliance node_state}.each do |idx_group|
        data["indices"].delete(idx_group) if data.key?("indices") && !send(idx_group)
      end

      data.to_json
    end

    def format_cluster_data_json(data)
      data["averages"] = transform_to_unit(data["averages"])

      data["nodes"].map! do |node|
        node["totals"] = transform_to_unit(node["totals"])
      end

      data
    end

    def format_indices_data_json(data)
      # map all values to the right unit of measurement
      %w{totals averages}.each do |g|
        if data.key?(g) && data[g]
          data[g] = transform_to_unit(data[g])
        end
      end

      if data.key?("indices") && data["indices"]
        data["indices"].map! do |idx|
          idx["totals"] = transform_to_unit(idx["totals"])
        end
      end

      data
    end

    def format_node_data(data)
      ns = data["node_state"]["totals"]

      return nil unless ns

      highline.list(
        [
          highline.color("INDEX NAME", :bold),
          highline.color("DELETED NODES", :bold),
          highline.color("TOTAL NODES", :bold),
          highline.color("TOTAL SIZE", :bold),
          "node-state",
          ns["deleted_nodes"],
          ns["nodes"],
          format_unit(ns["size_in_bytes"]),
        ].map(&:to_s),
        :uneven_columns_across,
        4
      )
    end

    def format_insights_data(data)
      return nil unless data["insights"]["totals"]

      avg = data["insights"]["averages"]
      tot = data["insights"]["totals"]
      indices = data["insights"]["indices"]

      avg_tot_out = [
        highline.color("INDICES GROUP", :bold),
        highline.color("INDICES TOTAL", :bold),
        highline.color("TOTAL CONVERGES", :bold),
        highline.color("AVG DAILY CONVERGE", :bold),
        highline.color("TOTAL SIZE", :bold),
        highline.color("AVG DAILY SIZE", :bold),
        "insights",
        tot["indices"],
        tot["converges"],
        avg["converges"],
        format_unit(tot["size_in_bytes"]),
        format_unit(avg["size_in_bytes"]),
      ]

      idx_out = [
        highline.color("INDEX NAME", :bold),
        highline.color("TOTAL CONVERGES", :bold),
        highline.color("TOTAL SIZE", :bold),
      ]

      indices.each do |idx|
        idx_out << idx["name"]
        idx_out << idx["totals"]["converges"]
        idx_out << format_unit(idx["totals"]["size_in_bytes"])
      end

      [
        highline.list(avg_tot_out.map(&:to_s), :uneven_columns_across, 6),
        highline.list(idx_out.map(&:to_s), :uneven_columns_across, 3),
      ].join("\n")
    end

    def format_compliance_data(data)
      return nil unless data["compliance"]["totals"]

      avg = data["compliance"]["averages"]
      tot = data["compliance"]["totals"]
      indices = data["compliance"]["indices"]

      avg_tot_out = [
        highline.color("INDICES GROUP", :bold),
        highline.color("INDICES TOTAL", :bold),
        highline.color("TOTAL INSPEC RUNS", :bold),
        highline.color("AVG DAILY INSPEC RUNS", :bold),
        highline.color("TOTAL SIZE", :bold),
        highline.color("AVG DAILY SIZE", :bold),
        "compliance",
        tot["indices"],
        tot["inspec_summaries"],
        avg["inspec_summaries"],
        format_unit(tot["size_in_bytes"]),
        format_unit(avg["size_in_bytes"]),
      ]

      idx_out = [
        highline.color("INDEX NAME", :bold),
        highline.color("TOTAL INSPEC RUNS", :bold),
        highline.color("TOTAL SIZE", :bold),
      ]

      indices.each do |idx|
        idx_out << idx["name"]
        idx_out << idx["totals"]["inspec_summaries"]
        idx_out << format_unit(idx["totals"]["size_in_bytes"])
      end

      [
        highline.list(avg_tot_out.map!(&:to_s), :uneven_columns_across, 6),
        highline.list(idx_out.map!(&:to_s), :uneven_columns_across, 3),
      ].join("\n")
    end

    def format_cluster_data(data)
      avg = data["averages"]
      return nil unless avg

      c_out = [
      # Add cluster averages
        highline.color("CLUSTER NAME", :bold),
        highline.color("DISK FREE", :bold),
        highline.color("MEM FREE", :bold),
        highline.color("AVG ES CPU %", :bold),
        highline.color("AVG OS CPU %", :bold),
        highline.color("AVG ES HEAP", :bold),
        highline.color("AVG ES NON HEAP", :bold),
        cluster_data["name"],
        format_unit(avg["fs_total_in_bytes"] - avg["fs_free_in_bytes"]),
        format_unit(avg["os_mem_total_in_bytes"] - avg["os_mem_used_in_bytes"]),
        avg["es_cpu_percent"],
        avg["os_cpu_percent"],
        format_unit(avg["jvm_heap_used_in_bytes"]),
        format_unit(avg["jvm_non_heap_used_in_bytes"]),
      ]

      n_out = [
        # Add node specific settings
        highline.color("NODE NAME", :bold),
        highline.color("DISK FREE", :bold),
        highline.color("MEM FREE", :bold),
        highline.color("AVG ES CPU %", :bold),
        highline.color("AVG OS CPU %", :bold),
        highline.color("AVG ES HEAP", :bold),
        highline.color("AVG ES NON HEAP", :bold),
      ]

      data["nodes"].each do |node|
        t = node["totals"]
        n_out << node["name"]
        n_out << format_unit(t["fs_total_in_bytes"] - t["fs_free_in_bytes"])
        n_out << format_unit(t["os_mem_total_in_bytes"] - t["os_mem_used_in_bytes"])
        n_out << t["es_cpu_percent"]
        n_out << t["os_cpu_percent"]
        n_out << format_unit(t["jvm_heap_used_in_bytes"])
        n_out << format_unit(t["jvm_non_heap_used_in_bytes"])
      end

      [
        highline.list(c_out.map!(&:to_s), :uneven_columns_across, 7),
        highline.list(n_out.map!(&:to_s), :uneven_columns_across, 7),
      ].join("\n")
    end

    def transform_to_unit(hash)
      # exit early if we don't need to transform our unit
      return hash unless unit_measure

      # data is already in bytes so we don't need to transform bytes
      return hash if unit_measure == "b"

      new_keys = {}
      hash.each_pair do |k, v|
        if k =~ /bytes/
          hash.delete(k)
          new_keys[k.gsub(/bytes/, unit_measure)] = to_unit_measure(v)
        end
      end
      hash.merge!(new_keys)
      hash
    end

    def to_unit_measure(bytes)
      case unit_measure.downcase
      when "kb"
        (bytes.to_f / KB).round(2)
      when "mb"
        (bytes.to_f / MB).round(2)
      when "gb"
        (bytes.to_f / GB).round(2)
      else
        bytes
      end
    end

    def format_unit(bytes)
      case unit_measure.downcase
      when "kb", "mb", "gb"
        "#{to_unit_measure(bytes)} #{unit_measure.upcase}"
      else
        "#{bytes} B"
      end
    end
  end
end
