add_command_under_category "node-summary", "Node Utilies", "Get the summary of the nodes in Chef Automate", 2 do
  require "optparse"
  require "ctl-helpers/exit_handler"
  require "node-utils/node_utils"
  require "node-utils/data_summary"
  require "node-utils/log"
  require "node-utils/uuid"

  CtlHelpers::ExitHandler.handle_exit do
    @config = {
      format: "text",
      request_timeout: 300,
    }

    # Backward compat support for the old JSON formatter
    @config[:format] = "json" if ARGV.delete("--json")

    OptionParser.new do |opts|
      opts.banner = "Usage: automate-ctl node-summary [options]"

      opts.on("-f", "--format string", "The output format. 'text' or 'json'") do |format|
        @config[:format] = format
      end

      opts.on("-r", "--request-timeout int", "The Elasticsearch client request timeout in seconds") do |timeout|
        @config[:request_timeout] = timeout.to_i
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end.parse!(ARGV)

    NodeUtils::Log.info(NodeUtils.node_summary(@config))
  end
end

add_command_under_category "data-summary", "Node Utilies", "Get the summary of Chef Automate's data store", 2 do
  require "optparse"
  require "ctl-helpers/exit_handler"
  require "node-utils/node_utils"
  require "node-utils/data_summary"
  require "node-utils/log"
  require "node-utils/uuid"

  CtlHelpers::ExitHandler.handle_exit do
    @config = {}
    @all = true

    OptionParser.new do |opts|
      opts.banner = "Usage: automate-ctl data-summary [options]"

      opts.on("-c", "--compliance", "Display compliance and inspec data") do |bool|
        @config[:compliance] = bool
        @all = false
      end

      opts.on("-f", "--format string", "The output format. 'text' or 'json'") do |format|
        @config[:format] = format
        @config[:unit_measure] ||= "b" # default to bytes with json
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end

      opts.on("-i", "--insights", "Display insights and converge data") do |bool|
        @config[:insights] = bool
        @all = false
      end

      opts.on("-n", "--node", "Display the node-state data") do |bool|
        @config[:node_state] = bool
        @all = false
      end

      opts.on("-s", "--cluster", "Display the Elasticsearch cluster data") do |bool|
        @config[:cluster] = bool
        @all = false
      end

      opts.on("-u", "--unit string", "What unit measurement to use (b, kb, mb, gb)") do |unit|
        @config[:unit_measure] = unit
      end

    end.parse!(ARGV)

    if @all
      @config.merge!({
        compliance: true,
        node_state: true,
        insights: true,
        cluster: true,
      })
    end

    @config[:unit_measure] ||= "gb"

    NodeUtils::Log.info(NodeUtils.data_summary(@config))
  end
end

add_command_under_category "delete-node", "Node Utilies", "Delete a node from Chef Automate", 2 do
  require "optparse"
  require "ctl-helpers/exit_handler"
  require "node-utils/node_utils"
  require "node-utils/data_summary"
  require "node-utils/log"
  require "node-utils/uuid"

  @config = {
    node_data: true,
    compliance_data: false,
    force: false,
    purge: false,
  }

  CtlHelpers::ExitHandler.handle_exit do
    OptionParser.new do |opts|
      opts.banner = "Usage: automate-ctl delete-node [UUID] [options]"

      opts.on("-u", "--uuid string", "The UUID of the node you wish to delete") do |uuid|
        @config[:uuid] = uuid
      end

      opts.on("-n", "--name string", "The name of the node you wish to delete") do |name|
        @config[:name] = name
      end

      opts.on("-o", "--org string", "The organization name of the node you wish to delete") do |org|
        @config[:org] = org
      end

      opts.on("-s", "--chef-server-fqdn string", "The fully qualified domain name of the node's Chef server") do |fqdn|
        @config[:chef_server_fqdn] = fqdn
      end

      opts.on("-b", "--batch-size string", "Maximum number of documents to modify in each Elasicsearch bulk request") do |batch|
        @config[:batch_size] = batch
      end

      opts.on("-d", "--[no-]node-data", "Delete the node run and converge data") do |bool|
        @config[:node_data] = bool
      end

      opts.on("-c", "--[no-]compliance-data", "Delete the node compliance data") do |bool|
        @config[:compliance_data] = bool
      end

      opts.on("--force", "Agree to all warnings and prompts") do
        @config[:force] = true
      end

      opts.on("--purge", "Purge all node data") do
        @config[:node_data] = true
        @config[:compliance_data] = true
        @config[:purge] = true
      end

      opts.on("-r", "--request-timeout int", "The Elasticsearch client request timeout in seconds") do |timeout|
        @config[:request_timeout] = timeout.to_i
      end

      opts.on("-h", "--help", "Show this message") do
        puts opts
        exit
      end
    end.parse!(ARGV)

    if @config[:uuid] && NodeUtils::UUID.regex !~ @config[:uuid]
      raise "'#{@config[:uuid]}' is not a valid node UUID"
    end

    unless @config[:name] || @config[:uuid]
      raise "You must provide either a node UUID or a combination of name, " \
            "org, and chef-server-fqdn. Please see `automate-ctl delete-node -h` " \
            "for all options."
    end

    # Exit early if we're not going to delete anything
    return unless @config[:node_data] || @config[:compliance_data]

    NodeUtils.delete_node(@config)
  end
end
