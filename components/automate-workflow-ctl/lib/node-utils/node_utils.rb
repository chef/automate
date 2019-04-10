require "node-utils/node_summary"
require "node-utils/delete_node"

module NodeUtils
  module_function

  def node_summary(*args)
    NodeUtils::NodeSummary.summary(*args)
  end

  def data_summary(*args)
    NodeUtils::DataSummary.summary(*args)
  end

  def delete_node(*args)
    NodeUtils::DeleteNode.delete(*args)
  end
end
