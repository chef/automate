require "json"

module NodeUtils
  module Config
    def delivery_running
      @delivery_running ||= begin
        path = "/etc/delivery/delivery-running.json"
        raise "You must configure Chef Automate before running the node-summary.\n" unless File.exist?(path)
        JSON.parse(File.read(path))
      end
    end
    module_function :delivery_running

    def missing_threshold_mins
      delivery_running["delivery"]["insights"]["node_missing_threshold_mins"]
    end
    module_function :missing_threshold_mins
  end
end
