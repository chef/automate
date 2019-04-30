
require "chef-cli-unity/options"

module Telemetry
  class Options < ChefCliUnity::Options

    def initialize(args, desc)
      super(args,
            "Usage: workflow-ctl telemetry ACTION",
            description: desc,
            arguments: {
                "ACTION" => "Action to take on Telemetry. It must be enable, disable or status.",
              })
    end

  end
end
