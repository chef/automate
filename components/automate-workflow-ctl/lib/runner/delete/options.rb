require 'optparse'

require 'chef-cli-unity/options'

module Runner
  module Delete
    # Class solely responsible handling delete-runner option definitions and parsing.
    class Options < ChefCliUnity::Options
      attr_reader :options, :option_parser

      def initialize(args, desc)
        super(args,
              "delete-runner FQDN [options...]",
              description: desc,
              arguments: {
                "FQDN" => "Fully qualified domain name of the remote host that will be deleted as a runner"
              })
      end

      def additional_options(opts)
        opts.on("-e", "--enterprise ENTERPRISE", "Enterprise to use. Legacy option, only required if you have more than 1 Enterprise configured") do |enterprise|
          options[:enterprise] = enterprise
        end

        opts.on("-y", "--yes", "Skip configuration confirmation and delete the runner with the provided FQDN.") do |yes|
          options[:yes] = yes
        end
      end

    end
  end
end

