module Automate
  module Backend
    # Connector class
    class Connector
      attr_accessor :config_path

      def initialize(path)
        @config_path = path
      end

      def content
        Log.info "Parsing toml file #{config_path}"
        TomlRB.load_file(config_path, symbolize_keys: true)
      end
    end
  end
end
