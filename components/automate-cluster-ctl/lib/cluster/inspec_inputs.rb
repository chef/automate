require 'yaml'

module AutomateCluster
  class InspecInputs
    def initialize
      @inputs = {}
    end

    def input_file
      @input_file ||= Tempfile.new(['inpsec-inputs', '.yml'])
    end

    def add(key, value)
      @inputs[key] = value
    end

    def path
      input_file.path
    end

    def delete
      input_file.close
      input_file.unlink
    end

    def save
      input_file.truncate(0)
      input_file << @inputs.to_yaml unless @inputs.keys.empty?
      input_file.fsync

      input_file.path
    end
  end
end
