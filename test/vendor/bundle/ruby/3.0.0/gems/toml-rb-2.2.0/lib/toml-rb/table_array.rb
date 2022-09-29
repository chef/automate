module TomlRB
  class TableArray
    def initialize(dotted_keys)
      @dotted_keys = dotted_keys
    end

    def navigate_keys(hash, symbolize_keys = false)
      current = hash
      keys = symbolize_keys ? @dotted_keys.map(&:to_sym) : @dotted_keys
      last_key = keys.pop

      # Go over the parent keys
      keys.each do |key|
        current[key] = {} unless current[key]

        if current[key].is_a? Array
          current[key] << {} if current[key].empty?
          current = current[key].last
        else
          current = current[key]
        end
      end

      # Define Table Array
      if current[last_key].is_a? Hash
        fail TomlRB::ParseError,
          "#{last_key} was defined as hash but is now redefined as a table!"
      end
      current[last_key] = [] unless current[last_key]
      current[last_key] << {}

      current[last_key].last
    end

    def accept_visitor(parser)
      parser.visit_table_array self
    end

    def full_key
      @dotted_keys.join(".")
    end
  end

  # Used in document.citrus
  module TableArrayParser
    def value
      TomlRB::TableArray.new(captures[:stripped_key].map(&:value).first)
    end
  end
end
