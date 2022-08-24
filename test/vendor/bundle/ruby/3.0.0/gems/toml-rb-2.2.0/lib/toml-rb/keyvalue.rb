require_relative "inline_table"

module TomlRB
  class Keyvalue
    attr_reader :dotted_keys, :value, :symbolize_keys

    def initialize(dotted_keys, value)
      @dotted_keys = dotted_keys
      @value = value
      @symbolize_keys = false
    end

    def assign(hash, fully_defined_keys, symbolize_keys = false)
      @symbolize_keys = symbolize_keys
      dotted_keys_str = @dotted_keys.join(".")
      keys = symbolize_keys ? @dotted_keys.map(&:to_sym) : @dotted_keys
      update = keys.reverse.inject(visit_value(@value)) { |k1, k2| {k2 => k1} }

      if @value.is_a?(InlineTable)
        fully_defined_keys << dotted_keys_str
        hash.merge!(update) { |key, _, _| fail ValueOverwriteError.new(key) }
      elsif fully_defined_keys.find { |k| update.dig(*k) }
        hash.merge!(update) { |key, _, _| fail ValueOverwriteError.new(key) }
      else
        dotted_key_merge(hash, update)
      end
    end

    def dotted_key_merge(hash, update)
      hash.merge!(update) { |key, old, new|
        if old.is_a?(Hash) && new.is_a?(Hash)
          dotted_key_merge(old, new)
        else
          fail ValueOverwriteError.new(key)
        end
      }
    end

    def accept_visitor(parser)
      parser.visit_keyvalue self
    end

    private

    def visit_value(a_value)
      return a_value unless a_value.respond_to? :accept_visitor

      a_value.accept_visitor self
    end
  end

  # Used in document.citrus
  module KeyvalueParser
    def value
      TomlRB::Keyvalue.new(capture(:stripped_key).value, capture(:v).value)
    end
  end
end
