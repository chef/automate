module TomlRB
  class InlineTable
    def initialize(keyvalue_pairs)
      @pairs = keyvalue_pairs
    end

    def accept_visitor(keyvalue)
      value keyvalue.symbolize_keys
    end

    def value(symbolize_keys = false)
      result = {}
      @pairs.each do |kv|
        update = kv.assign({}, [], symbolize_keys)
        result.merge!(update) { |key, _, _| fail ValueOverwriteError.new(key) }
      end
      result
    end
  end

  module InlineTableParser
    def value
      TomlRB::InlineTable.new captures[:keyvalue].map(&:value)
    end
  end
end
