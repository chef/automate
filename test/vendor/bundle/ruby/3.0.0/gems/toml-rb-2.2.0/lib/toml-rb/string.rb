module TomlRB
  # Used in primitive.citrus
  module BasicString
    SPECIAL_CHARS = {
      "\\0" => "\0",
      "\\t" => "\t",
      "\\b" => "\b",
      "\\f" => "\f",
      "\\n" => "\n",
      "\\r" => "\r",
      '\\"' => '"',
      "\\\\" => "\\"
    }.freeze

    def value
      aux = TomlRB::BasicString.transform_escaped_chars first.value

      aux[1...-1]
    end

    # Replace the unicode escaped characters with the corresponding character
    # e.g. \u03B4 => ?
    def self.decode_unicode(str)
      [str[2..-1].to_i(16)].pack("U")
    end

    def self.transform_escaped_chars(str)
      str.gsub(/\\(u[\da-fA-F]{4}|U[\da-fA-F]{8}|.)/) do |m|
        if m.size == 2
          SPECIAL_CHARS[m] || parse_error(m)
        else
          decode_unicode(m).force_encoding("UTF-8")
        end
      end
    end

    def self.parse_error(m)
      fail ParseError.new "Escape sequence #{m} is reserved"
    end
  end

  module LiteralString
    def value
      first.value[1...-1]
    end
  end

  module MultilineString
    def value
      return "" if captures[:text].empty?
      aux = captures[:text].first.value

      # Remove spaces on multilined Singleline strings
      aux.gsub!(/\\\r?\n[\n\t\r ]*/, "")

      TomlRB::BasicString.transform_escaped_chars aux
    end
  end

  module MultilineLiteral
    def value
      return "" if captures[:text].empty?
      aux = captures[:text].first.value

      aux.gsub(/\\\r?\n[\n\t\r ]*/, "")
    end
  end
end
