# frozen_string_literal: true

module Assert
  class FileLine
    def self.parse(file_line_path)
      new(*(file_line_path.to_s.match(/(^[^\:]*)\:*(\d*).*$/) || [])[1..2])
    end

    attr_reader :file, :line

    def initialize(file = nil, line = nil)
      @file, @line = file.to_s, line.to_s
    end

    def to_s
      "#{file}:#{line}"
    end

    def ==(other)
      if other.is_a?(FileLine)
        file == other.file &&
        line == other.line
      else
        super
      end
    end
  end
end
