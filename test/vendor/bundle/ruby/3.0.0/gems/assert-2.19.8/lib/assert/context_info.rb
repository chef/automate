# frozen_string_literal: true

module Assert
  class ContextInfo
    attr_reader :called_from, :klass, :file

    def initialize(klass, called_from = nil, first_caller = nil)
      @called_from = called_from || first_caller
      @klass = klass
      @file = @called_from.to_s.gsub(/\:[0-9]+.*$/, "") if @called_from
    end

    def test_name(name)
      [klass.description.to_s, name.to_s].compact.reject(&:empty?).join(" ")
    end
  end
end
