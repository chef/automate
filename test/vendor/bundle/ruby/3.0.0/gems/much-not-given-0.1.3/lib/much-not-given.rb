# frozen_string_literal: true

require "much-not-given/version"
require "much-mixin"

module MuchNotGiven
  include MuchMixin

  mixin_class_methods do
    def not_given
      @not_given ||=
        begin
          Class.new{
            def initialize(receiver_name)
              @receiver_name = receiver_name
            end

            def blank?
              true
            end

            def present?
              false
            end

            def to_s
              "#{@receiver_name}.not_given"
            end

            def inspect
              to_s
            end

            def ==(other)
              if other.is_a?(self.class)
                true
              else
                super
              end
            end
          }.new(inspect)
        end
    end

    def not_given?(value)
      value == not_given
    end

    def given?(value)
      value != not_given
    end
  end
end
