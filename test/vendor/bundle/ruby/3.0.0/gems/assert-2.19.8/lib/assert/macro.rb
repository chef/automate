# frozen_string_literal: true

module Assert
  class Macro < ::Proc
    # this class is essentially a way to define a custom set of tests using
    # arguments.  When passed as an argument to the "should" method, a macro
    # will be instance_eval'd in that Assert::Context.
    attr_accessor :name

    def initialize(name = nil, *_args)
      raise ArgumentError unless block_given?
      @name = name || "run this macro"
      super()
    end
  end
end
