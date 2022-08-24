# frozen_string_literal: true

require "much-mixin"
require "benchmark"

module Methods; end

module MyMixin
  def self.included(receiver)
    receiver.class_eval{ include Methods }
  end
end

module MyMuchMixin
  include MuchMixin

  mixin_included do
    include Methods
  end
end

Benchmark.bmbm do |x|
  x.report("MyMixin") do
    10_000.times{ Class.new{ include MyMixin } }
  end
  x.report("MyMuchMixin") do
    10_000.times{ Class.new{ include MyMuchMixin } }
  end
end
