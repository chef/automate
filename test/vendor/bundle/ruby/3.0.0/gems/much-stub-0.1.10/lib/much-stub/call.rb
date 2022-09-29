# frozen_string_literal: true

module MuchStub
  class Call
    attr_reader :pargs, :kargs, :block

    def initialize(*pargs, **kargs, &block)
      @pargs = pargs.empty? ? nil : pargs
      @kargs = kargs.empty? ? nil : kargs
      @block = block
    end

    def args
      @args ||= [*pargs, kargs].compact
    end
  end
end
