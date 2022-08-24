# frozen_string_literal: true

module Assert; end

class Assert::Context
  module SuiteDSL
    def suite(suite_obj = nil)
      if suite_obj
        @suite = suite_obj
      else
        @suite ||
        if superclass.respond_to?(:suite)
          superclass.suite
        else
          Assert.suite
        end
      end
    end
  end
end
