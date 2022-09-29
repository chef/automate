# frozen_string_literal: true

module Assert; end

class Assert::Context
  module SetupDSL
    def setup_once(&block)
      suite.setup(&block)
    end
    alias_method :before_once, :setup_once
    alias_method :startup, :setup_once

    def teardown_once(&block)
      suite.teardown(&block)
    end
    alias_method :after_once, :teardown_once
    alias_method :shutdown, :teardown_once

    def around(&block)
      arounds << block
    end

    def setup(method_name = nil, &block)
      setups << (block || method_name)
    end
    alias_method :before, :setup

    def teardown(method_name = nil, &block)
      teardowns << (block || method_name)
    end
    alias_method :after, :teardown

    def arounds
      @arounds ||= []
    end

    def setups
      @setups ||= []
    end

    def teardowns
      @teardowns ||= []
    end

    def run_arounds(scope, &run_block)
      context_block =
        arounds.compact.reverse.reduce(run_block) do |run_b, around_b|
          Proc.new{ scope.instance_exec(run_b, &around_b) }
        end

      if superclass.respond_to?(:run_arounds)
        superclass.run_arounds(scope, &context_block)
      else
        context_block.call
      end
    end

    def run_setups(scope)
      # setup the parent...
      superclass.run_setups(scope) if superclass.respond_to?(:run_setups)
      # ... before you setup the child
      setups.compact.each do |setup|
        setup.is_a?(::Proc) ? scope.instance_eval(&setup) : scope.send(setup)
      end
    end

    def run_teardowns(scope)
      # teardown the child...
      teardowns.compact.each do |teardown|
        if teardown.is_a?(::Proc)
          scope.instance_eval(&teardown)
        else
          scope.send(teardown)
        end
      end
      # ... before the parent
      superclass.run_teardowns(scope) if superclass.respond_to?(:run_teardowns)
    end
  end
end
