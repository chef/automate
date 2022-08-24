# frozen_string_literal: true

module Assert; end
class Assert::Context; end

module Assert::Context::LetDSL
  def let(name, &block)
    send(:define_method, name, &->{
      unless instance_variable_defined?("@__assert_let_#{name}__")
        instance_variable_set(
          "@__assert_let_#{name}__",
          instance_eval(&block),
        )
      end

      instance_variable_get("@__assert_let_#{name}__")
    })
  end
end
