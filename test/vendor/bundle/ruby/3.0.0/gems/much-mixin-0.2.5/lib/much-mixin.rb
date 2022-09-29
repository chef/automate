# frozen_string_literal: true

require "much-mixin/version"

module MuchMixin
  def self.included(receiver)
    receiver.class_eval{ extend ClassMethods }
  end

  module ClassMethods
    # Install an included block that first checks if the mix-in has already been
    # included. If it has not been, include the receiver mixin and run all of
    # the `mixin_included` blocks.
    def included(mixin_receiver)
      return if mixin_receiver.include?(much_mixin_included_detector)
      mixin_receiver.send(:include, much_mixin_included_detector)

      much_mixin_included_blocks.each do |block|
        mixin_receiver.class_eval(&block)
      end

      much_mixin_class_method_blocks.each do |block|
        much_mixin_class_methods_module.class_eval(&block)
      end
      mixin_receiver.send(:extend, much_mixin_class_methods_module)

      much_mixin_instance_method_blocks.each do |block|
        much_mixin_instance_methods_module.class_eval(&block)
      end
      mixin_receiver.send(:include, much_mixin_instance_methods_module)

      much_mixin_after_included_blocks.each do |block|
        mixin_receiver.class_eval(&block)
      end
    end

    # The included detector is an empty module that is only used to detect if
    # the mix-in has been included or not. It doesn't add any behavior or
    # methods to the receiver and uses `const_set` to name the module so if its
    # seen in the ancestors it doesn't look like some random module and it can
    # be tracked back to much-mixin.
    def much_mixin_included_detector
      @much_mixin_included_detector ||=
        Module.new.tap do |m|
          const_set("MuchMixinIncludedDetector", m)
        end
    end

    def much_mixin_class_methods_module
      @much_mixin_class_methods_module ||=
        Module.new.tap do |m|
          const_set("MuchMixinClassMethods", m)
        end
    end

    def much_mixin_instance_methods_module
      @much_mixin_instance_methods_module ||=
        Module.new.tap do |m|
          const_set("MuchMixinInstanceMethods", m)
        end
    end

    def much_mixin_included_blocks
      @much_mixin_included_blocks ||= []
    end

    def much_mixin_after_included_blocks
      @much_mixin_after_included_blocks ||= []
    end

    def much_mixin_class_method_blocks
      @much_mixin_class_method_blocks ||= []
    end

    def much_mixin_instance_method_blocks
      @much_mixin_instance_method_blocks ||= []
    end

    def mixin_included(&block)
      much_mixin_included_blocks << block
    end

    def after_mixin_included(&block)
      much_mixin_after_included_blocks << block
    end

    def mixin_class_methods(&block)
      much_mixin_class_method_blocks << block
    end

    def mixin_instance_methods(&block)
      much_mixin_instance_method_blocks << block
    end
  end
end
