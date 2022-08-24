# frozen_string_literal: true

require "assert/macro"

module Assert::Macros
  module Methods
    def self.included(receiver)
      receiver.send(:extend, ClassMethods)
    end

    module ClassMethods
      def have_instance_method(*methods)
        called_from =
          (methods.last.is_a?(Array) ? methods.pop : caller_locations).first
        Assert::Macro.new do
          methods.each do |m|
            _methods_macro_instance_methods << [m, called_from]
          end
          _methods_macro_test called_from
        end
      end
      alias_method :have_instance_methods, :have_instance_method
      alias_method :have_imeth, :have_instance_method
      alias_method :have_imeths, :have_instance_method

      def not_have_instance_method(*methods)
        called_from =
          (methods.last.is_a?(Array) ? methods.pop : caller_locations).first
        Assert::Macro.new do
          methods.each do |m|
            _methods_macro_not_instance_methods << [m, called_from]
          end
          _methods_macro_test called_from
        end
      end
      alias_method :not_have_instance_methods, :not_have_instance_method
      alias_method :not_have_imeth, :not_have_instance_method
      alias_method :not_have_imeths, :not_have_instance_method

      def have_class_method(*methods)
        called_from =
          (methods.last.is_a?(Array) ? methods.pop : caller_locations).first
        Assert::Macro.new do
          methods.each{ |m| _methods_macro_class_methods << [m, called_from] }
          _methods_macro_test called_from
        end
      end
      alias_method :have_class_methods, :have_class_method
      alias_method :have_cmeth, :have_class_method
      alias_method :have_cmeths, :have_class_method

      def not_have_class_method(*methods)
        called_from =
          (methods.last.is_a?(Array) ? methods.pop : caller_locations).first
        Assert::Macro.new do
          methods.each do |m|
            _methods_macro_not_class_methods << [m, called_from]
          end
          _methods_macro_test called_from
        end
      end
      alias_method :not_have_class_methods, :not_have_class_method
      alias_method :not_have_cmeth, :not_have_class_method
      alias_method :not_have_cmeths, :not_have_class_method

      def have_reader(*methods)
        methods << caller_locations unless methods.last.is_a?(Array)
        have_instance_methods(*methods)
      end
      alias_method :have_readers, :have_reader

      def not_have_reader(*methods)
        methods << caller_locations unless methods.last.is_a?(Array)
        not_have_instance_methods(*methods)
      end
      alias_method :not_have_readers, :not_have_reader

      def have_writer(*methods)
        called = methods.last.is_a?(Array) ? methods.pop : caller_locations
        writer_meths = methods.map{ |m| "#{m}=" }
        writer_meths << called
        have_instance_methods(*writer_meths)
      end
      alias_method :have_writers, :have_writer

      def not_have_writer(*methods)
        called = methods.last.is_a?(Array) ? methods.pop : caller_locations
        writer_meths = methods.map{ |m| "#{m}=" }
        writer_meths << called
        not_have_instance_methods(*writer_meths)
      end
      alias_method :not_have_writers, :not_have_writer

      def have_accessor(*methods)
        called = methods.last.is_a?(Array) ? methods.pop : caller_locations
        accessor_meths = methods.map{ |m| [m, "#{m}="] }.flatten
        accessor_meths << called
        have_instance_methods(*accessor_meths)
      end
      alias_method :have_accessors, :have_accessor

      def not_have_accessor(*methods)
        called = methods.last.is_a?(Array) ? methods.pop : caller_locations
        accessor_meths = methods.map{ |m| [m, "#{m}="] }.flatten
        accessor_meths << called
        not_have_instance_methods(*accessor_meths)
      end
      alias_method :not_have_accessors, :not_have_accessor

      # private

      def _methods_macro_test(called_from)
        @_methods_macro_test ||=
          test "should respond to methods", called_from do
            self
              .class
              ._methods_macro_instance_methods
              .each do |(method, called_from)|
                msg =
                  "#{subject.class.name} does not have "\
                  "instance method ##{method}"
                with_backtrace([called_from]) do
                  assert_that(subject).responds_to(method, msg)
                end
              end

            self
              .class
              ._methods_macro_class_methods
              .each do |(method, called_from)|
                msg =
                  "#{subject.class.name} does not have class method ##{method}"
                with_backtrace([called_from]) do
                  assert_that(subject.class).responds_to(method, msg)
                end
              end

            self
              .class
              ._methods_macro_not_instance_methods
              .each do |(method, called_from)|
                msg = "#{subject.class.name} has instance method ##{method}"
                with_backtrace([called_from]) do
                  assert_that(subject).does_not_respond_to(method, msg)
                end
              end

            self
              .class
              ._methods_macro_not_class_methods
              .each do |(method, called_from)|
                msg = "#{subject.class.name} has class method ##{method}"
                with_backtrace([called_from]) do
                  assert_that(subject.class).does_not_respond_to(method, msg)
                end
              end
          end
      end

      def _methods_macro_instance_methods
        @_methods_macro_instance_methods ||= []
      end

      def _methods_macro_not_instance_methods
        @_methods_macro_not_instance_methods ||= []
      end

      def _methods_macro_class_methods
        @_methods_macro_class_methods ||= []
      end

      def _methods_macro_not_class_methods
        @_methods_macro_not_class_methods ||= []
      end
    end
  end
end
