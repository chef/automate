#
# Author:: Dominik Richter (<dominik.richter@gmail.com>)
# Author:: Christoph Hartmann (<chris@lollyrock.com>)

module Train
  module Options
    def self.attach(target)
      target.class.method(:include).call(ClassOptions)
      target.method(:include).call(InstanceOptions)
    end

    module ClassOptions
      def option(name, conf = nil, &block)
        d = conf || {}
        unless d.is_a? Hash
          raise Train::ClientError,
            "The transport plugin #{self} declared an option #{name} "\
            "and didn't provide a valid configuration hash."
        end

        if !conf.nil? && !conf[:default].nil? && block_given?
          raise Train::ClientError,
            "The transport plugin #{self} declared an option #{name} "\
            "with both a default value and block. Only use one of these."
        end

        d[:default] = block if block_given?

        default_options[name] = d
      end

      def default_options
        @default_options = {} unless defined? @default_options
        @default_options
      end

      def include_options(other)
        unless other.respond_to?(:default_options)
          raise "Trying to include options from module #{other.inspect}, "\
               "which doesn't seem to support options."
        end
        default_options.merge!(other.default_options)
      end
    end

    module InstanceOptions
      # @return [Hash] options, which created this Transport
      attr_reader :options

      def default_options
        self.class.default_options
      end

      def merge_options(base, opts)
        res = base.merge(opts || {})
        default_options.each do |field, hm|
          next unless res[field].nil? && hm.key?(:default)

          default = hm[:default]
          if default.is_a? Proc
            res[field] = default.call(res)
          elsif hm.key?(:coerce)
            field_value = hm[:coerce].call(res)
            res[field] = field_value.nil? ? default : field_value
          else
            res[field] = default
          end
        end
        res
      end

      def validate_options(opts)
        default_options.each do |field, hm|
          if opts[field].nil? && hm[:required]
            raise Train::ClientError,
              "You must provide a value for #{field.to_s.inspect}."
          end
        end
        opts
      end
    end
  end
end
