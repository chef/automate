module Assert
  class CLIRB  # Version 1.2.0, https://github.com/redding/cli.rb
    Error    = Class.new(RuntimeError);
    HelpExit = Class.new(RuntimeError); VersionExit = Class.new(RuntimeError)
    attr_reader :argv, :args, :opts, :data

    def initialize(&block)
      @options = []; instance_eval(&block) if block
      require "optparse"
      @data, @args, @opts = [], [], {}; @parser = OptionParser.new do |p|
        p.banner = ""; @options.each do |o|
          @opts[o.name] = o.value; p.on(*o.parser_args){ |v| @opts[o.name] = v }
        end
        p.on_tail("--version", ""){ |v| raise VersionExit, v.to_s }
        p.on_tail("--help",    ""){ |v| raise HelpExit,    v.to_s }
      end
    end

    def option(*args, **kargs); @options << Option.new(*args, **kargs); end
    def parse!(argv)
      @args = (argv || []).dup.tap do |args_list|
        begin; @parser.parse!(args_list)
        rescue OptionParser::ParseError => err; raise Error, err.message; end
      end; @data = @args + [@opts]
    end
    def to_s; @parser.to_s; end
    def inspect
      "#<#{self.class}:#{"0x0%x" % (object_id << 1)} @data=#{@data.inspect}>"
    end

    class Option
      attr_reader :name, :opt_name, :desc, :abbrev, :value, :klass, :parser_args

      def initialize(name, desc = nil, abbrev: nil, value: nil)
        @name, @desc = name, desc || ""
        @opt_name, @abbrev = parse_name_values(name, abbrev)
        @value, @klass = gvalinfo(value)
        @parser_args = if [TrueClass, FalseClass, NilClass].include?(@klass)
          ["-#{@abbrev}", "--[no-]#{@opt_name}", @desc]
        else
          ["-#{@abbrev}", "--#{@opt_name} VALUE", @klass, @desc]
        end
      end

      private

      def parse_name_values(name, custom_abbrev)
        [ (processed_name = name.to_s.strip.downcase).gsub("_", "-"),
          custom_abbrev || processed_name.gsub(/[^a-z]/, "").chars.first || "a"
        ]
      end
      def gvalinfo(v); v.kind_of?(Class) ? [nil,v] : [v,v.class]; end
    end
  end
end
