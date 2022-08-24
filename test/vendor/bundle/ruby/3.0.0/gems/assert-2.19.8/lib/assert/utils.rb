# frozen_string_literal: true

require "assert"

module Assert
  module Utils
    # show objects in a human-readable manner.  Either inspects or pretty-prints
    # them depending on settings.
    def self.show(obj, config)
      out = config.pp_objects ? config.pp_proc.call(obj) : obj.inspect
      out = out.encode(Encoding.default_external) if defined?(Encoding)
      out
    end

    # show objects in a human-readable manner and make the output diff-able.
    # This expands on the basic `show` util by escaping newlines and making
    # object id hex-values generic.
    def self.show_for_diff(obj, config)
      show(obj, config)
        .gsub(/\\n/, "\n")
        .gsub(/:0x[a-fA-F0-9]{4,}/m, ":0xXXXXXX")
    end

    # open a tempfile and yield it
    def self.tempfile(name, content)
      require "tempfile"
      Tempfile.open(name) do |tmpfile|
        tmpfile.puts(content)
        tmpfile.flush
        yield tmpfile if block_given?
      end
    end

    # Get a proc that uses stdlib `PP.pp` to pretty print objects
    def self.stdlib_pp_proc(width = nil)
      require "pp"
      Proc.new{ |obj| PP.pp(obj, +"", width || 79).strip }
    end

    # Return true if if either show output has newlines or is bigger than 29
    # chars.
    def self.default_use_diff_proc
      Proc.new do |exp_show_output, act_show_output|
        exp_show_output.include?("\n") || exp_show_output.size > 29 ||
        act_show_output.include?("\n") || act_show_output.size > 29
      end
    end

    # use `diff` system cmd to show exp/act show output
    def self.syscmd_diff_proc(syscmd = "diff --unified=-1")
      Proc.new do |exp_show_output, act_show_output|
        result = ""
        tempfile("exp_show_output", exp_show_output) do |a|
          tempfile("act_show_output", act_show_output) do |b|
            result = `#{syscmd} #{a.path} #{b.path}`.strip
            result.sub!(/^\-\-\- .+/, "--- expected")
            result.sub!(/^\+\+\+ .+/, "+++ actual")
            result = "--- expected\n+++ actual" if result.empty?
          end
        end
        result
      end
    end

    # use git to determine which files have changes
    def self.git_changed_proc
      Proc.new do |config, test_paths|
        files = []
        cmd =
          [
            # changed files
            "git diff --no-ext-diff --name-only #{config.changed_ref}",
            # added files
            "git ls-files --others --exclude-standard",
          ]
            .map{ |c| "#{c} -- #{test_paths.join(" ")}" }
            .join(" && ")
        Assert::CLI.bench("Load only changed files") do
          files = `#{cmd}`.split("\n")
        end
        puts Assert::CLI.debug_msg("  `#{cmd}`") if config.debug
        files
      end
    end
  end

  # alias for brevity
  U = Utils
end
