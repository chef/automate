# frozen_string_literal: true

require "benchmark"
require "set"
require "assert/assert_runner"
require "assert/clirb"
require "assert/version"

module Assert
  class CLI
    SUCCESS_EXIT_STATUS = 0
    ERROR_EXIT_STATUS   = 1

    def self.debug?(args)
      args.include?("-d") || args.include?("--debug")
    end

    def self.debug_msg(msg)
      "[DEBUG] #{msg}"
    end

    def self.debug_start_msg(msg)
      debug_msg("#{msg}...".ljust(30))
    end

    def self.debug_finish_msg(time_in_ms)
      " (#{time_in_ms} ms)"
    end

    def self.bench(start_msg, &block)
      unless Assert.config.debug
        block.call
        return
      end

      print debug_start_msg(start_msg)
      RoundedMillisecondTime
        .new(Benchmark.measure(&block).real)
        .tap do |time_in_ms|
          puts debug_finish_msg(time_in_ms)
        end
    end

    def initialize(*args)
      @args = args
      @cli =
        CLIRB.new do
          option(
            "runner_seed",
            "use a given seed to run tests",
            abbrev: "s",
            value: Integer,
          )
          option(
            "changed_only",
            "only run test files with changes",
            abbrev: "c",
          )
          option(
            "changed_ref",
            "reference for changes, use with `-c` opt",
            abbrev: "r",
            value: "",
          )
          option(
            "single_test",
            "only run the test on the given file/line",
            abbrev: "t",
            value: "",
          )
          option(
            "pp_objects",
            "pretty-print objects in fail messages",
            abbrev: "p",
          )
          option(
            "capture_output",
            "capture stdout and display in result details",
            abbrev: "o",
          )
          option(
            "halt_on_fail",
            "halt a test when it fails",
            abbrev: "h",
          )
          option(
            "profile",
            "output test profile info",
            abbrev: "e",
          )
          option(
            "verbose",
            "output verbose runtime test info",
            abbrev: "v",
          )
          option(
            "list",
            "list test files on $stdout",
            abbrev: "l",
          )

          # show loaded test files, cli err backtraces, etc
          option "debug", "run in debug mode", abbrev: "d"
        end
    end

    def run
      fails_plus_errors_count = 0
      begin
        @cli.parse!(@args)
        catch(:halt) do
          fails_plus_errors_count =
            Assert::AssertRunner.new(Assert.config, @cli.args, @cli.opts).run
        end
      rescue CLIRB::HelpExit
        puts help
        exit(SUCCESS_EXIT_STATUS)
      rescue CLIRB::VersionExit
        puts Assert::VERSION
        exit(SUCCESS_EXIT_STATUS)
      rescue CLIRB::Error => ex
        puts "#{ex.message}\n\n"
        puts Assert.config.debug ? ex.backtrace.join("\n") : help
        exit(ERROR_EXIT_STATUS)
      rescue => ex
        puts "#{ex.class}: #{ex.message}"
        puts ex.backtrace.join("\n")
        exit(ERROR_EXIT_STATUS)
      end
      exit(
        fails_plus_errors_count == 0 ? SUCCESS_EXIT_STATUS : ERROR_EXIT_STATUS,
      )
    end

    def help
      "Usage: assert [options] [TESTS]\n\n"\
      "Options:"\
      "#{@cli}"
    end
  end

  module RoundedMillisecondTime
    ROUND_PRECISION = 3
    ROUND_MODIFIER = 10**ROUND_PRECISION
    def self.new(time_in_seconds)
      (time_in_seconds * 1000 * ROUND_MODIFIER).to_i / ROUND_MODIFIER.to_f
    end
  end
end
