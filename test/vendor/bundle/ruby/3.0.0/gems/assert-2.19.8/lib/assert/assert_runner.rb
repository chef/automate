# frozen_string_literal: true

require "assert/cli"

module Assert
  class AssertRunner
    USER_SETTINGS_FILE  = ".assert/init.rb"
    LOCAL_SETTINGS_FILE = ".assert.rb"

    attr_reader :config

    def initialize(config, test_paths, test_options)
      @config = config
      Assert::CLI.bench("Applying settings") do
        apply_user_settings
        apply_local_settings
        apply_env_settings
        apply_option_settings(test_options)
      end

      paths = test_paths.empty? ? [*self.config.test_dir] : test_paths
      files = lookup_test_files(paths)
      init(files, path_of(self.config.test_dir, files.first))
    end

    def init(test_files, test_dir)
      # load any test helper file
      if (
        test_dir &&
        (h = File.join(test_dir, config.test_helper)) &&
        File.exist?(h)
      )
        Assert::CLI.bench("Requiring test helper"){ require h }
      end

      if config.list
        $stdout.puts test_files
        halt
      end

      # load the test files
      runner, suite, view =
        config.runner, config.suite, config.view
      runner.before_load(test_files)
      suite.before_load(test_files)
      view.before_load(test_files)
      Assert::CLI.bench("Requiring #{test_files.size} test files") do
        test_files.each{ |p| require p }
      end
      if config.debug
        puts Assert::CLI.debug_msg("Test files:")
        test_files.each{ |f| puts Assert::CLI.debug_msg("  #{f}") }
      end
      runner.after_load
      suite.after_load
      view.after_load
    end

    def run
      config.runner.run
    end

    private

    def halt
      throw(:halt)
    end

    def apply_user_settings
      safe_require("#{ENV["HOME"]}/#{USER_SETTINGS_FILE}") if ENV["HOME"]
    end

    def apply_local_settings
      safe_require(
        ENV["ASSERT_LOCALFILE"] ||
        path_of(LOCAL_SETTINGS_FILE, Dir.pwd),
      )
    end

    def apply_env_settings
      if ENV["ASSERT_RUNNER_SEED"]
        config.runner_seed ENV["ASSERT_RUNNER_SEED"].to_i
      end
    end

    def apply_option_settings(options)
      config.apply(options)
    end

    def lookup_test_files(test_paths)
      file_paths =
        if config.changed_only
          changed_test_files(test_paths)
        elsif config.single_test?
          globbed_test_files([config.single_test_file_path])
        else
          globbed_test_files(test_paths)
        end

      file_paths.select{ |p| is_test_file?(p) }.sort
    end

    def changed_test_files(test_paths)
      globbed_test_files(config.changed_proc.call(config, test_paths))
    end

    def globbed_test_files(test_paths)
      test_paths.reduce(Set.new) do |paths, path|
        p = File.expand_path(path, Dir.pwd)
        paths + Dir.glob("#{p}*") + Dir.glob("#{p}*/**/*")
      end
    end

    def is_test_file?(path)
      config.test_file_suffixes.reduce(false) do |result, suffix|
        result || path =~ /#{suffix}$/
      end
    end

    def safe_require(settings_file)
      require settings_file if File.exist?(settings_file)
    end

    def path_of(segment, a_path)
      # this method inspects a test path and finds the test dir path.
      full_path = File.expand_path(a_path || ".", Dir.pwd)
      seg_pos = full_path.index(segment_regex(segment))
      File.join(
        if seg_pos && (seg_pos > 0)
          full_path[0..(seg_pos - 1)]
        else
          full_path
        end,
        segment,
      )
    end

    def segment_regex(seg)
      %r{^#{seg}$|^#{seg}/|/#{seg}/|/#{seg}$}
    end
  end
end
