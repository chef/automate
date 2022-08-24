# frozen_string_literal: true

module Assert
  module ConfigHelpers
    def runner
      config.runner
    end

    def suite
      config.suite
    end

    def view
      config.view
    end

    def runner_seed
      config.runner_seed
    end

    def single_test?
      config.single_test?
    end

    def single_test_file_line
      config.single_test_file_line
    end

    def tests_to_run?
      config.suite.tests_to_run?
    end

    def tests_to_run_count
      config.suite.tests_to_run_count
    end

    def test_count
      config.suite.test_count
    end

    def result_count
      config.suite.result_count
    end

    def pass_result_count
      config.suite.pass_result_count
    end

    def fail_result_count
      config.suite.fail_result_count
    end

    def error_result_count
      config.suite.error_result_count
    end

    def skip_result_count
      config.suite.skip_result_count
    end

    def ignore_result_count
      config.suite.ignore_result_count
    end

    def all_pass?
      pass_result_count == result_count
    end

    def formatted_run_time(run_time, format = "%.6f")
      format % run_time
    end

    def formatted_test_rate(test_rate, format = "%.6f")
      format % test_rate
    end

    def formatted_result_rate(result_rate, format = "%.6f")
      format % result_rate
    end

    def formatted_suite_run_time(format = "%.6f")
      formatted_run_time(config.suite.run_time, format)
    end

    def formatted_suite_test_rate(format = "%.6f")
      formatted_test_rate(config.suite.test_rate, format)
    end

    def formatted_suite_result_rate(format = "%.6f")
      formatted_result_rate(config.suite.result_rate, format)
    end

    def show_test_profile_info?
      !!config.profile
    end

    def show_test_verbose_info?
      !!config.verbose
    end

    # return a list of result type symbols that have actually occurred
    def ocurring_result_types
      @result_types ||=
        [:pass, :fail, :ignore, :skip, :error].select do |sym|
          send("#{sym}_result_count") > 0
        end
    end

    private

    def get_rate(count, time)
      time == 0 ? 0.0 : (count.to_f / time.to_f)
    end
  end
end
