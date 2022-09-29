# frozen_string_literal: true

require "assert"
require "assert/view_helpers"

require "stringio"
require "assert/config"
require "assert/config_helpers"
require "assert/result"
require "assert/view"

module Assert::ViewHelpers
  class UnitTests < Assert::Context
    desc "Assert::ViewHelpers"
    subject{ unit_class }

    let(:unit_class) do
      test_opt_val = test_opt_val1
      Class.new do
        include Assert::ViewHelpers

        option "test_opt", test_opt_val

        def config
          # use the assert config since it has tests, contexts, etc
          # also maybe use a fresh config that is empty
          @config ||= [Assert.config, Assert::Config.new].sample
        end
      end
    end

    let(:test_opt_val1){ Factory.string }

    should have_imeths :option

    should "include the config helpers" do
      assert_that(subject).includes(Assert::ConfigHelpers)
    end

    should "write option values" do
      helpers = unit_class.new
      assert_that(helpers.test_opt).equals(test_opt_val1)

      new_val = Factory.integer
      helpers.test_opt new_val
      assert_that(helpers.test_opt).equals(new_val)

      other_val = Factory.integer
      helpers.test_opt new_val, other_val
      assert_that(helpers.test_opt).equals([new_val, other_val])
    end
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new }

    should have_imeths :captured_output, :re_run_test_cmd
    should have_imeths :tests_to_run_count_statement, :result_count_statement
    should have_imeths :to_sentence
    should have_imeths :all_pass_result_summary_msg, :result_summary_msg
    should have_imeths :results_summary_sentence

    should "know how to build captured output" do
      output = Factory.string
      exp =
        "--- stdout ---\n"\
        "#{output}"\
        "--------------"
      assert_that(subject.captured_output(output)).equals(exp)
    end

    should "know how to build the re-run test cmd" do
      test_id = "#{Dir.pwd}/#{Factory.string}_tests.rb:#{Factory.integer}"
      exp = "assert -t #{test_id.gsub(Dir.pwd, ".")}"
      assert_that(subject.re_run_test_cmd(test_id)).equals(exp)
    end

    should "know its tests-to-run count and result count statements" do
      exp =
        "#{subject.tests_to_run_count} "\
        "test#{"s" if subject.tests_to_run_count != 1}"
      assert_that(subject.tests_to_run_count_statement).equals(exp)

      exp = "#{subject.result_count} result#{"s" if subject.result_count != 1}"
      assert_that(subject.result_count_statement).equals(exp)
    end

    should "know how to build a sentence from a list of items" do
      items = 1.times.map{ Factory.string }
      assert_that(subject.to_sentence(items)).equals(items.first)

      items = 2.times.map{ Factory.string }
      assert_that(subject.to_sentence(items)).equals(items.join(" and "))

      items = (Factory.integer(3) + 2).times.map{ Factory.string }
      exp = [items[0..-2].join(", "), items.last].join(", and ")
      assert_that(subject.to_sentence(items)).equals(exp)
    end

    should "know its all pass result summary message" do
      Assert.stub(subject, :result_count){ 0 }
      assert_that(subject.all_pass_result_summary_msg).equals("uhh...")

      Assert.stub(subject, :result_count){ 1 }
      assert_that(subject.all_pass_result_summary_msg).equals("pass")

      Assert.stub(subject, :result_count){ Factory.integer(10) + 1 }
      assert_that(subject.all_pass_result_summary_msg).equals("all pass")
    end

    should "know its result summary msg" do
      res_type = :pass
      Assert.stub(subject, :all_pass?){ true }
      exp = subject.all_pass_result_summary_msg
      assert_that(subject.result_summary_msg(res_type)).equals(exp)

      Assert.stub(subject, :all_pass?){ false }
      res_type = [:pass, :ignore, :fail, :skip, :error].sample
      exp = "#{subject.send("#{res_type}_result_count")} #{res_type}"
      assert_that(subject.result_summary_msg(res_type)).equals(exp)
    end

    should "know its results summary sentence" do
      items =
        subject.ocurring_result_types.map do |result_sym|
          subject.result_summary_msg(result_sym)
        end
      exp = subject.to_sentence(items)
      assert_that(subject.results_summary_sentence).equals(exp)

      block = proc{ |summary, result| "#{summary}--#{result}" }
      items =
        subject.ocurring_result_types.map do |result_sym|
          block.call(subject.result_summary_msg(result_sym), result_sym)
        end
      exp = subject.to_sentence(items)
      assert_that(subject.results_summary_sentence(&block)).equals(exp)
    end
  end

  class AnsiTests < UnitTests
    desc "Ansi"
    subject{ Ansi }

    should have_imeths :code_for

    should "know its codes" do
      assert_that(subject::CODES).is_not_empty
    end

    should "map its code style names to ansi code strings" do
      styles = Factory.integer(3).times.map{ subject::CODES.keys.sample }
      exp = styles.map{ |n| "\e[#{subject::CODES[n]}m" }.join("")
      assert_that(subject.code_for(*styles)).equals(exp)

      styles = Factory.integer(3).times.map{ Factory.string }
      assert_that(subject.code_for(*styles)).equals("")

      styles = []
      assert_that(subject.code_for(*styles)).equals("")
    end
  end

  class AnsiInitTests < UnitTests
    desc "when mixed in on a view"
    subject{ view1 }

    let(:view_class1){ Class.new(Assert::View){ include Ansi } }
    let(:view1) do
      view_class1.new(Factory.modes_off_config, StringIO.new(+"", "w+"))
    end

    should have_imeths :ansi_styled_msg

    should "know how to build ansi styled messages" do
      msg = Factory.string
      result_type = [:pass, :fail, :error, :skip, :ignore].sample

      Assert.stub(subject, :is_tty?){ false }
      Assert.stub(subject, :styled){ false }
      assert_that(subject.ansi_styled_msg(msg, result_type)).equals(msg)

      Assert.stub(subject, :is_tty?){ false }
      Assert.stub(subject, :styled){ true }
      assert_that(subject.ansi_styled_msg(msg, result_type)).equals(msg)

      Assert.stub(subject, :is_tty?){ true }
      Assert.stub(subject, :styled){ false }
      assert_that(subject.ansi_styled_msg(msg, result_type)).equals(msg)

      Assert.stub(subject, :is_tty?){ true }
      Assert.stub(subject, :styled){ true }
      Assert.stub(subject, "#{result_type}_styles"){ [] }
      assert_that(subject.ansi_styled_msg(msg, result_type)).equals(msg)

      styles =
        Factory.integer(3).times.map do
          Assert::ViewHelpers::Ansi::CODES.keys.sample
        end
      Assert.stub(subject, "#{result_type}_styles"){ styles }
      exp_code = Assert::ViewHelpers::Ansi.code_for(*styles)
      exp = exp_code + msg + Assert::ViewHelpers::Ansi.code_for(:reset)
      assert_that(subject.ansi_styled_msg(msg, result_type)).equals(exp)
    end
  end
end
