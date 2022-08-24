# frozen_string_literal: true

require "assert"
require "assert/file_line"

class Assert::FileLine
  class UnitTests < Assert::Context
    desc "Assert::FileLine"
    subject{ unit_class }

    let(:unit_class){ Assert::FileLine }

    let(:file1){ "#{Factory.path}_tests.rb" }
    let(:line1){ Factory.integer.to_s }

    should have_imeths :parse

    should "know how to parse and init from a file line path string" do
      file_line_path = [
        "#{file1}:#{line1}",
        "#{file1}:#{line1} #{Factory.string}",
      ].sample
      file_line = subject.parse(file_line_path)

      assert_that(file_line.file).equals(file1)
      assert_that(file_line.line).equals(line1)
    end

    should "handle parsing bad data gracefully" do
      file_line = subject.parse(file1)
      assert_that(file_line.file).equals(file1)
      assert_that(file_line.line).equals("")

      file_line = subject.parse(line1)
      assert_that(file_line.file).equals(line1)
      assert_that(file_line.line).equals("")

      file_line = subject.parse("")
      assert_that(file_line.file).equals("")
      assert_that(file_line.line).equals("")

      file_line = subject.parse(nil)
      assert_that(file_line.file).equals("")
      assert_that(file_line.line).equals("")
    end
  end

  class InitTests < UnitTests
    desc "when init"
    subject{ unit_class.new(file1, line1) }

    should have_readers :file, :line

    should "know its file and line" do
      assert_that(subject.file).equals(file1)
      assert_that(subject.line).equals(line1)

      file_line = unit_class.new(file1)
      assert_that(file_line.file).equals(file1)
      assert_that(file_line.line).equals("")

      file_line = unit_class.new
      assert_that(file_line.file).equals("")
      assert_that(file_line.line).equals("")
    end

    should "know its string representation" do
      assert_that(subject.to_s).equals("#{subject.file}:#{subject.line}")
    end

    should "know if it is equal to another file line" do
      yes = unit_class.new(file1, line1)
      no = unit_class.new("#{Factory.path}_tests.rb", Factory.integer.to_s)

      assert_that(subject).equals(yes)
      assert_not_equal no, subject
    end
  end
end
