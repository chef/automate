# frozen_string_literal: true

require "assert"
require "assert/assertions"

require "assert/utils"

module Assert::Assertions
  class AssertFileExistsTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_file_exists`"
    subject do
      args = args1
      Factory.test do
        assert_file_exists(__FILE__) # pass
        assert_file_exists(*args)    # fail
      end
    end

    let(:desc1){ "assert file exists fail desc" }
    let(:args1){ ["/a/path/to/some/file/that/no/exists", desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to exist."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end

  class AssertNotFileExistsTests < Assert::Context
    include Assert::Test::TestHelpers

    desc "`assert_not_file_exists`"
    subject do
      args = args1
      Factory.test do
        assert_not_file_exists("/file/path") # pass
        assert_not_file_exists(*args)        # fail
      end
    end

    let(:desc1){ "assert not file exists fail desc" }
    let(:args1){ [__FILE__, desc1] }
    let(:config1){ subject.config }

    should "produce results as expected" do
      subject.run(&test_run_callback)

      assert_that(test_run_result_count).equals(2)
      assert_that(test_run_result_count(:pass)).equals(1)
      assert_that(test_run_result_count(:fail)).equals(1)

      exp =
        "#{args1[1]}\nExpected #{Assert::U.show(args1[0], config1)} to "\
        "not exist."
      assert_that(test_run_results(:fail).first.message).equals(exp)
    end
  end
end
