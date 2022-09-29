# frozen_string_literal: true

require "assert"
require "much-factory"

require "test/support/factory"

module MuchFactory
  class UnitTests < Assert::Context
    desc "MuchFactory"
    subject{ MuchFactory }

    should have_imeths :integer, :float
    should have_imeths :date, :time, :datetime
    should have_imeths :string, :symbol, :text, :slug, :hex
    should have_imeths :file_name, :dir_path, :file_path
    should have_imeths :path, :url, :email
    should have_imeths :binary, :boolean
    should have_imeths :type_cast, :type_converter

    should "return a random integer using `integer`" do
      assert_that(subject.integer).is_kind_of(Integer)
    end

    should "allow passing a maximum value using `integer`" do
      assert_that([1, 2]).includes(subject.integer(2))
    end

    should "return a random float using `float`" do
      assert_that(subject.float).is_kind_of(Float)
    end

    should "allow passing a maximum value using `float`" do
      float = subject.float(2)
      assert_that(float <= 2).is_true
      assert_that(float >= 0).is_true

      float = subject.float(0)
      assert_that(float).equals(0.0)
    end

    should "allow passing a precision using `float`" do
      float = subject.float(2, precision: 2)
      assert_that((float * 100).round / 100.0).equals(float)

      float = subject.float(0, precision: 2)
      assert_that(float).equals(0.00)
    end

    should "return a random date using `date`" do
      assert_that(subject.date).is_kind_of(Date)
    end

    should "return a random time object using `time`" do
      assert_that(subject.time).is_kind_of(Time)
    end

    should "return a random time object using `datetime`" do
      assert_that(subject.datetime).is_kind_of(DateTime)
    end

    should "return a random String using `string`" do
      assert_that(subject.string).is_kind_of(String)
      assert_that(subject.string.length).equals(10)
    end

    should "allow passing a maximum length using `string`" do
      assert_that(subject.string(1).length).equals(1)
    end

    should "return a random Symbol using `symbol`" do
      assert_that(subject.symbol).is_kind_of(Symbol)
      assert_that(subject.symbol.length).equals(10)
    end

    should "allow passing a maximum length using `symbol`" do
      assert_that(subject.symbol(1).length).equals(1)
    end

    should "return a random string using `text`" do
      assert_that(subject.text).is_kind_of(String)
      assert_that(subject.text.length).equals(20)
    end

    should "allow passing a maximum length using `text`" do
      assert_that(subject.text(1).length).equals(1)
    end

    should "return a random string using `slug`" do
      assert_that(subject.slug).is_kind_of(String)
      assert_that(subject.slug.length).equals(5)
    end

    should "allow passing a maximum length using `slug`" do
      assert_that(subject.slug(1).length).equals(1)
    end

    should "return a random hex string using `hex`" do
      assert_that(subject.hex).is_kind_of(String)
      assert_that(subject.hex).matches(/\A[0-9a-f]{10}\Z/)
    end

    should "allow passing a maximum length using `hex`" do
      assert_that(subject.hex(1).length).equals(1)
    end

    should "return a random file name string using `file_name`" do
      assert_that(subject.file_name).is_kind_of(String)
      assert_that(subject.file_name).matches(/\A[a-z]{6}\.[a-z]{3}\Z/)
    end

    should "allow passing a name length using `file_name`" do
      assert_that(subject.file_name(1)).matches(/\A[a-z]{1}.[a-z]{3}\Z/)
    end

    should "return a random folder path string using `dir_path`" do
      assert_that(subject.dir_path).is_kind_of(String)
      path_segments = subject.dir_path.split("/")
      assert_that(path_segments.size).equals(3)
      path_segments.each{ |s| assert_that(s).matches(/\A[a-z]{4}\Z/) }
    end

    should "allow passing a maximum length using `dir_path`" do
      assert_that(subject.dir_path(1).length).equals(1)
    end

    should "return a random folder path and file name using `file_path`" do
      assert_that(subject.file_path).is_kind_of(String)
      segments = subject.file_path.split("/")
      assert_that(segments.size).equals(4)
      segments[0..-2].each{ |s| assert_that(s).matches(/\A[a-z]{4}\Z/) }
      assert_that(segments.last).matches(/\A[a-z]{6}\.[a-z]{3}\Z/)
    end

    should "return a random url string using `url`" do
      u = subject.url
      segments = u.split("/")

      assert_that(u).is_kind_of(String)
      assert_that(u).matches(%r{\A/})
      assert_that(segments.size).equals(4)
      segments[1..-1].each{ |s| assert_that(s).matches(/\A[a-z]{4}\Z/) }
    end

    should "allow passing a host string using `url`" do
      host = "example.com"
      assert_that(subject.url(host)).matches(%r{\A#{host}/})
    end

    should "allow passing a maximum length using `url`" do
      # plus 1 for the leading "/"
      assert_that(subject.url("", 1).length).equals(2)
    end

    should "return a random email string using `email`" do
      e = subject.email
      assert_that(e).is_kind_of(String)
      assert_that(e).matches(/\A\w+@\w+\.com\Z/)
    end

    should "allow passing a custom domain to `email`" do
      e = subject.email("example.org")
      assert_that(e).matches(/@example\.org\Z/)
    end

    should "allow passing a mailbox length using `email`" do
      assert_that(subject.email(nil, 2).split("@").first.size).equals(2)
    end

    should "return a random binary string using `binary`" do
      assert_that(subject.binary).is_kind_of(String)
    end

    should "return a random boolean using `boolean`" do
      assert_that([TrueClass, FalseClass]).includes(subject.boolean.class)
    end

    should "type cast values to a specified type using `type_cast`" do
      expected = Date.parse("2013-01-01")
      assert_that(subject.type_cast("2013-01-01", :date)).equals(expected)
    end

    should "use `TypedConverter` for the default type converter" do
      assert_that(subject.type_converter).equals(TypeConverter)
    end
  end
end
