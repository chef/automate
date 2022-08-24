# frozen_string_literal: true

require "much-factory/version"

require "date"
require "time"

module MuchFactory
  extend self

  def integer(max = nil)
    type_cast(Random.integer(max), :integer)
  end

  def float(max = nil, precision: Factory.integer(5))
    factor = (10**precision).to_f
    (type_cast(Random.float(max), :float) * factor).round / factor
  end

  DAYS_IN_A_YEAR = 365
  SECONDS_IN_DAY = 24 * 60 * 60

  def date
    @date ||= type_cast(Random.date_string, :date)
    @date + Random.integer(DAYS_IN_A_YEAR)
  end

  def time
    @time ||= type_cast(Random.time_string, :time)
    @time + (Random.float(DAYS_IN_A_YEAR) * SECONDS_IN_DAY).to_i
  end

  def datetime
    @datetime ||= type_cast(Random.datetime_string, :datetime)
    @datetime + (Random.float(DAYS_IN_A_YEAR) * SECONDS_IN_DAY).to_i
  end

  def string(length = nil)
    type_cast(Random.string(length || 10), :string)
  end

  def symbol(*args)
    string(*args).to_sym
  end

  def text(length = nil)
    type_cast(Random.string(length || 20), :string)
  end

  def slug(length = nil)
    type_cast(Random.string(length || 5), :string)
  end

  def hex(length = nil)
    type_cast(Random.hex_string(length), :string)
  end

  def file_name(length = nil)
    type_cast(Random.file_name_string(length), :string)
  end

  def dir_path(length = nil)
    type_cast(Random.dir_path_string(length), :string)
  end

  def file_path
    type_cast(Random.file_path_string, :string)
  end

  alias_method :path, :dir_path

  def url(host = nil, length = nil)
    type_cast(Random.url_string(host, length), :string)
  end

  def email(domain = nil, length = nil)
    type_cast(Random.email_string(domain, length), :string)
  end

  def binary
    type_cast(Random.binary, :binary)
  end

  def boolean
    type_cast(Random.integer.even?, :boolean)
  end

  def type_cast(value, type)
    type_converter.send(type, value)
  end

  def type_converter
    TypeConverter
  end

  module TypeConverter
    def self.string(input)
      input.to_s
    end

    def self.integer(input)
      input.to_i
    end

    def self.float(input)
      input.to_f
    end

    def self.datetime(input)
      DateTime.parse(input.to_s) # rubocop:disable Style/DateTime
    end

    def self.time(input)
      Time.parse(input.to_s)
    end

    def self.date(input)
      Date.parse(input.to_s)
    end

    def self.boolean(input)
      !!input
    end

    def self.binary(input)
      input
    end
  end

  module Random
    # rand given a max int value returns integers between 0 and max-1
    def self.integer(max = nil)
      rand(max || 32_766) + 1
    end

    # `rand` with no args gives a float between 0 and 1
    def self.float(max = nil)
      (max || 100).to_f * rand
    end

    def self.date_string
      Time.now.strftime("%Y-%m-%d")
    end

    def self.datetime_string
      Time.now.strftime("%Y-%m-%d %H:%M:%S")
    end

    def self.time_string
      Time.now.strftime("%H:%M:%S")
    end

    DICTIONARY = [*"a".."z"].freeze

    def self.string(length = nil)
      [*0..((length || 10) - 1)]
        .map{ |_n|
          DICTIONARY[rand(DICTIONARY.size)]
        }
        .join
    end

    def self.hex_string(length = nil)
      length ||= 10
      integer(("f" * length).hex - 1).to_s(16).rjust(length, "0")
    end

    def self.file_name_string(length = nil)
      length ||= 6
      "#{string(length)}.#{string(3)}"
    end

    def self.dir_path_string(length = nil)
      length ||= 12
      File.join(*string(length).scan(/.{1,4}/))
    end

    def self.file_path_string
      File.join(dir_path_string, file_name_string)
    end

    def self.url_string(host = nil, length = nil)
      File.join(host.to_s, dir_path_string(length))
    end

    def self.email_string(domain = nil, length = nil)
      domain ||= "#{string(5)}.com"
      "#{string(length)}@#{domain}"
    end

    def self.binary
      [integer(10000)].pack("N*")
    end
  end
end
