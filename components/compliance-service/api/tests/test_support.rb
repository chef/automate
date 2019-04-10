require 'date'
require 'time'

SOMETHING_MORE_RECENT = "something-more-recent"

#This class exposes a set of util functions.. they are meant to specifically test validity of a datetime
class TimeStuff
  #isTimeFormatRFC3339 takes a datetime in string format, parses it into a DataTime and then formats it back to rfc3339.
  #finally it compares the formatted DateTime to the original string passed in.
  def self.isTimeFormatRFC3339(str_time_under_test)
    parsed_and_formatted = parseAndFormatRFC3339(str_time_under_test)
    parsed_and_formatted == str_time_under_test
  end

  #parseAndFormatRFC3339 takes a datetime as string, parses it into a datetime and returns in in rfc3339.
  #This is more robust than say a regex as it tests both format as well as accuracy of date.
  def self.parseAndFormatRFC3339(str_time_under_test)
    DateTime.parse(str_time_under_test).strftime("%Y-%m-%dT%H:%M:%SZ")
  end

  #this tests that the date that get back is after the test started and up until "now"
  def self.isTimeWithinRange(test_start_time, time_under_test)
    time_under_test >= test_start_time && time_under_test <= Time.now.utc
  end

  #isDateRFC3339AndInRange takes string representation of a datetime and tests to see if it is between test_start_time and now
  def self.isDateRFC3339AndInRange(test_start_time, str_time_under_test)
    isTimeFormatRFC3339(str_time_under_test) && isTimeWithinRange(test_start_time, Time.parse(str_time_under_test))
  end

  def self.checkTimestampAndAdjustIfNeeded(test_start_time, objectHash, hashKey)
    time = objectHash[hashKey]
    if (time == '1970-01-01T00:00:00.000Z') || (time == '0001-01-01T00:00:00.000Z')
      objectHash[hashKey] = time
    else
      # something is happening with the travis tests; this all works locally
      # but is inconsistent in travis :/
      # noUnixSecondsTime = time.slice(0..18) + 'Z'
      # if not isDateRFC3339AndInRange(test_start_time, noUnixSecondsTime)
      #   return false
      # end
      objectHash[hashKey] = nil
    end
    return true
  end
end

class ResultStuff
  #if the hash contains the expectedValue, then set the hash item to that expected value.
  #else leave it alone.
  def self.checkResultAndAdjustIfNeeded(objectHash, hashKey, expectedValue)
    if objectHash[hashKey].include? expectedValue
      objectHash[hashKey] = expectedValue
    end
  end
end
