defmodule Notifications.Formatters.UtilsTest do
  use ExUnit.Case

  alias Notifications.Formatters.Utils

  describe "truncate_slack_message/1" do
    test "when string is less than 1500 characters it is returned as-is" do
      expected = String.duplicate("a", 1499)
      assert Utils.truncate_slack_message(expected) == expected
    end
    test "when value is nil it is returned as is" do
      assert Utils.truncate_slack_message(nil) == nil
    end
    test "when value is exactly 1500 characters it gets '...' appended" do
      original = String.duplicate("a", 1500)
      expected = original <> "..."
      assert Utils.truncate_slack_message(original) == expected
    end
    test "when value is more than 1500 charactters it gets truncated and has '...' appended" do
      original = String.duplicate("a", 1501)
      expected = String.duplicate("a", 1500) <> "..."
      assert Utils.truncate_slack_message(original) == expected
    end
    test "when value is way more than 1500 charactters it gets truncated and has '...' appended" do
      original = String.duplicate("a", 15000)
      expected = String.duplicate("a", 1500) <> "..."
      assert Utils.truncate_slack_message(original) == expected
    end
  end

  describe "format_date_string/1" do
    test "when date is nil return nil" do
      assert Utils.format_date_string(nil) == ""
    end

    test "when date is blank return blank" do
      assert Utils.format_date_string("") == ""
    end

    test "when date is properly formatted return the correct padded string" do
       assert Utils.format_date_string("2017-06-05T21:11:52.000000Z") == "2017-06-05T21:11:52.000000Z"
    end

    test "when date is not in UTC return the date in the correct UTC format" do
       assert Utils.format_date_string("2017-06-05T15:11:52-06:00") == "2017-06-05T21:11:52.000000Z"
    end

    test "when date does not have enough seconds digits the digits will be added" do
       assert Utils.format_date_string("2017-06-05T21:11:54.000Z") == "2017-06-05T21:11:54.000000Z"
    end

    test "When the date has values for microseconds the microseconds should be returned" do
       assert Utils.format_date_string("2017-06-05T21:11:52.123456Z") == "2017-06-05T21:11:52.123456Z"
    end
  end
end
