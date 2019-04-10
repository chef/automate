defmodule Notifications.Formatters.ServiceNow.Compliance.Test do
  use ExUnit.Case
  alias Notifications.Formatters.ServiceNow.Compliance
  doctest Compliance

  test "One compliance test failure, with end time and timestamp" do
    notification = %Notifications.ComplianceFailure{
        id: "",
        compliance_url: "https://automate.io",
        node_name: "some node name",
        node_id: "some-id",
        inspec_version: "0.0.0",
        end_time: "2018-06-06T19:30:02.000000Z",
        timestamp: "2018-06-05T19:30:02.000000Z",
        test_totals: %Notifications.ComplianceFailure.ControlTotals{
          skipped: 0,
          passed: 0,
          failed: 0,
          critical: 0,
          critical_failed: 0
        },
        failed_profiles: []
      }
     message = Compliance.format(notification)
     assert notification.timestamp == message.timestamp_utc
     assert notification.end_time == message.end_time_utc
     assert notification.node_id == message.node_uuid
     assert notification.compliance_url == message.automate_failure_url
     assert notification.node_name == message.node_name
     assert notification.inspec_version == message.inspec_version
     assert notification.test_totals.passed == message.total_number_of_passed_tests
     assert notification.test_totals.critical == message.number_of_critical_tests
     assert notification.test_totals.failed + notification.test_totals.passed + notification.test_totals.skipped == message.total_number_of_tests
     assert notification.test_totals.failed == message.total_number_of_failed_tests
     assert notification.test_totals.critical_failed == message.number_of_failed_critical_tests
     assert notification.test_totals.skipped == message.total_number_of_skipped_tests
  end

end
