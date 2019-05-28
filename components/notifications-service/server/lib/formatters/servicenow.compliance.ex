defmodule Notifications.Formatters.ServiceNow.Compliance do
  @moduledoc """
  Build a message map from the compliance struct to be supplied to the ServiceNow url
  """
  require Logger
  @behaviour Notifications.Formatters.Behavior

  alias Notifications.ComplianceFailure
  alias Notifications.Formatters.ComplianceHelper
  alias Notifications.Formatters.Utils

  @crit_control_threshold 0.7

  @spec format(ComplianceFailure.t):: map()
  def format(notification) do
    get_servicenow_compliance_notification(notification, 0.1)
  end

  def format_critical(notification) do
    get_servicenow_compliance_notification(notification)
  end

  def get_servicenow_compliance_notification(notification) do
    get_servicenow_compliance_notification(notification, @crit_control_threshold)
  end
  def get_servicenow_compliance_notification(%ComplianceFailure{test_totals: totals} = notification, control_threshold) do
    Utils.to_map(notification)

    notification = ComplianceHelper.prune_and_augment(notification, control_threshold)
    %{
      automate_fqdn: Notifications.Config.automate_fqdn,
      failure_snippet: "InSpec found a control failure on [#{notification.node_name}](#{notification.compliance_url})",
      automate_failure_url: notification.compliance_url,
      node_name: notification.node_name,
      node_uuid: notification.node_id,
      number_of_critical_tests: totals.critical,
      total_number_of_tests: totals.failed + totals.passed + totals.skipped,
      total_number_of_failed_tests: totals.failed,
      number_of_failed_critical_tests: totals.critical_failed,
      total_number_of_passed_tests: totals.passed,
      total_number_of_skipped_tests: totals.skipped,
      inspec_version: notification.inspec_version,
      failed_critical_profiles: Utils.to_map(notification.failed_profiles),
      timestamp_utc: Utils.format_date_string(notification.timestamp),
      end_time_utc: Utils.format_date_string(notification.end_time),
      type: "compliance_failure"
    }
  end
end
