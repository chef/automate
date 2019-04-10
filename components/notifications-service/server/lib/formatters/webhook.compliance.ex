defmodule Notifications.Formatters.Webhook.Compliance do
  @moduledoc """
  Build a message map from the compliance struct to be supplied to the webook url
  """
  require Logger
  @behaviour Notifications.Formatters.Behavior

  alias Notifications.ComplianceFailure
  alias Notifications.Formatters.ComplianceHelper
  alias Notifications.Formatters.Utils

  #
  @spec format(ComplianceFailure.t):: map()
  def format(%ComplianceFailure{test_totals: totals} = notification) do
    Utils.to_map(notification)
    # For the most part we can just dump in the profiles - but we need to
    # remove passed tests first:
    notification = ComplianceHelper.prune_and_augment(notification)
    # The payload includes all the same data, but names and format are different.
    %{
      automate_fqdn: Notifications.Config.automate_fqdn,
      failure_snippet: "InSpec found a critical control failure on [#{notification.node_name}](#{notification.compliance_url})",
      automate_failure_url: notification.compliance_url,
      node_name: notification.node_name,
      node_uuid: notification.node_id,
      number_of_critical_tests: totals.critical,
      total_number_of_tests:   totals.failed + totals.passed + totals.skipped,
      total_number_of_failed_tests: totals.failed,
      number_of_failed_critical_tests: totals.critical_failed,
      total_number_of_passed_tests: totals.passed,
      total_number_of_skipped_tests: totals.skipped,
      inspec_version: notification.inspec_version,
      failed_critical_profiles: Utils.to_map(notification.failed_profiles),
      type: "compliance_failure"
    }
  end
end


