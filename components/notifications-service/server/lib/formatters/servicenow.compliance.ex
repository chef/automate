defmodule Notifications.Formatters.ServiceNow.Compliance do
  @moduledoc """
  Build a message map from the compliance struct to be supplied to the ServiceNow url
  """
  require Logger
  @behaviour Notifications.Formatters.Behavior

  alias Notifications.ComplianceFailure
  alias Notifications.Formatters.ComplianceHelper

  @spec format(ComplianceFailure.t):: map()
  def format(notification) do
    ComplianceHelper.get_servicenow_compliance_notification(notification, 0.1)
  end
end
