defmodule Notifications.Formatters.ServiceNow do
  @moduledoc """
  Format a message payload for posting to a general ServiceNow
  """
  @behaviour Notifications.Formatters.Behavior

  alias Notifications.CCRFailure
  alias Notifications.Formatters.Utils

  @spec format(Notifications.notification) :: map()
  def format(%Notifications.ComplianceFailure{} = c) do
    Notifications.Formatters.ServiceNow.Compliance.format(c)
  end
  def format(%CCRFailure{} = notification) do
    exception = notification.exception
    failure_snippet = "Chef client run failure on [#{notification.node_url}] #{notification.node_name} : #{notification.run_url}\n" <>
      "#{exception.title}\n#{exception.msg} \n"
    %{
      automate_fqdn: Notifications.Config.automate_fqdn,
      exception_backtrace: exception.backtrace,
      exception_title: exception.title,
      exception_message: exception.msg,
      automate_failure_url: notification.run_url,
      cookbook: notification.cookbook,
      timestamp_utc: Utils.format_date_string(notification.timestamp),
      start_time_utc: Utils.format_date_string(notification.time.start_time),
      end_time_utc: Utils.format_date_string(notification.time.end_time),
      node_name: notification.node_name,
      type: "converge_failure",
      failure_snippet: failure_snippet,
    }
  end

end
