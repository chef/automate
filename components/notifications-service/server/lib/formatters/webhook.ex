defmodule Notifications.Formatters.Webhook do
  @moduledoc """
  Format a message payload for posting to a general webhook
  """
  @behaviour Notifications.Formatters.Behavior

  alias Notifications.CCRFailure
  alias Notifications.Formatters.Utils

  @spec format(Notifications.notification) :: map()
  def format(%Notifications.ComplianceFailure{} = c) do
    Notifications.Formatters.Webhook.Compliance.format(c)
  end
  def format(%CCRFailure{} = notification) do
    # TODO - given we're just adding 'type' and 'failure_snippet', is there raelly any benefit
    # to doing additional translation here?
    exception = notification.exception
    failure_snippet = "Chef client run failure on [#{notification.node_url}] #{notification.node_name} : #{notification.run_url}\n" <>
      "#{exception.title}\n#{exception.msg} \n"
    %{
      # TODO - is this a necessary part of the payload? This is the only
      # element in
      automate_fqdn: Notifications.Config.automate_fqdn,
      exception_backtrace: exception.backtrace,
      exception_title: exception.title,
      exception_message: exception.msg,
      automate_failure_url: notification.run_url,

      # TODO - this is not avaialble and doesn't really add anything over
      # having the start and end time - the timestamp of _what_?
      # timestamp_utc: notification.timestamp_utc,

      # TODO confirm start-end time - are they UTC?
      start_time_utc: Utils.format_date_string(notification.time.start_time),
      end_time_utc: Utils.format_date_string(notification.time.end_time),
      node_name: notification.node_name,
      type: "converge_failure",
      failure_snippet: failure_snippet,
    }
  end

end
