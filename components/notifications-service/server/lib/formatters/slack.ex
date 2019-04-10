defmodule Notifications.Formatters.Slack do
  @moduledoc """

  Generates a hash suitable for posting to Slack webhooks as json.
  """
  alias Notifications.CCRFailure
  alias Notifications.CCRSuccess
  alias Slack.Message
  alias Slack.Message.Attachment
  alias Slack.Message.Attachment.Field

  @behaviour Notifications.Formatters.Behavior
  require Logger

  @spec format(Notifications.notification) :: map()
  def format(%{__struct__: type} = n) when type in [CCRFailure, CCRSuccess] do
    payload = Message.new(
      username: "Chef Automate",
      icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
      attachments: [attachment_note(n)],
      text: alert_text(n)
    )
    Notifications.Formatters.Utils.to_map(payload)
  end
  def format(%{__struct__: type} = n) when type in [Notifications.ComplianceFailure] do
    Notifications.Formatters.Slack.Compliance.format(n)
  end

  # TODO - wording on these and below - converge? chef client run?
  #
  def alert_text(%CCRSuccess{} = n),
    do: "<#{n.run_url}|Chef client run successful on #{n.node_name}>\n"
  def alert_text(%CCRFailure{} = n),
    do: "<#{n.run_url}|Chef client run failure on #{n.node_name}>\n"

  @spec attachment_note(Notifications.notification) :: Attachment.t
  defp attachment_note(%CCRFailure{:exception => e} = n) do
    Attachment.new(
      fallback: "Chef client run failed on #{n.node_name}",
      text:     "#{e.title}\n```#{create_message(n)}```",
      fields:   fields_from_notification(n),
      color:    "warning",
      mrkdwn_in: ["text"]
    )
  end

  defp attachment_note(%CCRSuccess{} = n) do
    Attachment.new(
      fallback: "Chef client run succesesful on #{n.node_name}",
      pretext:  "",
      text:     "",
      color:    "good",
      fields:   fields_from_notification(n),
      mrkdwn_in: []
    )
  end

  defp fields_from_notification(%CCRSuccess{} = n) do
    [Field.new(title: "Node", value: "#{n.node_name}", short: false),
     Field.new(title: "Resources Updated", value: "#{n.updated_resource_count}", short: true)]
  end

  defp fields_from_notification(%CCRFailure{} = n) do
    [
      Field.new(title: "Node", value: "#{n.node_name}", short: false),
      Field.new(title: "Cookbook::Recipe", value: "#{n.cookbook}::#{n.recipe}", short: false)
    ]
  end

  @spec create_message(CCRFailure.t):: map()
  defp create_message(%CCRFailure{exception: %Notifications.ExceptionInfo{} = e} = n) do
    message =
      "Error: #{e.class}\n" <>
      "Message: #{e.msg}\n" <>
      "File: #{n.recipe}"

    case parse_line_number(e.msg) do
      nil -> message
      line_number -> message <> "\nLine: #{line_number}"
    end
  end

  @spec parse_line_number(String.t | nil):: String.t | nil
  defp parse_line_number(exception_message) when is_nil(exception_message), do: nil
  defp parse_line_number(exception_message) do
    case Regex.run(~r"line\s(\d+)", exception_message) do
      nil -> nil
      matches -> Enum.at(matches, 1, nil)
    end
  end
end
