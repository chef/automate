defmodule Notifications.Formatters.Slack.Compliance do
  @moduledoc """
  Generates a map from the compliance struct suitable for posting to Slack webhooks as json.
  """
  @behaviour Notifications.Formatters.Behavior
  alias Notifications.ComplianceFailure
  alias Notifications.Formatters.Utils
  # TODO - the top level envelop bvelongs in slack formatter - it can call into the module
  # for attachment_note elements.
  @spec format(ComplianceFailure.t):: map()
  def format(%ComplianceFailure{} = c) do
    msg = Utils.maybe_markdown_url(c.compliance_url,
                                    "InSpec found a critical control failure on node #{c.node_name}")
    %{
      username: "Chef Automate",
      icon_url: "https://docs.chef.io/images/chef-icon.png",
      text: msg,
      attachments: [attachment_note(c)],
    }
  end


  defp attachment_note(%ComplianceFailure{} = compliance) do
    failed_profiles = compliance.failed_profiles
    crit_failed_controls = failed_critical_controls(failed_profiles)

    {profile_name, control_value} = collect_profile_and_control_names(failed_profiles, crit_failed_controls)

    message = message_from_failed_controls(crit_failed_controls)

    truncated_message = Utils.truncate_slack_message(message)
    fallback = "InSpec critical control failure on node #{compliance.node_name}."
    %{
      fallback: fallback <> "\n" <> truncated_message,
      text:     "#{maybe_markdown_preformat(truncated_message)}\n",
      color:    "warning",
      fields:   [
        %{
          title: "Control ID::Title",
          value: "#{control_value}",
          short: false
        },
        %{
          title: "Profile",
          value: "#{profile_name}",
          short: false
        },
        %{
          title: "Node",
          value: "#{compliance.node_name}",
          short: false
        }
      ],
      mrkdwn_in: ["text", "pretext"]
    }
  end
  # Avoids sending `````` when msg is blank -- which slack does not format well.
  defp maybe_markdown_preformat(""), do: ""
  defp maybe_markdown_preformat(msg), do: "```#{msg}```"

  # TODO - prefiltering must be in place to ensure we never see 0 critical failed
  defp collect_profile_and_control_names(profiles, failed_controls) do
    if Enum.count(profiles) == 1 do
      profile = hd(profiles)
      cv = if Enum.count(failed_controls) == 1 do
        failed_control = hd(failed_controls)
        "#{failed_control.id}::#{failed_control.title}"
       else
         "Multiple"
       end
      {profile.title, cv}
    else
      {"Multiple", "Multiple"}
    end
  end


  def failed_critical_controls(failed_profiles) do
      Enum.flat_map(failed_profiles,
                    fn(profile) ->
                      Enum.filter(profile.failed_controls, &Utils.failed_critical_control?(&1))
                    end)
  end

  # This evaluates the invidivual failed tests within the provided controls
  # and extracts a message.
  def message_from_failed_controls(failed_controls) do
    case Enum.reduce(failed_controls, 0,
      fn control, total -> total + control.stats.num_failed_tests end)  do
      0 -> "" # No failed tests
      1 -> # Exactly one failed test return that test's message
        failed_controls
          |> Enum.flat_map(fn control -> control.failed_results end)
          |> Enum.reduce("", fn result, message -> result.message end)
      total_failed_tests ->   # multiple failed tests
        total_tests = Enum.reduce(failed_controls, 0, fn control, total -> total + control.stats.num_tests end)
        "#{total_failed_tests} of #{total_tests} tests failed. View in Chef Automate for full details."
    end
  end
end
