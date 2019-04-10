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
      icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
      text: msg,
      attachments: [attachment_note(c)],
    }
  end


  defp attachment_note(%ComplianceFailure{} = compliance) do
    failed_profiles = compliance.failed_profiles
    crit_failed_controls = failed_critical_controls(failed_profiles)

    {profile_name, control_value} = collect_profile_and_control_names(failed_profiles, crit_failed_controls)

    message = message_from_failed_controls(crit_failed_controls, compliance
                                           .test_totals.critical)

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

  # If only one test fails the message from that test is used.
  # If multiple, it's  "failed_count of total_count tests failed"
  def all_failed_tests(failed_controls) do
    failures_filter = fn(control) ->
                        Enum.filter(control.failed_results,
                                    &Utils.failed_test?(&1))
    end

    Enum.flat_map(failed_controls, failures_filter)
  end

  # This evaluates the invidivual failed tests within the provided controls
  # and extracts a message.
  def message_from_failed_controls(failed_controls, total_tests) do
    case all_failed_tests(failed_controls) do
      [] -> ""                # No failed tests
      [test] -> test.message  # Exactly one failed test
      failed_tests ->         # multiple failed tests
        count = Enum.count(failed_tests)
        # TODO: #{count} of {total_tests} failed across {controls} controls.
        "#{count} of #{total_tests} tests failed. View in Chef Automate for full details."
    end
  end
end
