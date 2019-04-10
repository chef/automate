defmodule Notifications.Formatters.Slack.Test do
  use ExUnit.Case, async: false
  doctest Notifications.Formatters.Slack

  alias Notifications.Formatters.Slack

  test "Ensure the node built from example json creates correct slack map" do
    ccr_notification = TestHelper.load_failure("converge-failure-report")
    expected = %{
        attachments: [%{
            color: "warning",
            fallback: "Chef client run failed on insights.chef.co",
            fields: [%{
                short: false,
                title: "Node",
                value: "insights.chef.co"
            }, %{
                short: false,
                title: "Cookbook::Recipe",
                value: "insights-test::not_default"
            }],
            pretext: "",
            mrkdwn_in: ["text"],
          text: String.trim("""
          Error executing action `create` on resource 'file[/failed/file/resource]'
          ```Error: Chef::Exceptions::EnclosingDirectoryDoesNotExist
          Message: file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist.
          File: not_default
          Line: 26```
          """)
        }],
        icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
        text: "<https://localhost/nodes/0271e125-97dd-498a-b026-8448ee60aafe/runs/ba6acb91-1eaa-4c84-8d68-f19ee641e606|Chef client run failure on insights.chef.co>\n",
        username: "Chef Automate"
    }

    assert expected == Slack.format(ccr_notification)
  end
end
