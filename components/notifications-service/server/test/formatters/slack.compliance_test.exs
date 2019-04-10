defmodule Notifications.Formatters.Slack.Compliance.Test do
  use ExUnit.Case, async: false

  test "One compliance test failure creates map with test error message, control title, and profile title" do
    failure = TestHelper.load_failure("inspec-report-single-failure")
    expected = %{
        attachments: [%{
            color: "warning",
            fallback: "InSpec critical control failure on node pretty-chipper-node.\nexpected File /etc/ssh not to be readable by others",
            fields: [%{
                short: false,
                title: "Control ID::Title",
                value: "basic-3::/etc/ssh should have limited access to 0755"
            }, %{
                short: false,
                title: "Profile",
                value: "Basic SSH"
            }, %{
                short: false,
                title: "Node",
                value: "pretty-chipper-node"
            }],
            mrkdwn_in: ["text", "pretext"],
            text: "```expected File /etc/ssh not to be readable by others```\n"
        }],
        icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
        text: "<https://localhost/compliance/reporting/nodes/deadbeef05|InSpec found a critical control failure on node pretty-chipper-node>",
        username: "Chef Automate"
    }
    assert expected == Notifications.Formatters.Slack.Compliance.format(failure)
  end

  test "Two compliance test failures in same control creates map with number of tests failed, control title, and profile title" do
    failure = TestHelper.load_failure("inspec-report-multiple-failures-one-control")
    expected = %{
        attachments: [%{
            color: "warning",
            fallback: "InSpec critical control failure on node unhappy-node.\n2 of 3 tests failed. View in Chef Automate for full details.",
            fields: [%{
                short: false,
                title: "Control ID::Title",
                value: "basic-3::/etc/ssh should have limited access to 0755"
            }, %{
                short: false,
                title: "Profile",
                value: "Basic SSH"
            }, %{
                short: false,
                title: "Node",
                value: "unhappy-node"
            }],
            mrkdwn_in: ["text", "pretext"],
            text: "```2 of 3 tests failed. View in Chef Automate for full details.```\n"
        }],
        icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
        text: "<https://localhost/compliance/reporting/nodes/deadbeef04|InSpec found a critical control failure on node unhappy-node>",
        username: "Chef Automate"
    }
    assert expected == Notifications.Formatters.Slack.Compliance.format(failure)
  end

  test "Two compliance test failures in different controls creates map with number of tests failed, control with Multiple, and profile title" do
    failure = TestHelper.load_failure("inspec-report-multiple-failure")
    expected = %{
        attachments: [%{
            color: "warning",
            fallback: "InSpec critical control failure on node sad-node.\n2 of 3 tests failed. View in Chef Automate for full details.",
            fields: [%{
                short: false,
                title: "Control ID::Title",
                value: "Multiple"
            }, %{
                short: false,
                title: "Profile",
                value: "Basic SSH"
            }, %{
                short: false,
                title: "Node",
                value: "sad-node"
            }],
            mrkdwn_in: ["text", "pretext"],
            text: "```2 of 3 tests failed. View in Chef Automate for full details.```\n"
        }],
        icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
        text: "<https://localhost/compliance/reporting/nodes/deadbeef03|InSpec found a critical control failure on node sad-node>",
        username: "Chef Automate"
    }
      assert expected == Notifications.Formatters.Slack.Compliance.format(failure)
  end

  test "More than one test failure across different profiles describes control and profile as 'multiple'" do
    failure = TestHelper.load_failure("inspec-report")
    expected = %{
        attachments: [%{
            color: "warning",
            fallback: "InSpec critical control failure on node failing-node.\n97 of 174 tests failed. View in Chef Automate for full details.",
            fields: [%{
                short: false,
                title: "Control ID::Title",
                value: "Multiple"
            }, %{
                short: false,
                title: "Profile",
                value: "Multiple"
            }, %{
                short: false,
                title: "Node",
                value: "failing-node"
            }],
            mrkdwn_in: ["text", "pretext"],
            text: "```97 of 174 tests failed. View in Chef Automate for full details.```\n"
        }],
        icon_url: "https://docs.chef.io/_static/chef_logo_v2.png",
        text: "<https://localhost/compliance/reporting/nodes/deadbeef06|InSpec found a critical control failure on node failing-node>",
        username: "Chef Automate"
    }
    assert expected == Notifications.Formatters.Slack.Compliance.format(failure)
  end
end
