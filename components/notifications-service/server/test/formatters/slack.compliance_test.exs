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
        icon_url: "https://docs.chef.io/images/chef-icon.png",
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
        icon_url: "https://docs.chef.io/images/chef-icon.png",
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
            fallback: "InSpec critical control failure on node sad-node.\n2 of 4 tests failed. View in Chef Automate for full details.",
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
            text: "```2 of 4 tests failed. View in Chef Automate for full details.```\n"
        }],
        icon_url: "https://docs.chef.io/images/chef-icon.png",
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
            fallback: "InSpec critical control failure on node failing-node.\n97 of 122 tests failed. View in Chef Automate for full details.",
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
            text: "```97 of 122 tests failed. View in Chef Automate for full details.```\n"
        }],
        icon_url: "https://docs.chef.io/images/chef-icon.png",
        text: "<https://localhost/compliance/reporting/nodes/deadbeef06|InSpec found a critical control failure on node failing-node>",
        username: "Chef Automate"
    }
    assert expected == Notifications.Formatters.Slack.Compliance.format(failure)
  end

  test "count tests from the control stats struct" do
    failure = %Notifications.ComplianceFailure{id: "id",
      compliance_url: "https://localhost/compliance/reporting/nodes/ubuntu1604",
      node_name: "side_walk",
      node_id: "",
      inspec_version: "",
      end_time: "",
      timestamp: "",
      test_totals: %Notifications.ComplianceFailure.ControlTotals{failed: 1,
        skipped: 0,
        passed: 0,
        critical: 1,
        critical_failed: 1
      },
      failed_profiles: [
        %Notifications.Profile{
          name: "profile",
          title: "profile title",
          version: "",
          summary: "",
          maintainer: "",
          license: "",
          copyright: "",
          copyright_email: "",
          sha256: "",
          # supports: ,
          attributes: [],
          failed_controls: [
            %Notifications.Profile.Control{
              id: "id",
              impact: 1,
              title: "title",
              code: "code",
              desc: "desc",
              # source_location: "",
              refs: [],
              failed_results: [
                %Notifications.Profile.Control.Result{
                  status: "failed",
                  code_desc: "",
                  run_time: 1.9,
                  start_time: "",
                  message: "test result message 1",
                  skip_message: ""
                },
                %Notifications.Profile.Control.Result{
                  status: "failed",
                  code_desc: "",
                  run_time: 1.9,
                  start_time: "",
                  message: "test result message 2",
                  skip_message: ""
                }
              ],
              stats: %Notifications.Profile.Control.ResultTotals{
                num_tests: 225,
                num_failed_tests: 100,
                num_skipped_tests: 50,
                num_passed_tests: 75
              },
            }
          ],
          stats: %Notifications.Profile.ControlTotals{
            num_tests: 1,
            num_failed_tests: 1,
            num_skipped_tests: 0,
            num_passed_tests: 0
          } 
        }
      ]
    }

    message = Notifications.Formatters.Slack.Compliance.format(failure)

    assert 1 == Enum.count(message.attachments)

    attachment = Enum.at(message.attachments, 0)

    assert "InSpec critical control failure on node side_walk.\n100 of 225 tests failed. View in Chef Automate for full details." == attachment.fallback
    assert "```100 of 225 tests failed. View in Chef Automate for full details.```\n" == attachment.text
  end
end
