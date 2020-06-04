defmodule Notifications.Formatters.Webhook.Compliance.Test do
  use ExUnit.Case
  alias Notifications.Formatters.Webhook.Compliance
  alias Notifications.ComplianceFailure
  alias Notifications.Profile
  alias Notifications.ComplianceFailure.ControlTotals
  doctest Compliance

  test "One compliance test failure, create correct map" do
    notification = TestHelper.load_failure("inspec-report-single-failure")
    expected = %{
      failure_snippet: "InSpec found a critical control failure on [pretty-chipper-node](https://localhost/compliance/reporting/nodes/deadbeef05)",
      automate_fqdn: "http://localhost",
      automate_failure_url: "https://localhost/compliance/reporting/nodes/deadbeef05",
      failed_critical_profiles: [
        %{attributes: [],
          copyright: "Chef Software, Inc.",
          copyright_email: "support@chef.io",
          controls: [
            %{code: "control 'basic-3' do\n  impact 0.5\n  title '/etc/ssh should have limited access to 0755'\n  desc '\n    The OpenSSH configuration folder should have restricted\n    access permissions. It is an important step towards making\n    sure, that configuration files are only changed by\n    priviledged users, while leaving them readable for the\n    SSH client.\n  '\n  describe file('/etc/ssh') do\n    it { should_not be_writable.by('group') }\n    it { should_not be_writable.by('others') }\n    it { should_not be_readable.by('others') }\n  end\nend\n",
              desc: "The OpenSSH configuration folder should have restricted\naccess permissions. It is an important step towards making\nsure, that configuration files are only changed by\npriviledged users, while leaving them readable for the\nSSH client.",
              results: [
                %{code_desc: "File /etc/ssh should not be readable by others",
                  message: "expected File /etc/ssh not to be readable by others",
                  run_time: 0.00342399999499321,
                  skip_message: "",
                  start_time: "2017-07-05 14:08:05 +0200",
                  status: "failed"}
              ],
              id: "basic-3", impact: 0.8999999761581421, number_of_failed_tests: 1,
              source_location: %{line: 37, ref: "base/ssh/controls/ssh_folder_spec.rb"},
              status: "failed",
              number_of_tests: 3,
              title: "/etc/ssh should have limited access to 0755",
              removed_results_counts: nil,
              refs: []}
          ],
          license: "Proprietary, All rights reserved",
          maintainer: "Chef Software, Inc.",
          name: "ssh",
          number_of_controls: 2,
          sha256: "f56ef31e9e8be7c9064e84f1db699d3f0175924a0ee4517e3a626c38727ebf93",
          summary: "Verify that SSH Server and SSH Client are configured securely",
          supports: [%{inspec: "", os_family: "", os_name: "", release: ""}],
          title: "Basic SSH", version: "1.1.0"
        }
      ],
      inspec_version: "1.30.0",
      node_name: "pretty-chipper-node",
      node_uuid: "deadbeef05",
      number_of_critical_tests: 2,
      number_of_failed_critical_tests: 1,
      total_number_of_failed_tests: 1,
      total_number_of_passed_tests: 1,
      total_number_of_skipped_tests: 0,
      total_number_of_tests: 2,
      type: "compliance_failure"
    }
    assert expected == Compliance.format(notification)
  end

  test "mixed critical and not critical failures include only critical in the map" do
    failure = TestHelper.load_failure("inspec-mixed")
    expected =
      %{automate_failure_url: "https://localhost/compliance/reporting/nodes/deadbeef02",
        automate_fqdn: "http://localhost",
        failed_critical_profiles: [%{
          attributes: [],
          controls: [
            %{code: "control 'basic-3' do\n  impact 0.5\n  title '/etc/ssh should have limited access to 0755'\n  desc '\n    The OpenSSH configuration folder should have restricted\n    access permissions. It is an important step towards making\n    sure, that configuration files are only changed by\n    priviledged users, while leaving them readable for the\n    SSH client.\n  '\n  describe file('/etc/ssh') do\n    it { should_not be_writable.by('group') }\n    it { should_not be_writable.by('others') }\n    it { should_not be_readable.by('others') }\n  end\nend\n",
              desc: "The OpenSSH configuration folder should have restricted\naccess permissions. It is an important step towards making\nsure, that configuration files are only changed by\npriviledged users, while leaving them readable for the\nSSH client.",
              id: "basic-3",
              impact: 0.8999999761581421,
              number_of_failed_tests: 1,
              number_of_tests: 3,
              results: [
                %{code_desc: "File /etc/ssh should not be readable by others",
                  message: "expected File /etc/ssh not to be readable by others",
                  run_time: 0.00342399999499321,
                  skip_message: "",
                  start_time: "2017-07-05 14:08:05 +0200",
                  status: "failed"},
              ],
              source_location: %{line: 37, ref: "base/ssh/controls/ssh_folder_spec.rb"},
              status: "failed", title: "/etc/ssh should have limited access to 0755",
              refs: [],
              removed_results_counts: nil,
            }
          ],
          copyright: "Chef Software, Inc.",
          copyright_email: "support@chef.io",
          license: "Proprietary, All rights reserved",
          maintainer: "Chef Software, Inc.",
          name: "ssh 2",
          number_of_controls: 1,
          sha256: "f56ef31e9e8be7c9064e84f1db699d3f0175924a0ee4517e3a626c38727ebf93",
          summary: "Verify that SSH Server and SSH Client are configured securely",
          supports: [%{inspec: "", os_family: "", os_name: "", release: ""}],
          title: "Basic SSH 2" ,
          version: "1.1.0"
        }],
        failure_snippet: "InSpec found a critical control failure on [bleh-node](https://localhost/compliance/reporting/nodes/deadbeef02)",
        inspec_version: "1.30.0",
        node_name: "bleh-node",
        node_uuid: "deadbeef02",
        number_of_critical_tests: 3,
        number_of_failed_critical_tests: 1,
        total_number_of_failed_tests: 2,
        total_number_of_passed_tests: 1,
        total_number_of_skipped_tests: 1,
        total_number_of_tests: 4,
        type: "compliance_failure"
      }
    assert expected == Compliance.format(failure)
  end
  test "multiple critical control failures in a single profile are included n the payload" do
    failure = TestHelper.load_failure("inspec-report-multiple-failure")
    expected =
      %{automate_failure_url: "https://localhost/compliance/reporting/nodes/deadbeef03",
        automate_fqdn: "http://localhost",
        failed_critical_profiles: [%{attributes: [],
          controls: [
            %{code: "control 'basic-3' do\n  impact 0.5\n  title '/etc/ssh should have limited access to 0755'\n  desc '\n    The OpenSSH configuration folder should have restricted\n    access permissions. It is an important step towards making\n    sure, that configuration files are only changed by\n    priviledged users, while leaving them readable for the\n    SSH client.\n  '\n  describe file('/etc/ssh') do\n    it { should_not be_writable.by('group') }\n    it { should_not be_writable.by('others') }\n    it { should_not be_readable.by('others') }\n  end\nend\n",
              desc: "The OpenSSH configuration folder should have restricted\naccess permissions. It is an important step towards making\nsure, that configuration files are only changed by\npriviledged users, while leaving them readable for the\nSSH client.",
              id: "basic-3",
              impact: 0.8999999761581421,
              number_of_failed_tests: 1,
              number_of_tests: 3,
              results: [
                %{code_desc: "File /etc/ssh should not be readable by others",
                  message: "expected File /etc/ssh not to be readable by others",
                  run_time: 0.00342399999499321,
                  skip_message: "",
                  start_time: "2017-07-05 14:08:05 +0200",
                  status: "failed"}
              ],
              source_location: %{line: 37, ref: "base/ssh/controls/ssh_folder_spec.rb"},
              status: "failed", title: "/etc/ssh should have limited access to 0755",
              refs: [],
              removed_results_counts: nil,
            },
            %{code: "control 'basic-2' do\n  impact 1.0\n  title '/etc/ssh should be owned by root'\n  desc '\n    The OpenSSH configuration folder should be owned\n    by the root user. It is an important step towards making\n    sure, that configuration files are only changed by\n    priviledged users.\n  '\n  describe file('/etc/ssh') do\n    it { should be_owned_by 'root' }\n  end\nend\n",
              desc: "The OpenSSH configuration folder should be owned\nby the root user. It is an important step towards making\nsure, that configuration files are only changed by\npriviledged users.",
              id: "basic-2",
              impact: 1.0,
              number_of_failed_tests: 1,
              number_of_tests: 1,
              results: [
                %{code_desc: "File /etc/ssh should be owned by \"root\"",
                  message: "",
                  run_time: 0.026845000684261322,
                  skip_message: "",
                  start_time: "2017-07-05 14:08:05 +0200",
                  status: "failed"}
              ],
              source_location: %{line: 23, ref: "base/ssh/controls/ssh_folder_spec.rb"},
              status: "failed",
              title: "/etc/ssh should be owned by root",
              refs: [],
              removed_results_counts: nil,
            }
          ],
          copyright: "Chef Software, Inc.", copyright_email: "support@chef.io",
          license: "Proprietary, All rights reserved",
          maintainer: "Chef Software, Inc.", name: "ssh", number_of_controls: 3,
          sha256: "f56ef31e9e8be7c9064e84f1db699d3f0175924a0ee4517e3a626c38727ebf93",
          summary: "Verify that SSH Server and SSH Client are configured securely",
          supports: [%{inspec: "", os_family: "", os_name: "", release: ""}],
          title: "Basic SSH", version: "1.1.0"}],
          failure_snippet: "InSpec found a critical control failure on [sad-node](https://localhost/compliance/reporting/nodes/deadbeef03)",
          inspec_version: "1.30.0",
          node_name: "sad-node",
          node_uuid: "deadbeef03",
          number_of_critical_tests: 3,
          number_of_failed_critical_tests: 2,
          total_number_of_failed_tests: 2,
          total_number_of_passed_tests: 1,
          total_number_of_skipped_tests: 0,
          total_number_of_tests: 3,
          type: "compliance_failure"
      }
    assert expected == Compliance.format(failure)
  end

  test "removed results counts in control" do
    failure = %ComplianceFailure{id: "id",
      compliance_url: "https://localhost/compliance/reporting/nodes/ubuntu1604",
      node_name: "",
      node_id: "",
      inspec_version: "",
      end_time: "",
      timestamp: "",
      test_totals: %ControlTotals{failed: 1,
        skipped: 0,
        passed: 2,
        critical: 3,
        critical_failed: 1
      },
      failed_profiles: [
        %Profile{
          name: "",
          title: "",
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
                  status: "",
                  code_desc: "",
                  run_time: 1.9,
                  start_time: "",
                  message: "",
                  skip_message: ""
                }
              ],
              stats: %Notifications.Profile.Control.ResultTotals{
                num_tests: 225,
                num_failed_tests: 100,
                num_skipped_tests: 50,
                num_passed_tests: 75
              },
              removed_results_counts: %Notifications.Profile.Control.RemovedResultsCounts{
                failed: 100,
                skipped: 50,
                passed: 75
              }
            }
          ],
          stats: %Notifications.Profile.ControlTotals{
            num_tests: 3,
            num_failed_tests: 1,
            num_skipped_tests: 0,
            num_passed_tests: 2
          } 
        }
      ]
    }

    message = Compliance.format(failure)
    failedProfile = Enum.at(message.failed_critical_profiles, 0)
    failedControl = Enum.at(failedProfile.controls, 0)

    assert 100 == failedControl.removed_results_counts.failed
    assert 50 == failedControl.removed_results_counts.skipped
    assert 75 == failedControl.removed_results_counts.passed
  end

end
