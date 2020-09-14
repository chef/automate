##### GRPC SETUP #####
require 'interservice/compliance/reporting/reporting_pb'
require 'interservice/compliance/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "works" do
    ##### Failure tests #####
    assert_grpc_error("Parameter \'type\' not specified", 3) do
      GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new()
    end

    assert_grpc_error("Invalid suggestion type \'bobby\'", 3) do
      GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(type: 'bobby')
    end

    ##### Success tests #####

    # suggest environment, partial and case insensitive text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment', text: 'bet'
    )
    assert_suggestions_text(["DevSec Prod beta"], actual_data)

    # suggest environment, lower case text with space
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment', text: 'devsec zeT', size: 10
    )
    assert_suggestions_text(["DevSec Prod Zeta", "DevSec Prod Alpha", "DevSec Prod beta", "DevSec Prod Omega"], actual_data)

    # suggest environment, text with space and size limit
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment', text: 'devsec ze', size: 1
    )
    assert_suggestions_text(["DevSec Prod Zeta"], actual_data)


    # suggest environment, text too short, get all back
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment', text: 'x'
    )
    assert_suggestions_text(["DevSec Prod Alpha", "DevSec Prod beta", "DevSec Prod Omega", "DevSec Prod Zeta"], actual_data)

    # suggest environment, no text given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment'
    )
    assert_suggestions_text(["DevSec Prod Alpha", "DevSec Prod beta", "DevSec Prod Omega", "DevSec Prod Zeta"], actual_data)

    # suggest platform, partial match given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'platform', text: 'Cent'
    )
    assert_suggestions_text(["centos"], actual_data)

    # suggest platform, no text given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'platform'
    )
    assert_suggestions_text(["centos", "debian", "mac_os_x", "redhat", "ubuntu", "unknown", "windows"], actual_data)

    # suggest platform, match given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'platform', text: 'debian'
    )
    assert_suggestions_text(["debian"], actual_data)

    # suggest node names, text given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'node', text: 'beta'
    )
    assert_suggestions_text_id_version( [
      "RedHat(2)-beta-nginx(f)-apache(s)-failed--9b9f4e51-b049-4b10-9555-10578916e222--",
      "centos-beta--9b9f4e51-b049-4b10-9555-10578916e149--"
    ], actual_data )

    # suggest profiles, no text given
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'profile'
    )
    expected = ["A fake one--41a02797bfea15592ba2748d55929d8d1f9da205816ef18d3bb2ebe4c5ce18a9--2.0.1",
      "DevSec Apache Baseline--41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9--2.0.1",
      "DevSec Apache Baseline--41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8--2.0.0",
      "DevSec Linux Security Baseline--b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015--2.0.1",
      "DevSec Nginx Baseline--09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988--2.1.0",
      "My Faily Profile title--5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36888--1.0.0",
      "My Profile 1 title--447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea4--1.0.1",
      "My Profile 2 title--447542ecfb8a8800ed0146039da3af8fed047f575f6037cfba75f3b664a97ea5--1.0.5",
      "My Profile title--5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b367da--1.0.0",
      "My Skipped Profile title--5596bb07ef4f11fd2e03a0a80c4adb7c61fc0b4d0aa6c1410b3c715c94b36777--1.0.0"]
    assert_suggestions_text_id_version(expected, actual_data)

    # suggest profile, text with space and size limit
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'profile', text: 'devsec apa', size: 3
    )
    expected = [
      "DevSec Apache Baseline--41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8--2.0.0",
      "DevSec Apache Baseline--41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9--2.0.1",
      "DevSec Linux Security Baseline--b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015--2.0.1" ]
    assert_suggestions_text_id_version(expected, actual_data)

    # suggest all controls
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control', size: 10
    )
    expected =
        [
            "Apache should be enabled--apache-02--",
            "Apache should be running--apache-01--",
            "Apache should start max. 1 root-task--apache-03--",
            "Check Apache config file owner, group and permissions.--apache-05--",
            "Check Apache config folder owner, group and permissions.--apache-04--",
            "Disable Apache’s follows Symbolic Links for directories in alias.conf--apache-11--",
            "Disable Directory Listing for directories in alias.conf--apache-12--",
            "Disable insecure HTTP-methods--apache-10--",
            "Disable TRACE-methods--apache-09--",
            "Enable Apache Logging--apache-14--"
        ]
    assert_suggestions_text_id_version(expected, actual_data)

    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control', text: 'icmp', size: 3
    )
    expected = [
        "ICMP echo ignore broadcasts--sysctl-04--",
        "ICMP ignore bogus error responses--sysctl-03--",
        "ICMP ratelimit--sysctl-05--"
    ]
    assert_suggestions_text_id_version(expected, actual_data)

    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control', text: 'ApAcHe-is-awesome', size: 3
    )
    expected = []
    assert_suggestions_text_id_version(expected, actual_data)

    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'role', size: 2
    )
    assert_suggestions_text(["dot.role", "apache_linux"], actual_data)

    # results will be same as no text until you get to 2"chars
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'role', size: 2, text: 'd'
    )
    assert_suggestions_text(["dot.role", "apache_linux"], actual_data)

    # Having trouble with consistency of scored results on these tests, so checking
    # only for text value, since that is of greater importance
    res = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'role', text: 'apache'
    )
    actual = res['suggestions'].flat_map {|s| s['text']}.sort
    assert_equal(["apache_deb", "apache_linux", "apache_osx", "apache_windows"], actual)

    # res = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
    #   type: 'role', text: 'Apache Linux', size: 4
    # )
    # actual = res['suggestions'].flat_map {|s| s['text']}.sort
    # assert_equal(["apache_linux", "apache_osx", "apache_windows", "base_linux"], actual)

    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'recipe', text: 'apache_extras::harden'
    )
    expected = [
      "apache_extras::harden",
      "apache_extras::windows_harden",
      "apache_extras",
    ]
    assert_suggestions_text(expected, actual_data)


    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'recipe', size: 2, text: 'Hard Life'
    )
    assert_suggestions_text(["apache_extras::harden", "apache_extras::windows_harden"], actual_data)

    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'recipe', text: 'j'
    )
    expected = [
      "apache_extras",
      "apache_extras::harden",
      "java::default",
      "nagios::fix",
      "apache_extras::windows_harden",
    ]
    assert_suggestions_text(expected, actual_data)
  end

  it "list_suggestions controls, no date, 24h stuff" do
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
        type: 'control',
        text: 'Profile 2 - Control 1',
        size: 5,
        filters: [
            Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-01T00:00:00Z']),
            Reporting::ListFilter.new(type: 'end_time', values: ['2018-06-06T23:59:59Z'])
        ]
    )
    expected = [
        "Profile 1 - Control 1--pro1-con1--",
        "Profile 1 - Control 2--pro1-con2--",
        "Profile 1 - Control 3--pro1-con3--",
        "Profile 1 - Control 4--pro1-con4--",
        "Profile 2 - Control 1--pro2-con1--"
    ]
    assert_suggestions_text_id_version(expected, actual_data)
  end
end
