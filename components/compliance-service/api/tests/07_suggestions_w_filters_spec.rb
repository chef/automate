##### GRPC SETUP #####
require 'api/reporting/reporting_pb'
require 'api/reporting/reporting_services_pb'

describe File.basename(__FILE__) do
  Reporting = Chef::Automate::Domain::Compliance::Api::Reporting unless defined?(Reporting)

  def reporting
    Reporting::ReportingService;
  end

  it "works" do
    # suggest all controls on a 6 month file frame
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-07-01T23:59:59Z'])
      ]
    )
    expected = [
      "Check Apache config file owner, group and permissions.--apache-05--",
      "Disable Apache’s follows Symbolic Links for directories in alias.conf--apache-11--",
      "Disable Directory Listing for directories in alias.conf--apache-12--",
      "Disable insecure HTTP-methods--apache-10--",
      "Disable TRACE-methods--apache-09--",
      "Enable Apache Logging--apache-14--",
      "Set the apache server token--apache-07--",
      "Should not load certain modules--apache-08--",
      "SSL honor cipher order--apache-13--",
      "User and group should be set properly--apache-06--" ]
    assert_suggestions_text_id_version(expected, actual_data)


    # suggest controls with only time filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control',
      text: 'disable Addre',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-01T23:59:59Z'])
      ]
    )
    expected = [
      "Disable neighbor solicitations to send out per address--sysctl-27--",
      "Assign one global unicast IPv6 addresses to each interface--sysctl-28--",
      "Disable Core Dumps--sysctl-31--",
      "Disable log martians--sysctl-17--",
      "Disable TRACE-methods--apache-09--",
      "Disable Source Routing--sysctl-13--",
      "Disable IPv6 if it is not needed--sysctl-18--",
      "Disable learning Hop limit from router advertisement--sysctl-24--",
      "Disable the system`s acceptance of router advertisement--sysctl-25--",
      "Disable insecure HTTP-methods--apache-10--" ]
    assert_suggestions_text_id_version(expected, actual_data)


    # suggest controls with valid filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control',
      text: 'running User',
      size: 3,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-10T23:59:59Z']),
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod beta', 'DevSec Prod Alpha']),
        Reporting::ListFilter.new(type: 'role', values: ['nginx-hardening-prod', 'mia-role']),
        Reporting::ListFilter.new(type: 'control', values: ['os-01'])   # ignored when suggesting on controls
      ]
    )
    expected = [
      "Running worker process as non-privileged user--nginx-01--",
      "Apache should be running--apache-01--",
      "User and group should be set properly--apache-06--" ]
    assert_suggestions_text_id_version(expected, actual_data)


    # suggest controls with invalid filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'control',
      text: 'running User',
      size: 3,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-01-02T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-07-01T23:59:59Z']),
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Missing', 'DevSec Missing2']),
        Reporting::ListFilter.new(type: 'role', values: ['nginx-hardening', 'mia-role'])
      ]
    )
    expected = []
    assert_suggestions_text_id_version(expected, actual_data)

    # suggest profiles with a day filter and platform and valid recipe filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'profile',
      text: 'ngin',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-03-04T00:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'platform', values: ['centos', 'windows-missing']),
        Reporting::ListFilter.new(type: 'recipe', values: ['apache_extras', 'java::default']),
        Reporting::ListFilter.new(type: 'node_id', values: ['9b9f4e51-b049-4b10-9555-10578916e149', '123-missing']),
        Reporting::ListFilter.new(type: 'job_id', values: ['74a54a28-c628-4f82-86df-333333333333'])
      ]
    )
    expected = ["DevSec Nginx Baseline--09adcbb3b9b3233d5de63cd98a5ba3e155b3aaeb66b5abed379f5fb1ff143988--2.1.0"]
    assert_suggestions_text_id_version(expected, actual_data)


    # suggest profile, text with space and filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'profile',
      text: 'devsec apa ',
      size: 4,
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-02T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2018-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'platform', values: ['debian', 'fedora', 'win95'])
      ]
    )
    assert_suggestions_text_id_version( [
      "DevSec Apache Baseline--41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a8--2.0.0",
      "DevSec Linux Security Baseline--b53ca05fbfe17a36363a40f3ad5bd70aa20057eaf15a9a9a8124a84d4ef08015--2.0.1" ], actual_data )


    # suggest environment, partial and case insensitive text
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment',
      text: 'pro',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-02T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-04T23:59:59Z']),
        Reporting::ListFilter.new(type: 'platform', values: ['debian'])
      ]
    )
    assert_suggestions_text(["DevSec Prod Zeta"], actual_data)


    # suggest environment, when we have environment filter
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'environment',
      text: 'devsec',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2018-02-02T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-07T23:59:59Z']),
        Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod beta'])  # ignored when suggesting envs
      ]
    )
    assert_suggestions_text(["DevSec Prod Alpha", "DevSec Prod beta", "DevSec Prod Zeta"], actual_data)


    # suggest roles with valid filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'role',
      text: 'apache ',
      filters: [
       Reporting::ListFilter.new(type: 'start_time', values: ['2016-03-02T23:59:59Z']),
       Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-04T23:59:59Z']),
       Reporting::ListFilter.new(type: 'role', values: ['base_deb', 'missing-role'])
      ]
    )
    assert_suggestions_text(["apache_deb", "apache_linux", "apache_windows"], actual_data)


    # suggest recipes with valid filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'recipe',
      text: ' apaCHE fix',
      filters: [
       Reporting::ListFilter.new(type: 'start_time', values: ['2016-02-02T23:59:59Z']),
       Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-04T23:59:59Z']),
       Reporting::ListFilter.new(type: 'environment', values: ['DevSec Prod beta', 'missing'])
      ]
    )
    assert_suggestions_text(["nagios::fix", "apache_extras", "apache_extras::harden"], actual_data)

    # suggest inspec version
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'inspec_version',
      text: '3.1',
      filters: [
        Reporting::ListFilter.new(type: 'start_time', values: ['2016-02-02T23:59:59Z']),
        Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-04T23:59:59Z'])
      ]
    )
    assert_suggestions_text(["3.1.0", "3.1.3"], actual_data)

    # suggest nodes with valid filters
    actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
      type: 'node',
      text: 'red Win',
      filters: [
       Reporting::ListFilter.new(type: 'start_time', values: ['2016-02-02T23:59:59Z']),
       Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-04T23:59:59Z']),
       Reporting::ListFilter.new(type: 'platform', values: ['centos', 'redhat', 'windows'])
      ]
    )

    assert_suggestions_text_id_version( [
      "redhat(2)-alpha-nginx(f)-apache(s)-failed--9b9f4e51-b049-4b10-9555-10578916e111--",
      "windows(1)-zeta-apache(s)-skipped--a0ddd774-cbbb-49be-8730-49c92f3fc2a0--",
      "RedHat(2)-beta-nginx(f)-apache(s)-failed--9b9f4e51-b049-4b10-9555-10578916e222--",
      "redhat(2)-alpha-nginx(f)-apache(f)-failed--9b9f4e51-b049-4b10-9555-10578916e112--"
    ], actual_data )

      # suggest nodes with invalid filters
      actual_data = GRPC reporting, :list_suggestions, Reporting::SuggestionRequest.new(
        type: 'node',
        text: 'red Win',
        filters: [
         Reporting::ListFilter.new(type: 'start_time', values: ['2016-02-02T23:59:59Z']),
         Reporting::ListFilter.new(type: 'end_time', values: ['2019-03-04T23:59:59Z']),
         Reporting::ListFilter.new(type: 'environment', values: ['missing'])
        ]
      )
      assert_suggestions_text_id_version([], actual_data)
  end
end
