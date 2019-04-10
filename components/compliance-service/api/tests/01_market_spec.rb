require 'api/profiles/profiles_pb'
require 'api/profiles/profiles_services_pb'

describe File.basename(__FILE__) do
  Profiles = Chef::Automate::Domain::Compliance::Api::Profiles unless defined?(Profiles)

  def profiles ; Profiles::ProfilesService ; end

  it "errors out when an invalid sort field is specified" do
    assert_grpc_error(/sort field 'bogus' is invalid. Use either 'name', 'title' or 'maintainer'/, 3) do
      GRPC profiles, :list, Profiles::Query.new(
        sort: 'bogus',
        order: 1
      )
    end
  end

  it "returns no profile when filtering for a missing profile" do
    actual_data = GRPC profiles, :list, Profiles::Query.new(
      name: 'missing-in-action'
    )
    expected_data = {
      "profiles": []
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "lists all profiles" do
    actual_data = GRPC profiles, :list, Profiles::Query.new()
    expected_data = {
      "profiles": [
        {
          "name": "linux-patch-baseline",
          "title": "DevSec Linux Patch Benchmark",
          "maintainer": "Christoph Hartmann",
          "copyright": "Christoph Hartmann",
          "copyrightEmail": "chris@lollyrock.com",
          "license": "MPLv2",
          "summary": "Verifies that all patches have been applied",
          "version": "0.3.0",
          "supports"=>[{}],
          "depends": [],
          "sha256": "c774e15f448a22f37fc798d36c0fdb9a8bdbb4c45ba86025c2833ed3ba6b0324",
          "groups": [],
          "controls": [],
          "attributes": []
        },
        {
          "name": "windows-baseline",
          "title": "DevSec Windows Security Baseline",
          "maintainer": "DevSec Hardening Framework Team",
          "copyright": "DevSec Hardening Framework Team",
          "copyrightEmail": "hello@dev-sec.io",
          "license": "Apache 2 license",
          "summary": "Baselin for best-preactice Windows OS hardening",
          "version": "1.1.0",
          "supports"=>[{}],
          "depends": [],
          "sha256": "3ed3fcda4b03936f063f65598a7a08b2e37bd7a0a805939d1c0ba861b7160cc8",
          "groups": [],
          "controls": [],
          "attributes": []
        }
      ],
      "total": 2
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "lists all profiles by name" do
    actual_data = GRPC profiles, :list, Profiles::Query.new(
      name: 'windows-baseline'
    )

    expected_data = {
      "profiles": [
        {
          "name": "windows-baseline",
          "title": "DevSec Windows Security Baseline",
          "maintainer": "DevSec Hardening Framework Team",
          "copyright": "DevSec Hardening Framework Team",
          "copyrightEmail": "hello@dev-sec.io",
          "license": "Apache 2 license",
          "summary": "Baselin for best-preactice Windows OS hardening",
          "version": "1.1.0",
          "supports"=>[{}],
          "depends": [],
          "sha256": "3ed3fcda4b03936f063f65598a7a08b2e37bd7a0a805939d1c0ba861b7160cc8",
          "groups": [],
          "controls": [],
          "attributes": []
        }
      ],
      "total": 1
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "lists all profiles and sort by title" do
    actual_data = GRPC profiles, :list, Profiles::Query.new(
      sort: 'title',
      order: 1
    )
    expected_data = {
      "profiles": [
        {
          "name": "windows-baseline",
          "title": "DevSec Windows Security Baseline",
          "maintainer": "DevSec Hardening Framework Team",
          "copyright": "DevSec Hardening Framework Team",
          "copyrightEmail": "hello@dev-sec.io",
          "license": "Apache 2 license",
          "summary": "Baselin for best-preactice Windows OS hardening",
          "version": "1.1.0",
          "supports"=>[{}],
          "depends": [],
          "sha256": "3ed3fcda4b03936f063f65598a7a08b2e37bd7a0a805939d1c0ba861b7160cc8",
          "groups": [],
          "controls": [],
          "attributes": []
        },
        {
          "name": "linux-patch-baseline",
          "title": "DevSec Linux Patch Benchmark",
          "maintainer": "Christoph Hartmann",
          "copyright": "Christoph Hartmann",
          "copyrightEmail": "chris@lollyrock.com",
          "license": "MPLv2",
          "summary": "Verifies that all patches have been applied",
          "version": "0.3.0",
          "supports"=>[{}],
          "depends": [],
          "sha256": "c774e15f448a22f37fc798d36c0fdb9a8bdbb4c45ba86025c2833ed3ba6b0324",
          "groups": [],
          "controls": [],
          "attributes": []
        }
      ],
      "total": 2
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "streams results for read_tar" do
    res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
      name: 'linux-patch-baseline',
      version: '0.3.0'
    )
    res = res.to_a
    assert_equal 1, res.size
    assert_kind_of Profiles::ProfileData, res.first
  end

  it "returns a single 'market' profile" do
    patch_baseline = GRPC profiles, :read, Profiles::ProfileDetails.new(
      name: 'linux-patch-baseline',
      version: '0.3.0'
    )
    assert_kind_of(Profiles::Profile, patch_baseline)

    assert_equal("linux-patch-baseline", patch_baseline['name'])
    assert_equal("0.3.0", patch_baseline['version'])
    assert_kind_of(Google::Protobuf::RepeatedField, patch_baseline['controls'])
    assert_equal(2, patch_baseline['controls'].length)
    assert_equal("linux", patch_baseline['supports'][0]['os_family'])
  end

  it "returns a different single 'market' profile" do
    # since read returns the whole profile, we just check a few fields
    patch_baseline = GRPC profiles, :read, Profiles::ProfileDetails.new(
      name: 'windows-baseline',
      version: '1.1.0'
    )
    assert_kind_of(Profiles::Profile, patch_baseline)

    assert_equal("windows-baseline", patch_baseline['name'])
    assert_equal("1.1.0", patch_baseline['version'])
    assert_equal("DevSec Windows Security Baseline", patch_baseline['title'])
    assert_kind_of(Google::Protobuf::RepeatedField, patch_baseline['controls'])
    assert_equal(47, patch_baseline['controls'].length)
    control_ids = patch_baseline['controls'].map{ |c| c.id}
    assert_equal(["cis-access-cred-manager-2.2.1",
      "cis-account-lockout-duration-1.2.1",
      "cis-account-lockout-threshold-1.2.2",
      "cis-act-as-os-2.2.3",
      "cis-add-workstations-2.2.4",
      "cis-adjust-memory-quotas-2.2.5",
      "cis-enforce-password-history-1.1.1",
      "cis-maximum-password-age-1.1.2",
      "cis-minimum-password-age-1.1.3",
      "cis-minimum-password-length-1.1.4"], control_ids.slice(0,10))
    assert_equal("windows", patch_baseline['supports'][0]['os_family'])
  end
end
