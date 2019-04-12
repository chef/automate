require 'api/profiles/profiles_pb'
require 'api/profiles/profiles_services_pb'

describe File.basename(__FILE__) do
  Profiles = Chef::Automate::Domain::Compliance::Api::Profiles unless defined?(Profiles)

  def profiles;
    Profiles::ProfilesService;
  end

  def cleanup
    GRPC(profiles, :list, Profiles::Query.new(owner: 'chris'))['profiles'].each do |profile|
      GRPC profiles, :delete, Profiles::ProfileDetails.new(
          owner: 'chris',
          name: profile['name'],
          version: profile['version']
      )
    end
  end

  before(:all) {cleanup}
  after(:all) {cleanup}

  it "returns an error getting metadata for profiles that don't exist" do
    assert_grpc_error("could not determine profile information", 5) do
      GRPC profiles, :read, Profiles::ProfileDetails.new(
          owner: 'chris',
          name: 'apache-baseline',
          version: '2.0.2'
      )
    end
  end

  it "returns an error downloading profiles that don't exist" do
    assert_grpc_error("we could not find the requested profile", 5) do
      res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
          owner: 'chris',
          name: 'apache-baseline',
          version: '2.0.2'
      )
      res.to_a
    end
  end

  it "returns an error uploading invalid data" do
    invalid_content = "hello this is not a profile"
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
        chunk: Profiles::Chunk.new(data: invalid_content)
    )
    assert_grpc_error(/Check InSpec check failed for /, 3) do
      GRPC profiles, :create, [req]
    end
  end

  it "returns an empty list with no uploaded profiles" do
    actual = GRPC profiles, :list, Profiles::Query.new(owner: 'chris')
    assert_equal actual, Profiles::Profiles.new()
  end

  it "returns a success response when uploading a valid profile" do
    profile_content = File.read(File.join(MARKET_PATH, 'windows-baseline-1.1.0.tar.gz'), mode: 'rb')
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
        chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]
    assert_kind_of(Profiles::CheckResult, res)
    assert_equal(true, res['summary']['valid'])
  end

  it "returns the profile data" do
    res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'windows-baseline'
    )
    res = res.to_a
    assert_equal 1, res.size
    assert_kind_of Profiles::ProfileData, res.first
  end

  it "returns the profile metadata in a list" do
    actual_data = GRPC profiles, :list, Profiles::Query.new(
        owner: 'chris',
        name: 'windows-baseline'
    )
    expected_data = {
        "profiles" => [
            {
                "name" => "windows-baseline",
                "title" => "DevSec Windows Security Baseline",
                "maintainer" => "DevSec Hardening Framework Team",
                "copyright" => "DevSec Hardening Framework Team",
                "copyrightEmail" => "hello@dev-sec.io",
                "license" => "Apache 2 license",
                "summary" => "Baselin for best-preactice Windows OS hardening",
                "version" => "1.1.0",
                "owner" => "chris",
                "supports" => [{}],
                "depends" => [],
                "sha256" => "3ed3fcda4b03936f063f65598a7a08b2e37bd7a0a805939d1c0ba861b7160cc8",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            }
        ],
        "total" => 1
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "returns success for uploading another profile" do
    profile_content = File.read(File.join(MARKET_PATH, 'linux-patch-baseline-0.3.0.tar.gz'), mode: 'rb')
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
        chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]
    assert_equal(true, res['summary']['valid'])
  end

  it "returns all of a user's uploaded profiles" do
    actual_data = GRPC profiles, :list, Profiles::Query.new(owner: 'chris')
    expected_data = {
        "profiles" => [
            {
                "name" => "linux-patch-baseline",
                "title" => "DevSec Linux Patch Benchmark",
                "maintainer" => "Christoph Hartmann",
                "copyright" => "Christoph Hartmann",
                "copyrightEmail" => "chris@lollyrock.com",
                "license" => "MPLv2",
                "summary" => "Verifies that all patches have been applied",
                "version" => "0.3.0",
                "owner" => "chris",
                "supports" => [{}],
                "depends" => [],
                "sha256" => "c774e15f448a22f37fc798d36c0fdb9a8bdbb4c45ba86025c2833ed3ba6b0324",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            },
            {
                "name" => "windows-baseline",
                "title" => "DevSec Windows Security Baseline",
                "maintainer" => "DevSec Hardening Framework Team",
                "copyright" => "DevSec Hardening Framework Team",
                "copyrightEmail" => "hello@dev-sec.io",
                "license" => "Apache 2 license",
                "summary" => "Baselin for best-preactice Windows OS hardening",
                "version" => "1.1.0",
                "owner" => "chris",
                "supports" => [{}],
                "depends" => [],
                "sha256" => "3ed3fcda4b03936f063f65598a7a08b2e37bd7a0a805939d1c0ba861b7160cc8",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            }
        ],
        "total" => 2
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "returns the metadata for an individual 'available' profile" do
    res = GRPC profiles, :read, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'linux-patch-baseline',
        version: '0.3.0'
    )
    assert_equal('linux-patch-baseline', res['name'])
    assert_equal('DevSec Linux Patch Benchmark', res['title'])
    assert_equal('0.3.0', res['version'])
    assert_equal(false, res['controls'].nil?)
    assert_equal(2, res['controls'].length) unless res['controls'].nil?
    control_ids = res['controls'].map{ |c| c.id }
    assert_equal(["patches", "verify-patches"], control_ids)
    assert_equal('c774e15f448a22f37fc798d36c0fdb9a8bdbb4c45ba86025c2833ed3ba6b0324', res['sha256'])
    assert_equal("linux", res['supports'][0]['os_family'])
  end

  it "returns a success message when uploading a profile with dependencies" do
    profile_content = File.read(File.join('/tmp', 'mario-0.1.0.tar.gz'), mode: 'rb')
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
        chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]
    assert_equal(true, res['summary']['valid'])
  end

  it "returns a success message when uploading a profile in zip format" do
    profile_content = File.read(File.join('/tmp', 'apache-baseline-2.0.1.zip'), mode: 'rb')
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(contentType: 'application/zip'),
        chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]
    assert_equal(true, res['summary']['valid'])
  end

  it "returns a success message when uploading a newer version of an already uploaded profile" do
    profile_content = File.read(File.join('/tmp', 'apache-baseline-2.0.2.zip'), mode: 'rb')
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(contentType: 'application/zip'),
        chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]
    assert_equal(true, res['summary']['valid'])
  end

  it "returns the latest version of a profile when multiple are uploaded and no version number is specified" do
    res = GRPC profiles, :read, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'apache-baseline'
    )
    assert_equal('apache-baseline', res['name'])
    assert_equal('DevSec Apache Baseline', res['title'])
    assert_equal('chris', res['owner'])
    assert_equal('2.0.2', res['version'])
    assert_equal(false, res['controls'].nil?)
    assert_equal(14, res['controls'].length) unless res['controls'].nil?
    assert_equal('3e1310b071dc4d706263e9d07083e10a92b4b69e4a36cffa1eda7eaecc09969a', res['sha256'])
    assert_equal("unix", res['supports'][0]['os_family'])
  end

  it "returns the data for latest version of a profile" do
    res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'apache-baseline'
    )
    res = res.to_a
    assert_equal 1, res.size
    assert_kind_of Profiles::ProfileData, res.first
  end

  it "returns the data for a specific version of a profile" do
    res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'apache-baseline',
        version: '2.0.1'
    )
    res = res.to_a
    assert_equal 1, res.size
    assert_kind_of Profiles::ProfileData, res.first
  end

  it "returns an empty message when deleting a profile" do
    actual = GRPC profiles, :delete, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'windows-baseline',
        version: '1.1.0'
    )
    assert_equal actual, Google::Protobuf::Empty.new()
  end

  it "returns an error getting the metadata for a deleted profile" do
    assert_grpc_error("could not determine profile information", 5) do
      GRPC profiles, :read, Profiles::ProfileDetails.new(
          owner: 'chris',
          name: 'windows-baseline',
          version: '1.1.0'
      )
    end
  end

  it "returns a list of all profiles uploaded by the user" do
    actual_data = GRPC profiles, :list, Profiles::Query.new(owner: 'chris')
    expected_data = {
        "profiles" => [
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "maintainer" => "DevSec Hardening Framework Team",
                "copyright" => "DevSec Hardening Framework Team",
                "copyrightEmail" => "hello@dev-sec.io",
                "license" => "Apache 2 license",
                "summary" => "Test-suite for best-practice apache hardening",
                "version" => "2.0.2",
                "owner" => "chris",
                "supports" => [{}],
                "depends" => [],
                "sha256" => "3e1310b071dc4d706263e9d07083e10a92b4b69e4a36cffa1eda7eaecc09969a",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            },
            {
                "name" => "apache-baseline",
                "title" => "DevSec Apache Baseline",
                "maintainer" => "Hardening Framework Team",
                "copyright" => "Hardening Framework Team",
                "copyrightEmail" => "hello@dev-sec.io",
                "license" => "Apache 2 license",
                "summary" => "Test-suite for best-practice apache hardening",
                "version" => "2.0.1",
                "owner" => "chris",
                "supports" => [{}],
                "depends" => [],
                "sha256" => "41a02784bfea15592ba2748d55927d8d1f9da205816ef18d3bb2ebe4c5ce18a9",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            },
            {
                "name" => "linux-patch-baseline",
                "title" => "DevSec Linux Patch Benchmark",
                "maintainer" => "Christoph Hartmann",
                "copyright" => "Christoph Hartmann",
                "copyrightEmail" => "chris@lollyrock.com",
                "license" => "MPLv2",
                "summary" => "Verifies that all patches have been applied",
                "version" => "0.3.0",
                "owner" => "chris",
                "supports" => [{}],
                "depends" => [],
                "sha256" => "c774e15f448a22f37fc798d36c0fdb9a8bdbb4c45ba86025c2833ed3ba6b0324",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            },
            {
                "name" => "mario",
                "title" => "InSpec Profile",
                "maintainer" => "The Authors",
                "copyright" => "The Authors",
                "copyrightEmail" => "you@example.com",
                "license" => "Apache-2.0",
                "summary" => "An InSpec Compliance Profile",
                "version" => "0.1.0",
                "owner" => "chris",
                "supports" => [],
                "depends" => [{"name" => "linux-baseline", "compliance" => "admin/linux-baseline"}],
                "sha256" => "30965cce5dbab899fc90820d6f3465514d7bd6ea6e8a6ed44225f2bf410bd7a5",
                "groups" => [],
                "controls" => [],
                "attributes" => []
            }
        ],
        "total" => 4
    }
    assert_equal_json_content(expected_data, actual_data)
  end

  it "returns a success message installing a market profile" do
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(
            contentType: 'application/json',
            name: 'windows-baseline',
            version: '1.1.0'
        )
    )

    res = GRPC profiles, :create, [req]
    assert_kind_of(Profiles::CheckResult, res)
    assert_equal(true, res['summary']['valid'])
  end

  it "returns the metadata for an installed market profile" do
    res = GRPC profiles, :read, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'windows-baseline',
        version: '1.1.0'
    )
    assert_kind_of(Profiles::Profile, res)
    assert_equal("windows", res['supports'][0]['os_family'])
  end

  it "returns an error installing a non-existant market profile" do
    req = Profiles::ProfilePostRequest.new(
        owner: 'chris',
        meta: Profiles::Metadata.new(
            contentType: 'application/json',
            name: 'windows-baseline',
            version: '999.999.999'
        )
    )
    assert_grpc_error("could not find profile", 5) do
      GRPC profiles, :create, [req]
    end
  end

  it "returns the data for an installed market profile" do
    res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'windows-baseline',
        version: '1.1.0'
    )
    res = res.to_a
    assert_equal 1, res.size
    assert_kind_of Profiles::ProfileData, res.first
  end

  it "returns an empty response from uninstalling a market profile" do
    actual = GRPC profiles, :delete, Profiles::ProfileDetails.new(
        owner: 'chris',
        name: 'windows-baseline',
        version: '1.1.0'
    )
    assert_equal actual, Google::Protobuf::Empty.new()
  end

  it "returns the data for an uninstalled market profile" do
    res = GRPC profiles, :read_tar, Profiles::ProfileDetails.new(
        name: 'windows-baseline',
        version: '1.1.0'
    )
    res = res.to_a
    assert_equal 1, res.size
    assert_kind_of Profiles::ProfileData, res.first
  end
end
