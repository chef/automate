##### GRPC SETUP #####
require 'api/version/version_pb'
require 'api/version/version_services_pb'

describe File.basename(__FILE__) do
  it "works" do
    version = Chef::Automate::Domain::Compliance::Api::Version::VersionService

    SHA = `git rev-parse HEAD`.chomp
    resp = GRPC version, :version, Google::Protobuf::Empty.new()
    assert_equal("compliance", resp.api)
    assert_equal("compliance", resp.name)
    assert_equal(SHA, resp.sha)
    assert_match(/^\d{14}$/, resp.version)
    assert_match(/^\d{14}$/, resp.built)
  end
end
