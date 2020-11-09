##### GRPC SETUP #####
require 'interservice/compliance/profiles/profiles_pb'
require 'interservice/compliance/profiles/profiles_services_pb'

describe File.basename(__FILE__) do
  Profiles = Chef::Automate::Domain::Compliance::Profiles unless defined?(Profiles)
  def profiles ; Profiles::ProfilesService ; end

  it "errors on invalid zip package" do
    profile_path = File.expand_path('./wonky-profiles/bad.zip', File.dirname(__FILE__))
    profile_content = File.open(profile_path, 'rb').read
    req = Profiles::ProfilePostRequest.new(
      owner: 'chris',
      meta: Profiles::Metadata.new(contentType: 'application/zip'),
      chunk: Profiles::Chunk.new(data: profile_content)
    )
    assert_grpc_error("zip: not a valid zip file", 13) do
      GRPC profiles, :create, [req]
    end
  end

  it "errors on bad inspec archive" do
    profile_path = File.expand_path('./wonky-profiles/myinvalid-resource-0.2.5.zip', File.dirname(__FILE__))
    profile_content = File.open(profile_path, 'rb').read
    req = Profiles::ProfilePostRequest.new(
      owner: 'chris',
      meta: Profiles::Metadata.new(contentType: 'application/zip'),
      chunk: Profiles::Chunk.new(data: profile_content)
    )
    assert_grpc_error(/InSpec archive failed for /, 13) do
      GRPC profiles, :create, [req]
    end
  end

  it "errors when doing inspec check on the profile" do
    profile_path = File.expand_path('./wonky-profiles/myinvalid-resource-0.2.5.tar.gz', File.dirname(__FILE__))
    profile_content = File.open(profile_path, 'rb').read
    req = Profiles::ProfilePostRequest.new(
      owner: 'chris',
      meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
      chunk: Profiles::Chunk.new(data: profile_content)
    )
    assert_grpc_error(/Check InSpec check failed for /, 3) do
      GRPC profiles, :create, [req]
    end
  end

  it "works with warnings" do
    profile_path = File.expand_path('./wonky-profiles/myinvalid-warnings-0.2.4.tar.gz', File.dirname(__FILE__))
    profile_content = File.open(profile_path, 'rb').read
    req = Profiles::ProfilePostRequest.new(
      owner: 'chris',
      meta: Profiles::Metadata.new(contentType: 'application/x-gtar'),
      chunk: Profiles::Chunk.new(data: profile_content)
    )
    res = GRPC profiles, :create, [req]

    # TODO: need to loop back around and get these fixed. something wacky with the results
    assert_equal(Profiles::CheckResult, res.class)

    assert_equal(true, res['summary']['valid'])
    assert_equal(true, res['warnings'].include?(
      Profiles::CheckMessage.new(file: "controls/files_spec.rb", line: 9, control_id: "apache-08", msg: "Control apache-08 has no tests defined")
    ))
  end
end
