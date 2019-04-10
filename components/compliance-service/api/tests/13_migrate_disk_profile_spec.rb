##### GRPC SETUP #####
require 'api/profiles/profiles_pb'
require 'api/profiles/profiles_services_pb'

describe File.basename(__FILE__) do
  Profiles = Chef::Automate::Domain::Compliance::Api::Profiles unless defined?(Profiles)

  OWNER = 'chris'

  def profiles ; Profiles::ProfilesService ; end
  def profiles_admin ; Profiles::ProfilesAdminService ; end

  def cleanup
    GRPC(profiles, :list, Profiles::Query.new(owner: OWNER))['profiles'].each do |profile|
      GRPC profiles, :delete, Profiles::ProfileDetails.new(
        owner: OWNER,
        name: profile['name'],
        version: profile['version']
      )
    end
  end

  before(:all) { cleanup }
  after(:all) { cleanup }

  it "works" do
    # put profile into user directory
    market_path = File.join(MARKET_PATH, 'windows-baseline-1.1.0.tar.gz')
    FileUtils.mkdir_p(File.join(PROFILES_PATH, OWNER))
    target_path = File.join(PROFILES_PATH, OWNER, 'windows-baseline-1.1.0.tar.gz')
    FileUtils.cp market_path, target_path

    # we run user migration command
    res = GRPC profiles_admin, :migrate_disk_profiles, Google::Protobuf::Empty.new()
    assert_kind_of(Google::Protobuf::Empty, res)

    # we check that the profile is there
    res = GRPC profiles, :read, Profiles::ProfileDetails.new(
      owner: OWNER,
      name: 'windows-baseline',
      version: '1.1.0'
    )
    assert_kind_of(Profiles::Profile, res)
  end
end
