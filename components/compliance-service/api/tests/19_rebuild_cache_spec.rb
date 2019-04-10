##### GRPC SETUP #####
require 'api/profiles/profiles_pb'
require 'api/profiles/profiles_services_pb'

describe File.basename(__FILE__) do
  Profiles = Chef::Automate::Domain::Compliance::Api::Profiles unless defined?(Profiles)
  def profilesAdmin ; Profiles::ProfilesAdminService ; end

  it "works" do
    ##### Helper #####
    require 'net/http'
    es_host = 'http://localhost:9200'

    # the profile sha for /profiles/chris/apache-baseline/version/2.0.2
    es_url = es_host + '/comp-2-profiles/doc/3e1310b071dc4d706263e9d07083e10a92b4b69e4a36cffa1eda7eaecc09969a'
    uri = URI(es_url)

    # delete profile
    Net::HTTP.start(uri.host, uri.port) do |http|
      request = Net::HTTP::Delete.new uri
      response = http.request request # Net::HTTPResponse object
    end

    # we check that the profile is not there
    Net::HTTP.start(uri.host, uri.port) do |http|
      request = Net::HTTP::Get.new uri
      response = http.request request # Net::HTTPResponse object
      assert_equal("404", response.code)
    end
  end
end
