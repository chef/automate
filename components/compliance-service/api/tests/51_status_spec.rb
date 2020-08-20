##### GRPC SETUP #####
require 'interservice/compliance/status/status_pb'
require 'interservice/compliance/status/status_services_pb'

describe File.basename(__FILE__) do
  it "works" do
    Status = Chef::Automate::Domain::Compliance::Status::ComplianceStatusService unless defined?(Status)

    resp = GRPC Status, :get_migration_status, Google::Protobuf::Empty.new()

    # Uncomment to see the entire migrations output when troubleshooting
    # puts resp.to_json

    assert_equal(41, resp.total)
    assert_equal(41, resp.completed)
    assert_equal(:FINISHED, resp.status)
  end
end
