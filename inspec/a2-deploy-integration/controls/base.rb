include_controls 'a2-deploy-smoke'
include_controls 'a2-api-integration'

describe file("/bin/chef-automate") do
  its('type') { should eq :symlink }
end

#
# chef-automate version: prints the version
#
describe command("chef-automate version") do
  its('stdout') { should match /Version: 2\nCLI Build: \d{14}\nServer Build: \d{14}\n/ }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

#
# chef-automate init-config: create a configuration
#
describe "chef-automate init-config" do
  let(:config_path) { "/tmp/config.toml" }
  let(:config_file) { inspec.backend.file(config_path) } # TODO: test file contents

  after { inspec.backend.run_command("rm -rf #{config_path}") }
  subject { command("chef-automate init-config --file #{config_path}") }

  its('stdout') { should match /Success/ }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

describe "chef-automate init-config --channel" do
  let(:config_path) { "/tmp/config.toml" }
  let(:config_file) { inspec.backend.file(config_path) } # TODO: test file contents
  let(:channel) { "current" }

  after { inspec.backend.run_command("rm -rf #{config_path}") }
  subject { command("chef-automate init-config --file #{config_path} --channel #{channel}") }

  its('stdout') { should match /Success/ }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

describe "chef-automate init-config --channel BAD_CHANNEL" do
  let(:config_path) { "/tmp/config.toml" }
  let(:config_file) { inspec.backend.file(config_path) } # TODO: test file contents
  let(:channel) { "BAD_CHANNEL" }

  after { inspec.backend.run_command("rm -rf #{config_path}") }
  subject { command("chef-automate init-config --file #{config_path} --channel #{channel}") }

  its('stdout') { should eq '' }
  its('stderr') { should match /Error/ }
  its('stderr') { should match /configuration/ }
  its('exit_status') { should eq 93 }
end

#
# chef-automate status: returns status of services
#
describe command("chef-automate status") do
  # TODO(ssd) 2018-07-23: This output is here to help us debug a
  # flakey test. Inspec's default output mode truncates the status
  # output and thus we can't see what is actually failing.
  it "complete output" do
    STDERR.puts "-------------- chef-automate status --------------"
    STDERR.puts subject.stdout
    STDERR.puts "--------------------------------------------------"
  end

  # NOTE: This list was up-to-date as of 2018-07-23
  %w{
      automate-postgresql
      automate-elasticsearch
      automate-ui
      authz-service
      es-sidecar-service
      automate-dex
      teams-service
      authn-service
      notifications-service
      compliance-service
      license-control-service
      local-user-service
      session-service
      ingest-service
      config-mgmt-service
      deployment-service
      data-lifecycle-service
      secrets-service
      automate-gateway
      automate-load-balancer
  }.each do |service_name|
    its('stdout') { should match /#{service_name}.* ok/ }
  end

  # status messages are defined in https://github.com/chef/automate/blob/master/components/automate-deployment/pkg/api/service_status.go
  # we check for "bad" statuses so the test output will show the full stdout if
  # we get a failure
  %w{
    warning
    CRITICAL
    unknown
    failed
  }.each do |unwanted_status|
    its('stdout') { should_not include(unwanted_status) }
  end

  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

describe "when deployment-service is restarted" do
  # HACK: We restart the deployment service in it's own control one time so that
  # the following behaviours have been tested after a restart. If we use a
  # before block in the tests it will be evaluated in every assertion which means
  # restarting the service several times.
  #
  # NOTE(ssd) 2018-07-23: The sleep is here to give deployment-service
  # time to process the signal.
  before(:all) { inspec.backend.run_command("pkill deployment; sleep 5") }

  subject { command("chef-automate status --wait-for-healthy --wait-timeout 40") }

  # TODO(ssd) 2018-07-23: This output is here to help us debug a
  # flakey test. Inspec's default output mode truncates the status
  # output and thus we can't see what is actually failing.
  it "complete output" do
    STDERR.puts "---- deployment-service restart: chef-automate status ----"
    STDERR.puts subject.stdout
    STDERR.puts "----------------------------------------------------------"
  end

  # status messages are defined in https://github.com/chef/automate/blob/master/components/automate-deployment/pkg/api/service_status.go
  # we check for "bad" statuses so the test output will show the full stdout if
  # we get a failure
  %w{
    warning
    CRITICAL
    unknown
    failed
  }.each do |unwanted_status|
    its('stdout') { should_not include(unwanted_status) }
  end

  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

# # We expect all services to come back after being restarted
# describe "when all services are restarted" do
#   before(:all) { inspec.backend.run_command("chef-automate restart-services") }
#   subject { command("chef-automate status --wait-for-healthy --wait-timeout 10") }
#   # TODO(ssd) 2018-07-23: This output is here to help us debug a
#   # flakey test. Inspec's default output mode truncates the status
#   # output and thus we can't see what is actually failing.
#   it "complete output" do
#     STDERR.puts "------ all services restart: chef-automate status ------"
#     STDERR.puts subject.stdout
#     STDERR.puts "--------------------------------------------------------"
#   end

#   its('exit_status') { should eq 0 }
# end

#
# chef-automate gather-logs: runs gather-logs to generate support bundle
#
describe command("chef-automate gather-logs") do
  after(:all) do
    archive_glob = "#{sys_info.hostname}-*T*Z.tar.gz"
    inspec.backend.run_command("rm #{archive_glob}")
  end

  its('exit_status') { should eq 0 }
  its('stderr')      { should eq '' }

  describe "gather-logs archive file" do
    before(:all) do
      archive_glob = "#{sys_info.hostname}-*T*Z.tar.gz"
      @archive_file = inspec.backend.run_command("ls #{archive_glob}").stdout.chomp
    end

    subject { file(@archive_file) }
    its('type')  { should eq :file }
    its('size')  { should > 0 }
    its('mtime') { should >= Time.now.to_i - 10 } # modified in the last 10 seconds
  end

  describe "gather-logs archive extracted" do
    before(:all) do
      archive_glob = "#{sys_info.hostname}-*T*Z.tar.gz"
      archive_file = inspec.backend.run_command("ls #{archive_glob}").stdout.chomp
      inspec.backend.run_command("tar xzf #{archive_file}")
    end

    subject { directory("#{sys_info.hostname}") }
    its('type') { should eq :directory }
  end
end

describe "hab pkg exec chef/deployment-service deployment-service health-check" do
  subject { command("hab pkg exec chef/deployment-service deployment-service health-check") }

  its('stdout') { should eq '' }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

describe "DEPLOYMENT_SERVICE_ADDRESS=BAD_ADDRESS hab pkg exec chef/deployment-service deployment-service health-check" do
  subject { command("DEPLOYMENT_SERVICE_ADDRESS=localhost:481516 hab pkg exec chef/deployment-service deployment-service health-check") }

  its('stdout') { should match /FAIL/ }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 4 }
end

# deployment-service decides when to update itself, not hab
describe file('/hab/sup/default/specs/deployment-service.spec') do
  its('content') { should match /update_strategy = "none"/ }
end

#
# chef-automate iam token create <name>: create a new token
#
describe command("chef-automate iam token create aNewToken --id inspec-test-#{Time.now.utc.to_i}") do
  its('stdout') { should_not be_nil }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end

#
# chef-automate iam admin-access restore chefautomate: restores admin password
# and permissions
#
# Note: This should be the default password, since this REALLY resets it.
#       This implies that you cannot mix a2-deploy-integration with inspec
#       test suites that accept a changed admin-password via the env var
#       AUTOMATE_API_DEFAULT_PASSWORD.
describe "chef-automate iam admin-access restore chefautomate" do
  subject { command("chef-automate iam admin-access restore chefautomate") }

  its('stdout') { should match /Success/ }
  its('stderr') { should eq '' }
  its('exit_status') { should eq 0 }
end
