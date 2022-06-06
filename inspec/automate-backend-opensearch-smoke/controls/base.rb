# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'Chef Automate Backend Deploy opensearch Smoke Tests'

%w(
  chef/automate-ha-opensearch
).each do |svc|
  describe hab_svc(svc) do
    it { should be_installed }
    it { should be_up }
  end
end

describe bash("HAB_LICENSE=accept-no-persist /hab/svc/automate-ha-opensearch/hooks/health-check") do
  its('exit_status') { should eq 0 }
end

describe x509_certificate('/hab/svc/automate-ha-opensearch/config/certificates/node1.pem') do
  its('validity_in_days') { should be > 30 }
end

describe x509_certificate('/hab/svc/automate-ha-opensearch/config/certificates/admin.pem') do
  its('validity_in_days') { should be > 30 }
end
