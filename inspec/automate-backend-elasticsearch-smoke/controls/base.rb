# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'Chef Automate Backend Deploy Elasticsearch Smoke Tests'

%w(
  chef/automate-ha-elasticsearch
  chef/automate-ha-kibana
  chef/automate-ha-metricbeat
  chef/automate-ha-journalbeat
).each do |svc|
  describe hab_svc(svc) do
    it { should be_installed }
    it { should be_up }
  end
end

describe bash("HAB_LICENSE=accept-no-persist /hab/svc/automate-ha-elasticsearch/hooks/health-check") do
  its('exit_status') { should eq 0 }
end

describe x509_certificate('/hab/svc/automate-ha-elasticsearch/config/certificates/odfe-ssl.pem') do
  its('validity_in_days') { should be > 30 }
end

describe x509_certificate('/hab/svc/automate-ha-elasticsearch/config/certificates/odfe-admin.pem') do
  its('validity_in_days') { should be > 30 }
end

describe x509_certificate('/hab/svc/automate-ha-kibana/config/certificates/kibana.pem') do
  its('validity_in_days') { should be > 30 }
end
