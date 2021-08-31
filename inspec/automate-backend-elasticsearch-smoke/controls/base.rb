# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved

title 'Chef Automate Backend Deploy Elasticsearch Smoke Tests'

%w(
  chef/automate-backend-elasticsearch
  chef/automate-backend-kibana
  chef/automate-backend-metricbeat
  chef/automate-backend-journalbeat
).each do |svc|
  describe hab_svc(svc) do
    it { should be_installed }
    it { should be_up }
  end
end

describe bash("HAB_LICENSE=accept-no-persist /hab/svc/automate-backend-elasticsearch/hooks/health-check") do
  its('exit_status') { should eq 0 }
end

describe x509_certificate('/hab/svc/automate-backend-elasticsearch/config/certificates/odfe-ssl.pem') do
  its('validity_in_days') { should be > 30 }
end

describe x509_certificate('/hab/svc/automate-backend-elasticsearch/config/certificates/odfe-admin.pem') do
  its('validity_in_days') { should be > 30 }
end

describe x509_certificate('/hab/svc/automate-backend-kibana/config/certificates/kibana.pem') do
  its('validity_in_days') { should be > 30 }
end
