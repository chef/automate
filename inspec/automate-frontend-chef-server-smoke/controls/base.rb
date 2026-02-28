# encoding: utf-8
# copyright: Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.
# license: All rights reserved.

title 'Chef Automate Chef Server Frontend Deploy Smoke Tests'

describe hab_svc("chef/deployment-service") do
  it { should be_installed }
  it { should be_up }
end

%w(
  chef/automate-load-balancer
  chef/license-control-service
  chef/automate-cs-oc-erchef
  chef/pg-sidecar-service
  chef/es-sidecar-service
  chef/automate-es-gateway
  chef/backup-gateway
  chef/automate-cs-nginx
  chef/automate-cs-bookshelf
  chef/deployment-service
  chef/automate-cs-oc-bifrost
  chef/automate-pg-gateway
).each do |svc|
  describe hab_svc(svc) do
    it { should be_installed }
    it { should be_up }
  end
end

%w(
  license-control-service
  automate-cs-oc-erchef
  pg-sidecar-service
  automate-es-gateway
  automate-cs-nginx
  deployment-service
  automate-cs-oc-bifrost
  automate-pg-gateway
).each do |svc|
  describe command("HAB_LICENSE=accept-no-persist hab pkg exec chef/#{svc} /hab/svc/#{svc}/hooks/health-check") do
    its('exit_status') { should eq 0 }
  end
end
