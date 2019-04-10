# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved

title 'Automate 2.0 Deploy Smoke Tests'

describe hab_svc("chef/deployment-service") do
  it { should be_installed }
  it { should be_running }
end

%w(
  chef/authn-service
  chef/authz-service
  chef/automate-dex
  chef/automate-gateway
  chef/automate-load-balancer
  chef/automate-ui
  chef/compliance-service
  chef/config-mgmt-service
  chef/ingest-service
  chef/license-control-service
  chef/notifications-service
  chef/session-service
  chef/teams-service
  chef/automate-elasticsearch
  chef/automate-postgresql
).each do |svc|
  describe hab_svc(svc) do
    it { should be_installed }
    it { should be_running }
  end
end

include_controls 'a2-api-smoke'
