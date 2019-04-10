# encoding: utf-8
# copyright: 2017, Chef Software, Inc.
# license: All rights reserved

title 'Automate 2.0 Docker Compose Smoke Tests'

%w(
  authn-service
  authz-service
  automate-dex
  automate-gateway
  automate-load-balancer
  automate-ui
  compliance-service
  config-mgmt-service
  elasticsearch
  ingest-service
  license-control-service
  local-user-service
  session-service
  teams-service
  notifications-service
  postgresql
).each do |component_service|
  describe docker_container("a2_#{component_service}_1") do
    it { should be_running }
    its('repo') { should eq "chef/#{component_service}" }
    its('tag') { should eq "dev" }
  end
end

include_controls 'a2-api-smoke'
