# encoding: utf-8
# copyright: 2019, Chef Software, Inc.
# license: All rights reserved.

title 'Chef Automate Frontend Deploy Smoke Tests'

describe hab_svc("chef/deployment-service") do
  it { should be_installed }
  it { should be_up }
end

%w(
  chef/backup-gateway
  chef/config-mgmt-service
  chef/automate-cs-bookshelf
  chef/pg-sidecar-service
  chef/session-service
  chef/automate-gateway
  chef/automate-cs-nginx
  chef/event-gateway
  chef/automate-load-balancer
  chef/event-service
  chef/automate-es-gateway
  chef/authz-service
  chef/secrets-service
  chef/teams-service
  chef/automate-ui
  chef/compliance-service
  chef/nodemanager-service
  chef/notifications-service
  chef/es-sidecar-service
  chef/automate-dex
  chef/automate-cs-oc-erchef
  chef/local-user-service
  chef/authn-service
  chef/license-control-service
  chef/automate-elasticsearch
  chef/automate-pg-gateway
  chef/automate-postgresql
  chef/deployment-service
  chef/ingest-service
  chef/automate-cs-oc-bifrost
  chef/data-feed-service
  chef/applications-service
).each do |svc|
  describe hab_svc(svc) do
    it { should be_installed }
    it { should be_up }
  end
end

control 'automate.service.status' do
  %w(
    applications-service
    authn-service
    authz-service
    automate-cs-nginx
    automate-cs-oc-bifrost
    automate-cs-oc-erchef
    automate-dex
    automate-elasticsearch
    automate-es-gateway
    automate-gateway
    automate-pg-gateway
    automate-postgresql
    automate-ui
    compliance-service
    config-mgmt-service
    data-feed-service
    deployment-service
    event-gateway
    event-service
    ingest-service
    license-control-service
    local-user-service
    nodemanager-service
    pg-sidecar-service
    secrets-service
    session-service
    teams-service
  ).each do |svc|
    describe command("HAB_LICENSE=accept-no-persist hab pkg exec chef/#{svc} /hab/svc/#{svc}/hooks/health-check") do
      its('exit_status') { should eq 0 }
    end
  end
end

control 'automate.diagnotsics' do
  only_if { input('run_diagnostics', value: false) }
  describe command('HAB_LICENSE=accept-no-persist chef-automate diagnostics run ~auth-policies ~cfgmgmt-liveness ~compliance-scanning ~purge') do
    its('exit_status') { should eq 0 }
  end
end


control 'automate.elasticsearch.diskspace' do
  describe elasticsearch_diskspace('http://localhost:10144') do
    its("total.min") { should cmp > 20 }
    its("available_percent.min") { should cmp > 10 }
  end
end

token = input('automate_admin_token', value: "")
lifecycle = automate_lifecycle(token)

control 'automate.lifecycle.settings' do
  only_if { !token.nil? && !token.empty? }
  describe lifecycle.jobs_for('infra') do
    # Ignore this one for now
    # its('delete_nodes') { should be_disabled }
    its('missing_nodes') { should be_enabled }
    its('missing_nodes_for_deletion') { should be_enabled }
    its('periodic_purge_timeseries') { should be_enabled }
  end

  describe lifecycle.jobs_for('compliance') do
    its('periodic_purge') { should be_enabled }
  end

  describe lifecycle.jobs_for('event_feed') do
    its('periodic_purge') { should be_enabled }
  end
end

cert_name = command('ls -t  /hab/svc/automate-load-balancer/data | grep .cert').stdout.lines.first.chomp

describe x509_certificate("/hab/svc/automate-load-balancer/data/#{cert_name}") do
  its('validity_in_days') { should be > 30 }
end
