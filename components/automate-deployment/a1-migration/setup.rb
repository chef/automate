require 'cheffish'

# Set up an AIO Automate v1.x in a manner that mimics the OpsWorks and
# Marketplace installations.

Chef::Config[:ssl_verify_mode] = :verify_none

compliance_secret_key = '21c9a5a2d80339aadbd404efe6e8cc6e'
db_superuser_password = '6591266f724f79da0ef8a984dee897b51eba969df9345ce0ddffdd9af627d39a83b3deb804ac4f2c000473c703a970d5f692'
data_collector_token = 'SI3JNdW4XIN3iNaNL2gWkSOjxowNQHW7r8NG9yV7fsI'
chef_private_key_path = '/a1-migration/keys/test.pem'
chef_public_key_path = '/a1-migration/keys/test.pub'

%w(
  /etc/opscode
  /etc/delivery
  /var/opt/delivery/license
).each do |dir|
  directory dir do
    recursive true
  end
end

directory ::File.dirname(chef_private_key_path) do
  recursive true
end

file '/etc/opscode/chef-server.rb' do
  content <<-EOS.gsub(/^\s+/, '')
    api_fqdn '#{ENV['API_FQDN']}'
    bookshelf['storage_type'] = 'sql'
    dark_launch['actions'] = false
    data_collector['root_url'] = 'https://localhost/data-collector/v0'
    data_collector['token'] = '#{data_collector_token}'
    nginx['non_ssl_port'] = false
    nginx["ssl_protocols"] = "TLSv1.2"
    nginx['ssl_port'] = 8443
    nginx['dhparam_key_length'] = 1024
    nginx['server_name'] = '#{ENV['API_FQDN']}'
    oc_id['enable'] = false
    oc_chef_pedant["chef_server"] = "https://#{ENV['API_FQDN']}"
    opscode_erchef['base_resource_url'] = :host_header
    opscode_erchef['nginx_bookshelf_caching'] = :on
    opscode_erchef['s3_url_expiry_window_size'] = '50%'
    opscode_erchef['search_provider'] = 'elasticsearch'
    opscode_erchef['search_queue_mode'] = 'batch'
    opscode_solr4['external'] = true
    opscode_solr4['external_url'] = 'http://localhost:8080/elasticsearch'
    opscode_expander['enable'] = false
    postgresql['db_superuser'] = 'chef-pgsql'
    postgresql['db_superuser_password'] = '#{db_superuser_password}'
    postgresql['external'] = true
    postgresql['vip'] = '127.0.0.1'
    profiles["root_url"] = "https://localhost"
    rabbitmq['enable'] = false
    rabbitmq['management_enabled'] = false
    rabbitmq['queue_length_monitor_enabled'] = false
  EOS
end

file '/etc/delivery/delivery.rb' do
  content <<-EOS.gsub(/^\s+/, '')
    delivery_fqdn '#{ENV['API_FQDN']}'
    delivery['chef_username'] = 'delivery'
    delivery['chef_private_key'] = '#{chef_private_key_path}'
    delivery['chef_server'] = 'https://#{ENV['API_FQDN']}:8443/organizations/test/'
    delivery['secrets_key'] = '#{compliance_secret_key}'
    insights['enable'] = true
    delivery['primary'] = true
    postgresql['listen_address'] = '0.0.0.0'
    postgresql['superuser_enable'] = true
    postgresql['superuser_password']= '#{db_superuser_password}'
    data_collector['token'] = '#{data_collector_token}'
    delivery['chef_server_proxy'] = true
    insights['enable_auth'] = true
    kibana['enable_auth'] = true
    # Don't enable reaper, it will delete things when you load an older sample dataset
    reaper['enable'] = false
    reaper['mode'] = 'delete'
  EOS
end

# Create a key that we'll use for org, admin user, and client.
private_key chef_private_key_path do
  cipher            'DES-EDE3-CBC'
  format            :pem
  public_key_format :openssh
  public_key_path   chef_public_key_path
  size              2048
  type              :rsa
end

bash 'copy the user keys' do
  command 'cp /a1-migration/keys/*{.pub,.pem} /etc/delivery/'
end

execute 'copy license file' do
  command 'cp /a1-migration/delivery.license /var/opt/delivery/license/delivery.license'
  creates '/var/opt/delivery/license/delivery.license'
end

execute 'delivery-ctl reconfigure' do
  live_stream true
end

execute 'create test enterprise' do
  command(
    'delivery-ctl create-enterprise test ' \
    "--ssh-pub-key-file=#{chef_public_key_path} " \
    '| tee /a1-migration/keys/enterprise-test-admin-login-creds'
  )

  retries 2

  not_if 'delivery-ctl list-enterprise | grep test'
end

remote_file '/etc/delivery/enterprise-test-admin-login-creds' do
  owner 'delivery'
  group 'delivery'
  source 'file:///a1-migration/keys/enterprise-test-admin-login-creds'
end

execute 'create test user' do
  command(
    'delivery-ctl create-user test test-admin '\
    "--ssh-pub-key-file=#{chef_public_key_path} " \
    '--password test-admin-password --roles admin '\
    '| tee /a1-migration/keys/test-admin-login-creds'
  )

  retries 2

  not_if 'delivery-ctl list-users test | grep test-admin'
end

execute 'create migration test users' do
  command 'delivery-ctl create-users /a1-migration/internal_users.tsv'
  retries 2
  not_if 'delivery-ctl list-users test | grep bob'
end

execute 'chef-server-ctl reconfigure' do
  live_stream true
end

# Bootstrap an organization and admin user as pivotal
with_chef_server 'https://127.0.0.1:8443',
  client_name: 'pivotal',
  signing_key_filename: '/etc/opscode/pivotal.pem' do

  chef_user 'test-admin' do
    user_name 'test-admin'
    display_name 'test-admin'
    admin true
    email 'test-admin@chef.io'
    password 'password123!'
    source_key_path chef_private_key_path
    retries 3
  end

  chef_organization 'test' do
    full_name 'test'
    retries 3
    members %w(test-admin)
  end

  chef_acl '/organizations/test/containers/clients' do
    rights :all, users: 'test-admin'
  end
end

# Bootstrap a client as the admin user
with_chef_server 'https://127.0.0.1/organizations/test',
  client_name: 'test-admin',
  signing_key_filename: chef_private_key_path do

  chef_client 'test-node' do
    source_key_path chef_private_key_path
    retries 3
  end
end

# Bootstrap a node as the client
with_chef_server 'https://127.0.0.1/organizations/test',
  client_name: 'test-node',
  signing_key_filename: chef_private_key_path do

  chef_node 'test-node' do
    run_list %w(audit)
    attributes(
      'audit' => {
        'reporter' => 'chef-server-automate',
        'fetcher' => 'chef-server',
        'inspec_version' => '1.33.1',
        'profiles' => [
          { 'name' => 'linux-baseline', 'url' => 'https://github.com/dev-sec/linux-baseline/archive/2.1.0.zip' },
          { 'name' => 'linux-patch-baseline', 'compliance' => 'test/linux-patch-baseline' },
        ],
      }
    )
  end
end

directory '/root/.chef' do
  recursive true
end

file '/root/.chef/knife.rb' do
  content <<-EOS.gsub(/^\s+/, '')
    log_location     STDOUT
    chef_server_url  'https://localhost/organizations/test'
    node_name        'test-admin'
    ssl_verify_mode  :verify_none
    client_key       '#{chef_private_key_path}'
  EOS
end

file '/etc/chef/Berksfile' do
  content <<-EOS.gsub(/^\s+/, '')
    source "https://supermarket.chef.io"

    cookbook "audit", ">= 4.1.1"
  EOS
end

bash 'upload-cookbooks' do
  code <<-EOS.gsub(/^\s+/, '')
    berks install -b /etc/chef/Berksfile
    berks upload --no-ssl-verify -b /etc/chef/Berksfile
  EOS

  live_stream true
end

directory '/var/opt/delivery/compliance/profiles/test' do
  owner 'delivery'
  group 'delivery'
  recursive true
end

remote_file '/var/opt/delivery/compliance/profiles/test/linux-patch-baseline-0.3.0.tar.gz' do
  owner 'delivery'
  group 'delivery'
  source 'file:///a1-migration/linux-patch-baseline-0.3.0.tar.gz'
end

# Setup the delivery-cli to point to our workflow server and configure everything
# we need to configure to create organizations and projects with the test-admin user
directory '/workspace'
directory '/root/.ssh'

execute 'delivery-cli-setup' do
  command 'delivery setup --ent=test --org=test --user=test-admin --server=localhost'
  creates '/workspace/.delivery/cli.toml'
  cwd '/workspace'
  action :run
end

execute 'delivery-cli-token' do
  command 'AUTOMATE_PASSWORD=\'test-admin-password\' delivery token'
  creates '/root/.delivery/api-tokens'
  cwd '/workspace'
  action :run
end

execute 'delivery-cli-create-org' do
  command 'delivery api post orgs -d \'{"name": "test"}\''
  cwd '/workspace'
  action :run
end

file '/root/.gitconfig' do
  content "[user]\n  email = test-admin@chef.io\n  name = Test Admin"
  mode '0744'
end

file '/root/.ssh/config' do
  content "Host *\n  IdentityFile /a1-migration/keys/test.pem\n  StrictHostKeyChecking no"
  mode '0744'
end

# @afiune We need the PATH env variable to find the git binary
%w(awesome test cookbook for-migration).each do |cookbook|
  execute "generate-cookbook-#{cookbook}" do
    command "chef generate cookbook #{cookbook}"
    environment ({'PATH' => '/opt/chefdk/bin:/opt/chefdk/embedded/bin:/opt/delivery/bin:/opt/opscode/bin:/opt/chefdk/bin:/opt/chefdk/embedded/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/chefdk/gitbin'})
    cwd '/workspace'
    action :run
  end

  execute "delivery-cli-init-#{cookbook}" do
    command 'delivery init'
    environment ({'PATH' => '/opt/chefdk/bin:/opt/chefdk/embedded/bin:/opt/delivery/bin:/opt/opscode/bin:/opt/chefdk/bin:/opt/chefdk/embedded/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/chefdk/gitbin'})
    cwd "/workspace/#{cookbook}"
    action :run
  end
end
