require 'cheffish'

data_collector_url = 'https://basic-a1/data-collector/v0'
data_collector_token = 'SI3JNdW4XIN3iNaNL2gWkSOjxowNQHW7r8NG9yV7fsI'
chef_private_key_path = '/volumes/keys/test.pem'

Chef::Config[:ssl_verify_mode] = :verify_none

# Regenerate the chef-server config with the data collector endpoint and reconfigure.
# The chef-server puts the data collector's host/port into an nginx upstream and will not
# start if it cannot find it.
config_content = []
config_content << "topology 'standalone'"
config_content << "api_fqdn '#{ENV['CHEF_FQDN']}'"
config_content << "nginx['server_name'] = '#{ENV['CHEF_FQDN']}'"
config_content << "nginx['dhparam_key_length'] = 1024"
config_content << "data_collector['root_url'] = '#{data_collector_url}'"
config_content << "data_collector['token'] = '#{data_collector_token}'"

file '/etc/opscode/chef-server.rb' do
  content config_content.join("\n")
end

execute 'chef-server-ctl reconfigure'

# Bootstrap an organization and admin user as pivotal
with_chef_server 'https://chef-server',
  client_name: 'pivotal',
  signing_key_filename: '/volumes/keys/pivotal.pem' do

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
with_chef_server 'https://chef-server/organizations/test',
  client_name: 'test-admin',
  signing_key_filename: chef_private_key_path do

  chef_client 'test-node' do
    source_key_path chef_private_key_path
    retries 3
  end
end

# Bootstrap a node as the client
with_chef_server 'https://chef-server/organizations/test',
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

# I'm not sure the node name is right, but I tried to make it line up with a1-migration
file '/root/.chef/knife.rb' do
  content <<-EOS.gsub(/^\s+/, '')
    log_location     STDOUT
    chef_server_url  'https://localhost/organizations/test'
    node_name        'test-admin'
    ssl_verify_mode  :verify_none
    client_key       '#{chef_private_key_path}'
  EOS
end

file '/tmp/Berksfile' do
  content <<-EOS.gsub(/^\s+/, '')
    source "https://supermarket.chef.io"

    cookbook "audit", ">= 4.1.1"
  EOS
end

bash 'upload-cookbooks' do
  code <<-EOS.gsub(/^\s+/, '')
    berks install -b /tmp/Berksfile
    berks upload --no-ssl-verify -b /tmp/Berksfile
  EOS

  live_stream true
end
