require 'cheffish'

# Set up a basic a1 install with an a1 on one node talking to a chef-server on another
# node.

Chef::Config[:ssl_verify_mode] = :verify_none

compliance_secret_key = '21c9a5a2d80339aadbd404efe6e8cc6e'
db_superuser_password = '6591266f724f79da0ef8a984dee897b51eba969df9345ce0ddffdd9af627d39a83b3deb804ac4f2c000473c703a970d5f692'
data_collector_token = 'SI3JNdW4XIN3iNaNL2gWkSOjxowNQHW7r8NG9yV7fsI'
chef_private_key_path = '/volumes/keys/test.pem'
chef_public_key_path = '/volumes/keys/test.pub'

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

file '/etc/delivery/delivery.rb' do
  content <<-EOS.gsub(/^\s+/, '')
    delivery_fqdn 'basic-a1'
    delivery['chef_username'] = 'delivery'
    delivery['chef_private_key'] = '#{chef_private_key_path}'
    delivery['chef_server'] = 'https://chef-server/organizations/test/'
    delivery['secrets_key'] = '#{compliance_secret_key}'
    insights['enable'] = true
    delivery['primary'] = true
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

execute 'copy the private key' do
  command "cp #{chef_private_key_path} /etc/delivery/"
end

execute 'copy the public key' do
  command "cp #{chef_public_key_path} /etc/delivery/"
end

# Put the last reconfigure script for the chef-server in /volumes/keys, which is shared.
execute 'copy chef-server script' do
  command 'cp /basic-a1/extra.rb /volumes/keys/extra.rb'
end

execute 'copy license file' do
  command 'cp /basic-a1/delivery.license /var/opt/delivery/license/delivery.license'
  creates '/var/opt/delivery/license/delivery.license'
end

execute 'delivery-ctl reconfigure' do
  live_stream true
end

execute 'create test enterprise' do
  command(
    'delivery-ctl create-enterprise test ' \
    "--ssh-pub-key-file=#{chef_private_key_path} " \
    '| tee /basic-a1/enterprise-test-admin-login-creds'
  )

  retries 2

  not_if 'delivery-ctl list-enterprises | grep test'
end

remote_file '/etc/delivery/enterprise-test-admin-login-creds' do
  owner 'delivery'
  group 'delivery'
  source 'file:///basic-a1/enterprise-test-admin-login-creds'
end

execute 'create test user' do
  command(
    'delivery-ctl create-user test test-admin '\
    "--ssh-pub-key-file=#{chef_private_key_path} " \
    '--password test-admin-password --roles admin '\
    '| tee /basic-a1/keys/test-admin-login-creds'
  )

  retries 2

  not_if 'delivery-ctl list-users test | grep test-admin'
end

execute 'create migration test users' do
  command 'delivery-ctl create-users /basic-a1/internal_users.tsv'
  retries 2
  not_if 'delivery-ctl list-users test | grep bob'
end

directory '/var/opt/delivery/compliance/profiles/test' do
  owner 'delivery'
  group 'delivery'
  recursive true
end

remote_file '/var/opt/delivery/compliance/profiles/test/linux-patch-baseline-0.3.0.tar.gz' do
  owner 'delivery'
  group 'delivery'
  source 'file:///basic-a1/linux-patch-baseline-0.3.0.tar.gz'
end
