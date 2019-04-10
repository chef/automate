build_user = 'job_runner'

if node['platform'] == 'mac_os_x'
  home_dir = '/Users/job_runner'
else
  home_dir = '/home/job_runner'
end

user build_user do
  action :create
  home home_dir
  shell "/bin/bash"
end

group build_user do
  action :create
  members build_user
  append true
end

case node['platform']
when 'mac_os_x'
  group 'com.apple.access_ssh' do
    action :create
    members build_user
    append true
  end
end

directory home_dir do
  owner build_user
  group build_user
end

# Notes:
# - the second ALL is a hostname restriction -- we don't care
# - (root) means "root is the only user build_user can sudo TO"
# - /bin/ls is used for healthchecking
# - secure_path includes the location of delivery-cmd so we do not need to
#     execute it using the full file location
file File.join('/etc/sudoers.d', build_user) do
  content <<EOF
#{build_user} ALL=(root) NOPASSWD:/usr/local/bin/delivery-cmd, /bin/ls
Defaults:#{build_user} !requiretty
Defaults:#{build_user} secure_path = /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
EOF
  mode 0440
end

directory File.join(home_dir, '.ssh') do
  owner build_user
  group build_user
  mode 0700
end

file File.join(home_dir, '.ssh/authorized_keys') do
  content ARGV[1]
  owner build_user
  group build_user
  mode 0600
end

file '/usr/local/bin/delivery-cmd' do
  content ::File.read('/var/opt/delivery/workspace/bin/delivery-cmd')
  owner 'dbuild'
  group 'dbuild'
  mode 0755
end
