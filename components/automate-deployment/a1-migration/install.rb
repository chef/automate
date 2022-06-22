require 'mixlib/install'
require 'fileutils'

#
# install.rb is intended to be run in container or on an VM that will be used
# to generate A1 test data. It installs and configures A1 in as an AIO. It also
# installs hab so that the machine can generate hab data artifacts.
#

[
  { 'product_name' => 'automate',
    'channel'      => ENV['AUTOMATE_CHANNEL'].to_sym,
    'version'      => ENV['AUTOMATE_VERSION'],
    'package_name' => 'delivery',
  },
  { 'product_name' => 'chef-server',
    'channel'      => ENV['SERVER_CHANNEL'].to_sym,
    'version'      => ENV['SERVER_VERSION'],
    'package_name' => 'chef-server-core',
  },
].each do |product|
  options = {
    channel: product['channel'].to_sym,
    product_name: product['product_name'],
    platform: 'ubuntu',
    platform_version: '16.04',
    architecture: 'x86_64',
  }
  options['product_version'] = product['version'] if product['version']

  # Our target version
  artifact = Mixlib::Install.new(options).artifact_info

  # Our installed version
  version_cmd = Mixlib::ShellOut.new("dpkg-query --showformat='${Version}' --show #{product['package_name']}")
  version_cmd.run_command
  # Strip off the build version, eg "1.7.120-1" => "1.7.120"
  version_cmd.stdout =~ /\d+\.\d+\.\d+/
  installed_version = Regexp.last_match

  # Exit early and don't download the package if we're already running the
  # correct version.
  next if installed_version == artifact.version.to_s

  package_path = "/a1-migration/packages/#{product['package_name']}-#{artifact.version}.deb"

  directory ::File.dirname(package_path)

  remote_file package_path do
    source artifact.url
  end

  dpkg_package product['package_name'] do
    source package_path
    action :install
  end

  # rm for speed
  execute "rm #{package_path}"
end

# Install us some habitat so that we can build a1-migration-data artifacts

hab_zip_path = ::File.join(Chef::Config[:file_cache_path], 'hab.tgz')
hab_source = 'https://packages.chef.io/files/current/habitat/latest/hab-x86_64-linux.tar.gz?bt_package=hab-x86_64-linux'

remote_file hab_zip_path do
  source hab_source
end

execute "tar --strip=1 -xvf #{hab_zip_path} -C #{Chef::Config[:file_cache_path]}"

execute "#{Chef::Config[:file_cache_path]}/hab pkg install core/hab" do
  environment({"HAB_LICENSE" => "accept-no-persist"})
end

execute "#{Chef::Config[:file_cache_path]}/hab pkg binlink core/hab hab" do
  environment({"HAB_LICENSE" => "accept-no-persist"})
end

group 'hab'

user 'hab' do
  group 'hab'
end
