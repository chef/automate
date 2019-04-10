require 'open-uri'
require 'json'

$enter_studio = <<SCRIPT
tee "/etc/profile.d/hab_studio_setup.sh" > "/dev/null" <<EOF
export HAB_STUDIO_SECRET_GITHUB_TOKEN=#{ENV['HAB_STUDIO_SECRET_GITHUB_TOKEN']}
export HAB_ORIGIN=#{ENV['HAB_ORIGIN']}

export AWS_ACCESS_KEY_ID=#{ENV['AWS_ACCESS_KEY_ID']}
export AWS_SECRET_ACCESS_KEY=#{ENV['AWS_SECRET_ACCESS_KEY']}

export AZURE_CLIENT_ID=#{ENV['AZURE_CLIENT_ID']}
export AZURE_CLIENT_SECRET=#{ENV['AZURE_CLIENT_SECRET']}
export AZURE_TENANT_ID=#{ENV['AZURE_TENANT_ID']}

sudo sysctl fs.inotify.max_user_watches=524288
sudo sysctl -p

cd /vagrant
source .envrc
hab studio enter

EOF
SCRIPT

def hab_version_from_manifest
  manifest = JSON.parse(open("https://packages.chef.io/manifests/dev/automate/latest.json").read)
  hab = manifest["hab"]
  hab.find {|x| x.start_with?("core/hab/") }.gsub("core/hab/", "")
end

$install_hab = <<SCRIPT
curl --silent https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh | sudo bash -s -- -v #{hab_version_from_manifest}
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "bento/ubuntu-16.04"

  config.vm.provider "virtualbox" do |v|
    v.memory = 8192
    v.cpus = 2
    v.customize ["modifyvm", :id, "--audio", "none"]
  end

  config.vm.synced_folder "~/.hab", "/home/vagrant/.hab"

  config.vm.network "forwarded_port", guest: 9010, host: 9010
  config.vm.network "forwarded_port", guest: 2345, host: 2345

  config.vm.hostname = 'a2-dev.test'
  config.vm.network 'private_network', ip: '192.168.33.222'

  config.vm.provision "shell", inline: $install_hab
  config.vm.provision "shell", inline: $enter_studio, run: "always"
end
