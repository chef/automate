# How to Trial A2 using Vagrant

## You will need

* Vagrant
* VirtualBox

You can install these using Homebrew cask:

```
brew cask install virtualbox
brew cask install vagrant
```

## Create your workspace

```
mkdir ~/a2-testing
cd ~/a2-testing
```

## Create a Vagrantfile

Create `Vagrantfile` with the following contents:

```
Vagrant.configure(2) do |config|
  config.vm.provider "virtualbox" do |v|
    v.memory = 2048
    v.cpus = 2
  end

  config.vm.box = "bento/ubuntu-16.04"
  config.vm.synced_folder ".", "/opt/a2-testing", create: true
  config.vm.hostname = 'automate-deployment.test'
  config.vm.network 'private_network', ip: '192.168.33.199'
  config.vm.provision "shell", inline: "apt-get update && apt-get install -y unzip"
  config.vm.provision "shell", inline: "sysctl -w vm.max_map_count=262144"
  config.vm.provision "shell", inline: "sysctl -w vm.dirty_expire_centisecs=20000"
end
```

## Start a new VM

Bring up a fresh vm:

```
cd ~/a2-testing
vagrant up
```

## Install A2 from within your new VM

Log in to your vm:

```
cd ~/a2-testing
vagrant ssh
```

Download the latest `chef-automate` CLI:

```
curl -O https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip
unzip chef-automate_linux_amd64.zip
```

## Create a config file

Build a default working config at `config.toml`:

```
./chef-automate init-config
```

## Start your deploy

Run the `deploy` subcommand as shown below to begin deployment to the
vm:

```
sudo ./chef-automate deploy config.toml
```

## Accessing the A2 Web UI

You need to access your instance of A2 using its configured FQDN for
authorization to work properly (this is an intentional security
feature). For local testing, add an alias to your Mac's `/etc/hosts`
file. Here's a one-liner to do this (do this in a new terminal session, or
exit your Vagrant shell session first):

```
echo 192.168.33.199 automate-deployment.test | sudo tee -a /etc/hosts
```

Now go to https://automate-deployment.test and log in with username
`admin` and password `chefautomate`.
