+++
title = "Quick Start Demo"
description = "The fast way to try Chef Automate"
weight = 1
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 10
+++

Hello and welcome to Chef Automate 2! This document guides you through the initial installation and trial activation.

The easiest way to start exploring Chef Automate is to install it on a local VM.
We recommend using [Vagrant][] to create your test environment.
Use this Vagrantfile to provision a VM. Chef recommends a minimum of 4GB RAM.

## Create a Vagrantfile

Copy and paste the command below to create a `Vagrantfile`:

```ruby
cat > Vagrantfile <<EOH
# Put the contents of this snippet in a file named Vagrantfile
Vagrant.configure(2) do |config|
  config.vm.provider "virtualbox" do |v|
    v.memory = 8192
    v.cpus = 2
  end

  config.vm.box = "bento/ubuntu-16.04"
  config.vm.synced_folder ".", "/opt/a2-testing", create: true
  config.vm.hostname = 'chef-automate.test'
  config.vm.network 'private_network', ip: '192.168.33.199'
  config.vm.provision "shell", inline: "apt-get update && apt-get install -y unzip"
  config.vm.provision "shell", inline: "sysctl -w vm.max_map_count=262144"
  config.vm.provision "shell", inline: "sysctl -w vm.dirty_expire_centisecs=20000"
end
EOH
```

## Add an entry to /etc/hosts

The example Vagrantfile sets the VM hostname to `chef-automate.test`. The installer uses this hostname as the FQDN that Chef Automate listens on.
For security reasons, you must access Chef Automate's web UI using the configured FQDN.
To avoid configuring DNS for your test instance, we recommend adding an entry to your workstation's `/etc/hosts` file.
Use the following command to append the required entry:

```bash
echo 192.168.33.199 chef-automate.test | sudo tee -a /etc/hosts
```

[Vagrant]: https://www.vagrantup.com/downloads.html

## Start the VM

Now that you have the `Vagrantfile` in your current directory, provision a new VM by running:

```bash
vagrant up
```

If this is your first time provisioning a VM, this operation may take a few
minutes to download the base OS image.

## Access the VM and Install Chef Automate

1. Open a terminal session on your VM:

     ```bash
     vagrant ssh
     ```

1. Download and unzip the command line tool:

     ```bash
     curl https://packages.chef.io/files/current/automate/latest/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
     ```

1. Run

     ```bash
     sudo ./chef-automate deploy
     ```

    You will be prompted to accept our [Terms of Service][] and our [Master License and Services Agreement][].

Once you've accepted these agreements, the installer downloads, configures, and deploys Chef Automate.
When the command completes, you can access the web UI by browsing to `https://chef-automate.test/` (assuming you followed the above instructions and created an entry in `/etc/hosts`).
You can find login credentials by opening the `automate-credentials.toml`, which the installer has written in your current working directory.

   [Terms of Service]: https://www.chef.io/terms-of-service
   [Master License and Services Agreement]: https://www.chef.io/online-master-agreement

## Start a free 60-day Trial

Retrieving a trial license through Chef Automate requires the Vagrant instance to have internet connectivity (at the time of trial license creation only).

1. Sign into the web UI at `https://chef-automate.test/`. Your credentials are in the automate-credentials.toml file.
1. Follow the instructions in the web UI to obtain a 60-day trial license by entering your name and email address.

## Installing without Vagrant

You can install Chef Automate on any x86_64 Linux instance running CentOS 7.5,
RHEL 7.5, or Ubuntu 16.04 with the following minimum system resources:

* 4GB RAM
* 5 GB free disk space
* 2 CPUs

Follow the instructions above, beginning with downloading the `chef-automate` zip file.
