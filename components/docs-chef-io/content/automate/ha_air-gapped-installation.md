+++
title = "Air-Gapped Installation"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Air-Gapped Installation"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_air-gapped-installation.md Air-Gapped Installation"
    weight = 100
+++

An air-gapped host has no direct inbound or outbound internet traffic. To install air-gapped in Chef Automate High Availability (HA), follow the steps below:

1. Download Chef-Automate CLI using the `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate` command.

1. To install air-gapped, keep the airgap-bundle (`.aib`) file handy in *bastion host*. Once the chef-automate CLI has been downloaded, create an airgap bundle using the `./chef-automate airgap bundle create` command. The bundle will be used to deploy Automate HA.

1. Copy the air-gapped bundle and chef-automate cli on the non-internet environment. It is recommended to copy the bundle using `scp` from `scp -i your-private-key.pem airgap-bundle.aib user@destination-ip-addess-172-32-0-1:airgap-bundle.aib scp -i your-private-key.pem chef-automate user@destination-ip-addess-172-32-0-1:chef-automate` command.

1. Permit the chef-automate CLI using the `chmod +x chef-automate` command.

1. Log in to your non-internet instance where you have copied the airgap bundle and generate `config.toml` using the `./chef-automate init-config-ha existing_infra` command.

1. Open `config.toml` and fill in the required details. For example, `ssh_user`, `ssh_key_file`, `fqdn`, `instance_count`, `automate_private_ip`, and other IP fields.

1. If the instance is *redhat*, set SElinux config `enforcing` to `permissive` in all the nodes. `.ssh` into your instance where you want to set SElinux `config.Reboot` the instance after executing the `sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config` command.

1. Start the deployment process using `./chef-automate deploy </path/to/config.toml> --airgap-bundle </path/to/airgap-bundle>` command.
