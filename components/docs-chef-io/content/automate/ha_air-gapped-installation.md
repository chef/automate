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

1. Download chef-automate cli using below command. curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate

1. For airgap installation we need to have airgap-bundle (.aib) file handy in bastion host. After downloading the chef-automate cli, make an airgap bundle using below command that will be used to deploy Automate HA. ./chef-automate airgap bundle create

1. Now copy airgap bundle and chef-automate cli on your non-internet environment that we have downloaded using above two steps. You can use scp to copy. scp -i your-private-key.pem airgap-bundle.aib user@destination-ip-addess-172-32-0-1:airgap-bundle.aib scp -i your-private-key.pem chef-automate user@destination-ip-addess-172-32-0-1:chef-automate

1. After copying two things just make sure that chef-automate cli has an executable permision assigned. If not provide permission using below command chmod +x chef-automate

1. Now login to your non-internet instance where you have copied airgap bundle and generate config.toml using below command. ./chef-automate init-config-ha existing_infra

1. Open config.toml and fill necessary details. Like ssh_user, ssh_key_file, fqdn, instance_count, automate_private_ip and other ips field.

1. If your instance is redhat then set SElinux config "enforcing" to "permissive" in all the nodes. ssh into your instance where you want to set SElinux config.Reboot the instance after executing below command. sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config

1. Now start the deployment process using below command. ./chef-automate deploy </path/to/config.toml> --airgap-bundle </path/to/airgap-bundle>
