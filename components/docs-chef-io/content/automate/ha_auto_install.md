+++
title = "Getting Started"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Getting Started"
    parent = "automate/deploy_high_availability/aws_deployment"
    identifier = "automate/deploy_high_availability/aws_deployment/ha_auto_install.md Getting Started"
    weight = 200
+++

Both deployment models require installing and configuring the Chef Automate High Availability (HA) on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

## Download and Install the Chef Automate Utility

Follow these steps to install the **Chef Automate** utility on the new server.

1. Log in to your [Bastion Host](/automate/ha_bastion) and navigate to your preferred location.

1. Execute the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` . The command downloads the Chef Automate utility package in .zip format and installs the utility by providing the execute permission to the Chef Automate file.

1. Execute `./chef-automate airgap bundle create` command. It downloads and bundles the software `automate-<timestamp>.aib` in the bastion host.

The installation of the Chef Automate utility completes, and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}
