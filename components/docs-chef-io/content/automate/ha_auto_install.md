+++
title = "Getting Started - High Availability Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Getting Started - High Availability Deployment"
    parent = "automate/install"
    identifier = "automate/install/ha_auto_install.md Getting Started - High Availability Deployment"
    weight = 260
+++

Both deployment models require you to install and configure Chef Automate on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

## Download and Install the Chef Automate Utility

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.
- Type the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**. The command downloads the Chef Automate utility package in .zip format, and installs the utility by providing the execute permission to the Chef Automate file.

  The installation of the Chef Automate utility completes, and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}
