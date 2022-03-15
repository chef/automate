+++
title = "Getting Started"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Getting Started"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha_auto_install.md Getting Started"
    weight = 260
+++

Both deployment models require installing and configuring the Chef Automate High Availability (HA) on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

## Download and Install the Chef Automate Utility

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.

- Enter the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**.

  The command downloads the Chef Automate utility package in .zip format and installs the utility by providing the execute permission to the Chef Automate file.

The installation of the Chef Automate utility completes, and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}
