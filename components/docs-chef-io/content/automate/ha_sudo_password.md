+++
title = "Sudo Password"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Sudo Password"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_sudo_password.md Sudo Password"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This page explains enabling the `sudo password` for the Chef Automate High Availability (HA) deployment.

If you have configured the sudo password for the user, you need to create an environment variable **sudo_password** and set the password as the variable's value. Example: `export sudo_password=YOUR_SUDO_PASSWORD`. Once its done, run all sudo commands with the `sudo -E` or `--preserve-env` option. Example: `sudo -E chef-automate iam version`. This is required for the chef-automate CLI to run the commands with sudo privileges.

## Steps to Enable Sudo Password

To enable the `sudo password` for the Chef Automate HA deployment, follow the steps below:

1. Create a `sudo_password` environment variable. Example: `export sudo_password=1234`.
2. Pass the `sudo_password` environment variable to the `chef-automate` CLI commands. Example: `sudo -E chef-automate iam version`.

   a. To pass all your environment variables, including the `sudo_password` to the `chef-automate` CLI commands, you can use the `-E` or `--preserve-env` option. Example: `sudo -E chef-automate iam version` or `sudo --preserve-env chef-automate iam version`.

   b. To pass the `sudo_password` environment variable to the `chef-automate` CLI commands, you can supply the environment variable name to the `--preserve-env` argument. Example: `sudo --preserve-env=sudo_password chef-automate iam version`.

3. You can also set environment variables directly in the sudo command. Example: `sudo sudo_password=1234 chef-automate iam version`.

Pass the `sudo_password` environment variable to all the `chef-automate` CLI commands.
