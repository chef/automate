+++
title = "Upgrade Chef Automate"
date = 2022-01-04T12:02:46-08:00
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Upgrade"
    identifier = "automate/upgrade/upgrade.md Upgrade"
    parent = "automate/upgrade"
    weight = 10
+++

Chef Automate upgrades from one minor version to another automatically, but you cannot directly upgrade to any major version of Chef Automate. Use the `--major` flag to upgrade from the latest to a major version. This section will talk about the major version upgrade of Chef Automate with its possible scenarios.

{{< warning >}} Upgrade to the 4.7.x version requires SAN certificates, refer to [update non-SAN certificates](/automate/upgrade_san_certificates) for more details. {{< /warning >}}

## Release Channels

The Chef Automate upgrade process makes use of **release channels** to allow greater control over the automatic upgrades applied to your system. Chef Automate pulls the latest release within a specified release channel. The default channel is the `current` channel.

To change the release channel that is used for upgrades, modify the `channel` setting in your `config.toml` file:

```toml
channel = "current"
```

## Disable Automatic Upgrades

You can disable automatic upgrades by changing the `upgrade_strategy` setting to `none` in your `config.toml`:

```toml
upgrade_strategy = "none"
```

To manually initiate a minor upgrade, run:

```shell
chef-automate upgrade run
```

To manually initiate a major upgrade, run:

```shell
chef-automate upgrade run --major
```

This command upgrades Chef Automate to the latest version available from your release channel.

## Troubleshoot an Upgrade

If you are unable to open Chef Automate, check that the `config.toml` contains the public DNS as the FQDN.

```shell
# This is a default Chef Automate configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate instance with default settings.

[global.v1]
# The external fully qualified domain name.
# When the application is deployed you should be able to access 'https://<fqdn>/'
# to login.
fqdn = "<_Public DNS_name>"
```

Once you correct and save the FQDN, run:

```shell
sudo chef-automate config patch config.toml
```

Open Chef Automate in your browser.
