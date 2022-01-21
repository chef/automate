+++
title = "Upgrade Chef Automate"
date = 2022-01-04T12:02:46-08:00
draft = false

[menu]
  [menu.automate]
    title = "Upgrade"
    identifier = "automate/upgrade.md Upgrade"
    parent = "automate"
+++

[\[edit on GitHub\]](https://github.com/chef/automate/blob/main/components/docs-chef-io/content/automate/upgrade.md)

By default, Chef Automate automatically upgrades to the latest version.

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

To manually initiate an upgrade, run

```shell
chef-automate upgrade run
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
