+++
title = "Introduction"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Introduction"
    parent = "automate/deploy_high_availability/ha_upgrade"
    identifier = "automate/deploy_high_availability/ha_upgrade/ha_upgrade_introduction.md Introduction"
    weight = 200
+++

## What is Upgradation?

Upgradation is a process of replacing older version with improved newer version with some new features or/and fixes for bugs that has been noticed in older versions. Use the --major flag to upgrade from the latest to a major version. This section will talk about the version upgrade of Chef Automate with its possible scenarios.

{{< note >}} Unlike some of the other chef services, Automate HA won't upgrade from one minor version to another {{< /note >}}

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
