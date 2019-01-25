+++
title = "Installation Guide"
description = "Install Chef Automate 2.0 directly on Ubuntu or CentOS servers."
draft = false
bref = "Chef Automate 2.0 can be installed directly on Ubuntu or CentOS servers."
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 20
+++

Before beginning your installation, check the [System Requirements]({{< relref "system-requirements.md" >}}) for Automate.

See [Airgapped Installation]({{< relref "airgapped-installation.md" >}}) for
installing Chef Automate to a host with no inbound or outbound internet
traffic.

## Obtain Chef Automate Installation and Admin Tool

```shell
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

## Create Default Configuration

```shell
sudo ./chef-automate init-config
```

This will create a config.toml file with default values that should allow you to proceed,
or you can edit it to specify desired FQDN, login name, and so on.

See [Configuring Chef Automate]({{< relref "configuration.md" >}}) for more information
on configuration settings.

{{< warning >}}
Chef Automate's default memory configuration for the embedded Elasticsearch is too small for
uses other than POC or testing.
{{< /warning >}}

To configure it properly, assuming you have 16GB of memory or greater, create a patch toml
like the following examples and apply it following the instructions at
[Elasticsearch heap size]({{< relref "configuration.md#setting-elasticsearch-heap" >}})
Consult with Chef support before exceeding 25% of total RAM on your Chef Automate system.

```toml
[elasticsearch.v1.sys.runtime]
  heapsize = "4g"
```

If you have 32GB, this is also acceptable

```toml
[elasticsearch.v1.sys.runtime]
  heapsize = "8g"
```

## Deploy Chef Automate

```shell
sudo ./chef-automate deploy config.toml
```

Deployment takes a few minutes. The first step is accepting the terms of service in the command line, after which the installer performs a series of pre-flight checks;
any unsuccessful checks have information for resolving issues or skipping the check.
Run the deploy command again, after resolving any pre-flight issues.

At the end of the deployment process you will see:

```shell
Deploy complete
```

The deployment process writes login credentials to the `automate-credentials.toml` in your current working directory.

## Open Chef Automate

Navigate to `https://<chef-automate-fqdn>` in a browser and log in to Chef Automate with
the credentials provided in `automate-credentials.toml`.  Once you log in, Chef Automate
prompts you for a license.

When your Chef Automate instance is equipped with internet connectivity, you can get a 60-day trial license from there.
Alternatively, a license obtained by other means can be applied.

### Configure Data Collection

To send data from your Chef Server or Chef Clients to Chef Automate 2, the process is the same as Chef Automate 1.
See ["Configure Data Collection"]({{< relref "data-collection.md" >}}) for more information.

## Upgrades

By default, Chef Automate will automatically upgrade to the latest version available. These updates can be taken safely, as we've committed to ensuring the stability of the upgrade process - automatic updates will never introduce breaking changes.

### Release Channels

The Chef Automate upgrade process makes use of **release channels** to allow greater control over the automatic upgrades applied to your system. Chef Automate will always pull from the latest release within its specified release channel. We're initially shipping with the default `current` channel, but additional channels will be introduced in the future.

To change the release channel that is used for upgrades, modify the `channel` setting in your `config.toml` file:

```toml
channel = "current"
```

### Disable Automatic Upgrades

You can disable automatic upgrades by modifying the `upgrade_strategy` setting in your `config.toml` file:

```toml
upgrade_strategy = "none"
```

To manually initiate an upgrade, run

```shell
chef-automate upgrade run
```

This command upgrades Chef Automate to the latest version available from your release channel.

### Common Problems

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

Once you correct and save the FQDN, run

```shell
sudo chef-automate config patch config.toml
```

and retry opening Chef Automate in your browser.
