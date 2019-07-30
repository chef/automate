+++
title = "Airgapped Installation"
description = "Install Chef Automate 2.0 on an airgapped host."
draft = false
bref = "Chef Automate 2.0 can be installed on an airgapped host."
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 30
+++

## Overview

An airgapped host is one that has no direct inbound or outbound internet
traffic. To install or upgrade Chef Automate on an airgapped host, create an Airgap
Installation Bundle (AIB) on an internet-connected host. Transfer the Airgap Installation
Bundle and the `chef-automate` binary used to create it to the airgapped host for use.

## Obtain a License

Contact your Chef account representative to obtain a license for Chef Automate; Chef
Automate's trial license feature requires internet connectivity and cannot be used on an
airgapped host.

## Create an Airgap Installation Bundle

On an internet-connected host, download the Chef Automate command-line tool and use it to
prepare an Airgap Installation Bundle.

### Obtain Chef Automate installation and admin tool

Download the Chef Automate command-line tool from the `current` [release channel]({{< relref "install.md#release-channels" >}}).

```shell
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

### Prepare Airgap Installation Bundle

To download and bundle the software included in a Chef Automate release, run:

```shell
./chef-automate airgap bundle create
```

A successful execution of this command produces an Airgap Installation Bundle named
`automate-<timestamp>.aib`.

## Deploy the Airgap Installation Bundle

Transfer the `chef-automate` binary and the Airgap Installation Bundle to the airgapped
host. Put the chef-automate command line tool in a directory that is NOT in $PATH. The Automate installation process will put its own copy of `chef-automate` in /bin and manage it.

### Create Default Configuration

``` shell
sudo ./chef-automate init-config --upgrade-strategy none
```

creates a `config.toml` file with default values. Setting an upgrade strategy of `none`
prevents Chef Automate from checking its release channel for updates via the internet.

Edit `config.toml` to make changes to FQDN and other configuration settings. See
[Configuring Chef Automate]({{< relref "configuration.md" >}}) for more information on configuration settings.

### Deploy Chef Automate

```shell
sudo ./chef-automate deploy config.toml --airgap-bundle </path/to/airgap-install-bundle>
```

Deployment takes a few minutes. The first step is accepting the terms of service in the
command line, after which the installer performs a series of pre-flight checks.
Unsuccessful checks have information for resolving issues or skipping the check.
After resolving any pre-flight issues, run the deploy command again.

At the end of the deployment process you will see:

```shell
Deploy complete
```

The deployment process writes login credentials to the `automate-credentials.toml` in your current working directory.

### Open Chef Automate

Navigate to [https://<chef-automate-fqdn>](https://<chef-automate-fqdn>) in a browser and log in to Chef Automate with
the credentials provided in `automate-credentials.toml`.

Once you log in, Chef Automate prompts you for a license.

#### Configure Data Collection

To send data from your Chef Server or Chef Clients to Chef Automate 2, the process is the same as Chef Automate 1.
See [Configure Data Collection]({{< relref "data-collection.md" >}}) for more information.

### Upgrades
To upgrade an airgapped install, you must supply an airgap bundle.

On an internet-connected host, follow the steps in [Create an Airgap
Installation Bundle]({{< relref "#create-an-airgap-installation-bundle" >}}) to upgrade your
Chef Automate command-line tool and prepare an Airgap Installation Bundle. Transfer the
bundle and Chef Automate command-line tool to the airgapped host and run:

```shell
sudo chef-automate upgrade run --airgap-bundle </path/to/bundle>
```

Upgrades can be taken safely. We've committed to ensuring the stability of the upgrade
process to support Chef Automate's automatic upgrades feature.

### Common Problems

If you are unable to open Chef Automate, check that the `config.toml` contains the host's public DNS name as the FQDN.

```shell
# This is a default Chef Automate configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate instance with default settings.

[global.v1]
# The external fully qualified domain name.
# When the application is deployed you should be able to access 'https://<fqdn>/'
# to login.
fqdn = "<_Public DNS_name_>"
```

Once you correct and save the FQDN, run

```shell
sudo chef-automate config patch config.toml
```

and retry opening Chef Automate in your browser.
