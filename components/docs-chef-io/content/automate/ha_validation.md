+++
title = "HA Validation Commands"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA Validation Commands"
    parent = "automate/install"
    identifier = "automate/install/ha_validation.md HA Validation Commands"
    weight = 200
+++

This page elaborates the validation procedure that checks the firewall rules and ports before Chef Automate High Availability (HA) backend cluster deployment in your network infrastructure.

## Validation Procedure for Airgap Environment

Follow these steps to examine the firewall rules are stateful, and ports are open before Chef Automate High Availability (HA) backend cluster deployment in air-gapped environment (means no access to the internet):

1. Download hab, *hab-x86_64-linux.tar.gz* by executing the command, `sudo wget https://packages.chef.io/files/stable/habitat/latest/hab-x86_64-linux.tar.gz`.

1. Install hab package in your internet environment by executing the following commands that generate *netcate package*:

```bash

sudo tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1

export HAB_LICENSE=accept-no-persist

hab pkg install core/netcat -bf

ls -dtr1 /hab/cache/artifacts/core-netcat-*

```

1. Provide the path of the `config.toml` file, `hab-utitlity` and `netcate` package in the command, ./chef-automate validate-ha-infrastructure */path/to/config.toml* */path/to/hab.tar.gz* */path/to/netcat.hart*  as parameters.

```bash

./chef-automate validate-ha-infrastructure /root/config.toml /root/hab-x86_64-linux.tar.gz /hab/cache/artifact/core-netcat-<version>.hart 

```

This command show the status of the set firewall rules and the ports configured.

## Validation Procedure for Airgap Environment

You need to execute the following command to examine the firewall rules are stateful, and ports are open before Chef Automate High Availability (HA) backend cluster deployment in your network environment which has access to the internet:

`./chef-automate validate-ha-infrastructure /path/to/config.toml`

where you need to provide the path of the *config.toml* file in */path/to/config.toml* in the above command.

This command show the status of the set firewall rules and the ports configured.
