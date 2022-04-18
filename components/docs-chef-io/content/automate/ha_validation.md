+++
title = "Validation Commands"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Validation Commands"
    parent = "automate/deploy_high_availability/on_premises_deployment"
    identifier = "automate/deploy_high_availability/on_premises_deployment/ha_validation.md Validation Commands"
    weight = 210
+++

This page elaborates the validation procedure that checks the firewall rules and ports before Chef Automate High Availability (HA) backend cluster deployment in your network infrastructure.

## Validation Procedure for Airgap Environment

To examine whether the firewall rules are stateful and ports are opened before HA backend cluster deployment in an air-gapped environment( which means no access to the internet), follow the steps given below:

1. Download **hab**, *hab-x86_64-linux.tar.gz* by executing the following command:

```bash
sudo wget https://packages.chef.io/files/stable/habitat/latest/hab-x86_64-linux.tar.gz
```

2. Install hab package in your internet environment by executing the following commands that generate *Netcate Package*:

```bash
sudo tar -xvzf /tmp/hab-x86_64-linux.tar.gz -C /usr/local/bin --strip-components 1
export HAB_LICENSE=accept-no-persist
hab pkg install core/netcat -bf
ls -dtr1 /hab/cache/artifacts/core-netcat-*
```

3. Set the path of the `config.toml` file, `hab-utitlity` and `netcate` package in the given command, `./chef-automate validate-ha-infrastructure */path/to/config.toml* */path/to/hab.tar.gz* */path/to/netcat.hart*` as parameters.

```bash
./chef-automate validate-ha-infrastructure /root/config.toml /root/hab-x86_64-linux.tar.gz /hab/cache/artifact/core-netcat-<version>.hart
```

The above command shows the set firewall rules and the ports configured.

## Validation Procedure

To examine whether the firewall rules are stateful and ports are open before HA backend cluster deployment in your network environment having internet access, execute the following command:

```bash
`./chef-automate validate-ha-infrastructure /path/to/config.toml`
```

In the above command, provide the path of **config.toml** file in */path/to/config.toml*. The command shows the status of the set firewall rules and the ports configured.
