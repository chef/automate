+++
title = "Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Configuration"
    identifier = "automate/deploy_high_availability/ha_config.md Configuration"
    parent = "automate/deploy_high_availability"
    weight = 50
+++

## Patch Configuration

Patching new configuration in all nodes can be done from bastion server. This page explains all the argument and flags available using `config patch` command.

The available flags are listed below:

| Flag           | Duplicate | Shorthand | Usage                                                                              |
| -------------- | --------- | --------- | ---------------------------------------------------------------------------------- |
| --automate    | --a2     | -a        | Patch configuration to all the Automate nodes.                                     |
| --chef_server | --cs     | -c        | Patch configuration to all the Chef Server nodes.                                  |
| --frontend    | --fe     | -f        | Patch configuration to all the Frontend nodes. (Includes Automate and Chef Server) |
| --postgresql  | --pg     | -p        | Patch configuration to the PostgresQL cluser.                                      |
| --opensearch  | --os     | -o        | Patch configuration to the OpenSearch cluser.                                      |

You can use the above patches using the following format:

```toml
chef-automate config patch --< FLAG > < PATH_TO_TOML_FILE >
```

For Example: `chef-automate config patch --frontend /home/frontend.toml`

{{< note >}}

- Frontend patch will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node
    of the cluser.
- After patching some services will go restart. So the health status will take upto 2 minutes to show healthy.

{{< /note >}}

{{< warning >}}

- For certificate rotation, don't use `config patch`. Instead `cert-rotate` command can be used. To know more about certificate rotation click [here](/automate/ha_cert_rotaion)
- While patching the same from **the provision host**, structures such as TLS from OpenSearch configuration toml file and SSL from PostgreQL configuration toml file will be ignored.

{{< /warning >}}

## Set Configuration

Setting new configuration in all nodes can be done from bastion server using below commands.

### Set Configuration for Automate cluster

To set configuration for Automate cluster, run the following command:

```bash
chef-automate config set path/to/automate-config.toml --automate
#or
chef-automate config set path/to/automate-config.toml -a
#or
chef-automate config set path/to/automate-config.toml --a2
```

### Set Configuration for Chef Server cluster

To set configuration for Chef Server cluster, run the following command:

```bash
chef-automate config set path/to/chef_server-config.toml --chef_server
#or
chef-automate config set path/to/chef_server-config.toml -c
#or
chef-automate config set path/to/chef_server-config.toml --cs
```

### Set Configuration for Postgresql cluster

To set configuration for Postgresql cluster, run the following command:

```bash
chef-automate config set path/to/postgresql-config.toml --postgresql
#or
chef-automate config set path/to/postgresql-config.toml -p
#or
chef-automate config set path/to/postgresql-config.toml --pg
```

### Set Configuration for OpenSearch cluster

To set configuration for OpenSearch cluster, run the following command:

```bash
chef-automate config set path/to/opensearch-config.toml --opensearch
#or
chef-automate config set path/to/opensearch-config.toml -o
#or
chef-automate config set path/to/opensearch-config.toml --os
```
