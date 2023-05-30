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

{{< note >}}

- All the nodes will apply the Frontend patch/set wherever the Postgresql and OpenSearch changes will be used to only one cluster node.
- After patching/setting, some services will restart. So the health status will take up to 2 minutes to show healthy.

{{< /note >}}

{{< warning >}}

- For certificate rotation, don't use `config patch`. Instead, the `cert-rotate` command can be used. To know more about certificate rotation, click [here](/automate/ha_cert_rotaion)
- While patching the same from **the provision host**, structures such as TLS from the OpenSearch configuration .toml file and SSL from the PostgreSQL configuration toml file will be ignored.

{{< /warning >}}

## Show Configuration

Showing configuration in all nodes can be done from the bastion server using the below commands.

### Show Configuration for Automate cluster

To show the configuration for Automate cluster, run the following command:

```bash
chef-automate config show --automate
#or
chef-automate config show -a
#or
chef-automate config show --a2
```

### Show Configuration for Chef Server cluster

To show the configuration for the Chef Server cluster, run the following command:

```bash
chef-automate config show --chef_server
#or
chef-automate config show -c
#or
chef-automate config show --cs
```

### Show Configuration for Postgresql cluster

To show the configuration for the Postgresql cluster, run the following command:

```bash
chef-automate config show --postgresql
#or
chef-automate config show -p
#or
chef-automate config show --pg
```

### Show Configuration for OpenSearch cluster

To show the configuration for the OpenSearch cluster, run the following command:

```bash
chef-automate config show --opensearch
#or
chef-automate config show -o
#or
chef-automate config show --os
```

## Patch Configuration

The bastion server can patch new configurations in all nodes using the commands below.

### Patch Configuration for Frontend cluster

For Information on Frontend cluster configs visit links below:
- Click [here](/automate/config_automate) for Automate
- Click [here](/automate/config_chef_server) for Chef Server
The configs which are common for Automate and Chef server both can be patched with --fe flag

To patch configuration for the Frontend cluster (includes Automate and Chef Server), run the following command:

```bash
chef-automate config patch path/to/automate-config.toml --frontend
#or
chef-automate config patch path/to/automate-config.toml -f
#or
chef-automate config patch path/to/automate-config.toml --fe
```

### Patch Configuration for Automate cluster

For Information on Automate node configs visit links below:
- Click [here](/automate/config_automate) for Automate

To patch the configuration for Automate cluster, run the following command:

```bash
chef-automate config patch path/to/automate-config.toml --automate
#or
chef-automate config patch path/to/automate-config.toml -a
#or
chef-automate config patch path/to/automate-config.toml --a2
```

### Patch Configuration for Chef Server cluster

For Information on Chef Server node configs visit links below:
- Click [here](/automate/config_automate) for Chef Server

To patch the configuration for the Chef Server cluster, run the following command:

```bash
chef-automate config patch path/to/chef_server-config.toml --chef_server
#or
chef-automate config patch path/to/chef_server-config.toml -c
#or
chef-automate config patch path/to/chef_server-config.toml --cs
```

### Patch Configuration for Postgresql cluster

For Information on PostgreSQL node configs visit links below:
- Click [here](/automate/config_automate) for PostgreSQL

To patch the configuration for the Postgresql cluster, run the following command:

```bash
chef-automate config patch path/to/postgresql-config.toml --postgresql
#or
chef-automate config patch path/to/postgresql-config.toml -p
#or
chef-automate config patch path/to/postgresql-config.toml --pg
```

### Patch Configuration for OpenSearch cluster

For Information on OpenSearch node configs visit links below:
- Click [here](/automate/config_automate) for OpenSearch

To patch the configuration for the OpenSearch cluster, run the following command:

```bash
chef-automate config patch path/to/opensearch-config.toml --opensearch
#or
chef-automate config patch path/to/opensearch-config.toml -o
#or
chef-automate config patch path/to/opensearch-config.toml --os
```

## Set Configuration

Setting new configurations in all nodes can be done from the bastion server using the below commands.

### Set Configuration for Automate cluster

For Information on Automate node configs visit links below:
- Click [here](/automate/config_automate) for Automate

To set the configuration for Automate cluster, run the following command:

```bash
chef-automate config set path/to/automate-config.toml --automate
#or
chef-automate config set path/to/automate-config.toml -a
#or
chef-automate config set path/to/automate-config.toml --a2
```

### Set Configuration for Chef Server cluster

For Information on Chef Server node configs visit links below:
- Click [here](/automate/config_automate) for Chef Server

To set the configuration for the Chef Server cluster, run the following command:

```bash
chef-automate config set path/to/chef_server-config.toml --chef_server
#or
chef-automate config set path/to/chef_server-config.toml -c
#or
chef-automate config set path/to/chef_server-config.toml --cs
```

### Set Configuration for Postgresql cluster

For Information on PostgreSQL node configs visit links below:
- Click [here](/automate/config_automate) for PostgreSQL

To set the configuration for the Postgresql cluster, run the following command:

```bash
chef-automate config set path/to/postgresql-config.toml --postgresql
#or
chef-automate config set path/to/postgresql-config.toml -p
#or
chef-automate config set path/to/postgresql-config.toml --pg
```

### Set Configuration for OpenSearch cluster

For Information on OpenSearch node configs visit links below:
- Click [here](/automate/config_automate) for OpenSearch

To set the configuration for the OpenSearch cluster, run the following command:

```bash
chef-automate config set path/to/opensearch-config.toml --opensearch
#or
chef-automate config set path/to/opensearch-config.toml -o
#or
chef-automate config set path/to/opensearch-config.toml --os
```
