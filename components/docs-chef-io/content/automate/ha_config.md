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

- Frontend patch/set will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node
    of the cluser.
- After patching/setting some services will restart. So the health status will take upto 2 minutes to show healthy.

{{< /note >}}

{{< warning >}}

- For certificate rotation, don't use `config patch`. Instead `cert-rotate` command can be used. To know more about certificate rotation click [here](/automate/ha_cert_rotaion)
- While patching the same from **the provision host**, structures such as TLS from OpenSearch configuration toml file and SSL from PostgreQL configuration toml file will be ignored.

{{< /warning >}}

## Show Configuration

Showing configuration in all nodes can be done from bastion server using below commands.

### Show Configuration for Automate cluster

To show configuration for Automate cluster, run the following command:

```bash
chef-automate config show --automate
#or
chef-automate config show -a
#or
chef-automate config show --a2
```

### Show Configuration for Chef Server cluster

To show configuration for Chef Server cluster, run the following command:

```bash
chef-automate config show --chef_server
#or
chef-automate config show -c
#or
chef-automate config show --cs
```

### Show Configuration for Postgresql cluster

To show configuration for Postgresql cluster, run the following command:

```bash
chef-automate config show --postgresql
#or
chef-automate config show -p
#or
chef-automate config show --pg
```

### Show Configuration for OpenSearch cluster

To show configuration for OpenSearch cluster, run the following command:

```bash
chef-automate config show --opensearch
#or
chef-automate config show -o
#or
chef-automate config show --os
```

## Patch Configuration

Patching new configuration in all nodes can be done from bastion server using below commands.

### Patch Configuration for Frontend cluster

To patch configuration for Frontend cluster (includes Automate and Chef Server), run the following command:

```bash
chef-automate config patch path/to/automate-config.toml --frontend
#or
chef-automate config patch path/to/automate-config.toml -f
#or
chef-automate config patch path/to/automate-config.toml --fe
```

### Patch Configuration for Automate cluster

To patch configuration for Automate cluster, run the following command:

```bash
chef-automate config patch path/to/automate-config.toml --automate
#or
chef-automate config patch path/to/automate-config.toml -a
#or
chef-automate config patch path/to/automate-config.toml --a2
```

### Patch Configuration for Chef Server cluster

To patch configuration for Chef Server cluster, run the following command:

```bash
chef-automate config patch path/to/chef_server-config.toml --chef_server
#or
chef-automate config patch path/to/chef_server-config.toml -c
#or
chef-automate config patch path/to/chef_server-config.toml --cs
```

### Patch Configuration for Postgresql cluster

To patch configuration for Postgresql cluster, run the following command:

```bash
chef-automate config patch path/to/postgresql-config.toml --postgresql
#or
chef-automate config patch path/to/postgresql-config.toml -p
#or
chef-automate config patch path/to/postgresql-config.toml --pg
```

### Patch Configuration for OpenSearch cluster

To patch configuration for OpenSearch cluster, run the following command:

```bash
chef-automate config patch path/to/opensearch-config.toml --opensearch
#or
chef-automate config patch path/to/opensearch-config.toml -o
#or
chef-automate config patch path/to/opensearch-config.toml --os
```

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
