+++
title = "Overview"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Overview"
    identifier = "automate/deploy_high_availability/configuration/ha_config.md Overview"
    parent = "automate/deploy_high_availability/configuration"
    weight = 200
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

{{< note >}}

- All the nodes will apply the Frontend patch/set wherever the PostgreSQL and OpenSearch changes will be used to only one cluster node.
- After patching/setting any configuration, certain services will restart. The time required for your services to regain stability may vary depending on the amount of data stored in your system.

{{< /note >}}

{{< warning >}}

- For certificate rotation, use the `cert-rotate` command instead of `config patch`. For more information, see [certificate rotation](/automate/ha_cert_rotation).
- While patching the same from **the provision host**, structures such as TLS from the OpenSearch configuration .toml file and SSL from the PostgreSQL configuration toml file will be ignored.

{{< /warning >}}

## Show Configuration

Showing configuration in all nodes can be done from the bastion server using the below commands.

### Show Configuration for Automate cluster

To show the configuration for Automate cluster, run the following command:

```bash
chef-automate config show --automate
```

or

```bash
chef-automate config show -a
```

or

```bash
chef-automate config show --a2
```

### Show Configuration for Chef Server cluster

To show the configuration for the Chef Server cluster, run the following command:

```bash
chef-automate config show --chef_server
```

or

```bash
chef-automate config show -c
```

or

```bash
chef-automate config show --cs
```

### Show Configuration for PostgreSQL cluster

To show the configuration for the PostgreSQL cluster, run the following command:

```bash
chef-automate config show --postgresql
```

or

```bash
chef-automate config show -p
```

or

```bash
chef-automate config show --pg
```

### Show Configuration for OpenSearch cluster

To show the configuration for the OpenSearch cluster, run the following command:

```bash
chef-automate config show --opensearch
```

or

```bash
chef-automate config show -o
```

or

```bash
chef-automate config show --os
```

## Patch Configuration

The bastion server can patch new configurations in all nodes using the commands below.

### Patch Configuration for Frontend cluster

To patch configuration for the Frontend cluster (includes Automate and Chef Server), run the following command:

```bash
chef-automate config patch path/to/automate-config.toml --frontend
```

or

```bash
chef-automate config patch path/to/automate-config.toml -f
```

or

```bash
chef-automate config patch path/to/automate-config.toml --fe
```

### Patch Configuration for Automate cluster

To patch the configuration for Automate cluster, run the following command:

```bash
chef-automate config patch path/to/automate-config.toml --automate
```

or

```bash
chef-automate config patch path/to/automate-config.toml -a
```

or

```bash
chef-automate config patch path/to/automate-config.toml --a2
```

### Patch Configuration for Chef Server cluster

To patch the configuration for the Chef Server cluster, run the following command:

```bash
chef-automate config patch path/to/chef_server-config.toml --chef_server
```

or

```bash
chef-automate config patch path/to/chef_server-config.toml -c
```

or

```bash
chef-automate config patch path/to/chef_server-config.toml --cs
```

### Patch Configuration for PostgreSQL cluster

For Information on PostgreSQL node configs visit links below:

- To know more about PostgreSQL node configs, see [HA PostgreSQL Node Config](/automate/config_postgresql) page.

To patch the configuration for the PostgreSQL cluster, run the following command:

```bash
chef-automate config patch path/to/postgresql-config.toml --postgresql
```

or

```bash
chef-automate config patch path/to/postgresql-config.toml -p
```

or

```bash
chef-automate config patch path/to/postgresql-config.toml --pg
```

### Patch Configuration for OpenSearch cluster

For Information on OpenSearch node configs visit links below:

- To know more about OpenSearch node configs, see [HA OpenSearch Node Config](/automate/config_opensearch) page.

To patch the configuration for the OpenSearch cluster, run the following command:

```bash
chef-automate config patch path/to/opensearch-config.toml --opensearch
```

or

```bash
chef-automate config patch path/to/opensearch-config.toml -o
```

or

```bash
chef-automate config patch path/to/opensearch-config.toml --os
```

## Set Configuration

Setting new configurations in all nodes can be done from the bastion server using the below commands.

### Set Configuration for Automate cluster

To set the configuration for Automate cluster, run the following command:

```bash
chef-automate config set path/to/automate-config.toml --automate
```

or

```bash
chef-automate config set path/to/automate-config.toml -a
```

or

```bash
chef-automate config set path/to/automate-config.toml --a2
```

### Set Configuration for Chef Server cluster

To set the configuration for the Chef Server cluster, run the following command:

```bash
chef-automate config set path/to/chef_server-config.toml --chef_server
```

or

```bash
chef-automate config set path/to/chef_server-config.toml -c
```

or

```bash
chef-automate config set path/to/chef_server-config.toml --cs
```

### Set Configuration for PostgreSQL cluster

For Information on PostgreSQL node configs visit links below:

- To know more about PostgreSQL node configs, see [HA PostgreSQL Node Config](/automate/config_postgresql) page.

To set the configuration for the PostgreSQL cluster, run the following command:

```bash
chef-automate config set path/to/postgresql-config.toml --postgresql
```

or

```bash
chef-automate config set path/to/postgresql-config.toml -p
```

or

```bash
chef-automate config set path/to/postgresql-config.toml --pg
```

### Set Configuration for OpenSearch cluster

For Information on OpenSearch node configs visit links below:

- To know more about OpenSearch node configs, see [HA OpenSearch Node Config](/automate/config_opensearch) page.

To set the configuration for the OpenSearch cluster, run the following command:

```bash
chef-automate config set path/to/opensearch-config.toml --opensearch
```

or

```bash
chef-automate config set path/to/opensearch-config.toml -o
```

or

```bash
chef-automate config set path/to/opensearch-config.toml --os
```

### How to patch max shards per node for opensearch

To configure OpenSearch max shard per node, create a TOML file that contains the partial configuration below. Change max_shards_per_node as needed, and then run [command]({{< relref "ha_config/#patch-configuration-for-opensearch-cluster" >}}) to apply change.

```toml
[cluster]
  max_shards_per_node = "2000"
```
