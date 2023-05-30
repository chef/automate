+++
title = "HA Chef Server Node config"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA Chef Server Node config"
    parent = "automate/deploy_high_availability/configuration"
    identifier = "automate/deploy_high_availability/configuration/config_chef_server.md HA Chef Server Node config"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

The below configurations can be patched to Chef Server nodes. Please add the values you want to patch to a `config.toml` file and run `chef-automate config patch config.toml --cs` from bastion node.

### General Chef Server Configuration

#### Configure chef-server to send data to Automate

```toml
[global.v1.external.automate]
enable = true
node = "https://<automate server url>"
[global.v1.external.automate.auth]
token = "<data-collector token>"
[global.v1.external.automate.ssl]
server_name = "<server name from the automate server ssl cert>"
root_cert = """<pem format root CA cert>
"""
[cs_nginx.v1.sys.ngx.http]
ssl_verify_depth = 6
```

#### Chef Infra Configuration In Chef Automate

Click [here](/automate/chef_infra_in_chef_automate) for more information

#### Patching Automate FQDN (Fully Qualified Domain Name)

Click [here](/automate/configuration/#chef-automate-fqdn) to learn more.

#### Auto Upgrade ON/OFF

Click [here](/automate/configuration/#upgrade-strategy) to learn more.

#### Proxy Settings

Click [here](/automate/configuration/#proxy-settings) for more information

#### Global Log Level

Click [here](/automate/configuration/#global-log-level) for more information

#### Centralised Logs

Click [here](/automate/centralizing_log/) for more information

#### Load Balancer
Click [here](/automate/configuration/#load-balancer) for more information

### Backups

#### Backup to a Filesystem

Click [here](/automate/backup/#backup-to-a-filesystem) for more information

#### Backup to AWS S3

Click [here](/automate/backup/#backup-to-aws-s3) for more information

### Configuring External OpenSearch in Automate

#### Configure External Opensearch

To know about OpenSearch configuration click [here](/automate/install/#configuring-external-opensearch)

#### Adding resolvers for external OpenSearch
To know about adding OpenSearch resolvers click [here](/automate/install/#adding-resolvers-for-opensearch)

#### Backup externally deployed OpenSearch to local filesystem

Click [here](/automate/install/#backup-externally-deployed-opensearch-to-local-filesystem) for more information.

#### Backup externally deployed OpenSearch to AWS S3

Click [here](/automate/install/#backup-externally-deployed-opensearch-to-aws-s3) for more information.

### Configuring External PostgreSQL in Automate

#### Configuring External PostgresSQL

Click [here](/automate/install/#configuring-an-external-postgresql-database) for more information on external PostgreSQL configuration.

#### Adding resolvers for PostgreSQl database

Click [here](/automate/install/#adding-resolvers-for-postgresql-database) for more information on external PostgreSQL configuration.

### Troubleshooting

Click [here](/automate/configuration/#troubleshooting) for more information