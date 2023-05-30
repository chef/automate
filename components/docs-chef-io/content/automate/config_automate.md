+++
title = "HA Automate Node config"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA Automate Node config"
    parent = "automate/deploy_high_availability/configuration"
    identifier = "automate/deploy_high_availability/configuration/config_automate.md HA Automate Node config"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Automate provides various configuration options that can be patched to customize its behavior and meet specific requirements. This guide outlines some out of all the available configurations that you can modify.

The below configurations can be patched to Automate nodes. Please add the values you want to patch to a `config.toml` file and run `chef-automate config patch config.toml --a2` from bastion node.

### General Automate Configuration

#### Patching Automate FQDN (Fully Qualified Domain Name)

Click [here](/automate/configuration/#chef-automate-fqdn) to learn more.

#### Auto Upgrade ON/OFF

Click [here](/automate/configuration/#upgrade-strategy) to learn more.

#### Global Log Level

Click [here](/automate/log_management/) for more information

#### Centralised Logs

Click [here](/automate/centralizing_log/) for more information

#### Load Balancer

Click [here](/automate/configuration/#load-balancer) for more information

#### Content Security Policy Header

Click [here](/automate/configuration/#content-security-policy-header) for more information

#### Configure Data Feed

Click [here](/automate/datafeed/#configuring-global-data-feed-behavior) for more information

#### Proxy Settings

Click [here](/automate/configuration/#proxy-settings) for more information

#### Buffer Size

Configure message buffer ingest size:

Click [here](/automate/configuration/#buffer-size) for more information

#### Compliance Configuration

Click [here](/automate/configuration/#compliance-configuration) for more information

#### Configure Inflight Data Collector Request Maximum

Click [here](/automate/configuration/#configure-inflight-data-collector-request-maximum) for more information

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

#### Backup externally-deployed OpenSearch to local filesystem

Click [here](/automate/install/#backup-externally-deployed-opensearch-to-local-filesystem) for more information.

#### Backup externally deployed OpenSearch to AWS S3

Click [here](/automate/install/#backup-externally-deployed-opensearch-to-aws-s3) for more information.

### Configuring External PostgresSQL in Automate

#### Configure External PostgresSQL

Click [here](/automate/install/#configuring-an-external-postgresql-database) for more information on external PostgreSQL configuration.

#### Adding resolvers for PostgreSQL database

Click [here](/automate/install/#adding-resolvers-for-postgresql-database) for more information on external PostgreSQL configuration.

### Automate UI Configurations

#### Sign-out on Browser Close
Click [here](/automate/configuration/#sign-out-on-browser-close) for more information

#### Disclosure Banner

Click [here](/automate/configuration/#disclosure-banner) for more information

#### Disclosure Panel

Click [here](/automate/configuration/#disclosure-panel) for more information

#### Session Timeout

Click [here](/automate/session_timeout/) for more information

####  Telemetry

Click [here](/automate/telemetry/) for more information

#### Invalid Login Attempts

Click [here](/automate/invalid_login_attempts/) for more information

### Troubleshooting

Click [here](/automate/configuration/#troubleshooting) for more information