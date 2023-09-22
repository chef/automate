+++
title = "Upgrade to External OpenSearch"
date = 2022-01-04T12:09:09-08:00
draft = false

[menu]
  [menu.automate]
    title = "Upgrade to OpenSearch"
    identifier = "automate/configuring_automate/elasticsearch/opensearch_external_upgrade.md opensearch_external_upgrade.md"
    parent = "automate/configuring_automate/elasticsearch"
+++

This guide covers upgrading services used by Chef Automate.

## Upgrade Elasticsearch to OpenSearch

On January 21, 2021, Elastic NV announced that they would change their software licensing strategy and not release new versions of Elasticsearch and Kibana under the permissive ALv2 license. Instead, Elastic is releasing Elasticsearch and Kibana under the Elastic license, with source code available under the Elastic License or Server Side Public License (SSPL). These licenses are not open source and do not offer users the same freedoms. Because of this, AWS made the decision to create and maintain a fork from the last ALv2 version of Elasticsearch and Kibana. The fork is called OpenSearch and is available under ALv2.

To upgrade AWS Elasticsearch, please follow instructions on [Upgrading to AWS OpenSearch](https://aws.amazon.com/blogs/aws/amazon-elasticsearch-service-is-now-amazon-opensearch-service-and-supports-opensearch-10/)


### Migration Planning

The upgrade process for Elasticsearch to OpenSearch requires a one-time downtime and takes about 10 mins. This process may take longer depending on your server hardware and the size of the node objects in Chef Automate.

### Requirements

{{< warning >}}
Upgrading Elasticsearch to OpenSearch upgrades the database for all connected services. If you have multiple services connected to Elasticsearch, make sure that you have stopped the other services and prepared them for the upgrade.
{{< /warning >}}

This upgrade guide is for systems running:

- A Single Elasticsearch v6.8 installation
- Using Ubuntu 18.04 or higher
- On a virtual machine such as an EC2 instance or on a single physical machine
- Enough free space to run a second copy of the data that is in the existing Elasticsearch 6.8 installation. This upgrade requires a minimum of 55% free space on the machine.

### Backup Chef Automate

{{< danger >}}
**BACKUP CHEF AUTOMATE AND SECURE THE DATA**. Preserve your backup at all costs. Copy the backup to a second and separate location.
{{< /danger >}}

Database migrations have inherent risk to your system. Create a backup before beginning any migration or update. This ensures that you have a recoverable state in case any step in the process fails. Copy the backup to a another disk that is not connected to Chef Automate. This ensures that you have state to restore, in case of a failure in the upgrade process.

Follow the [Backup]({{< relref "backup.md" >}}) documentation to make a copy of your Chef Automate data.

### Upgrade Chef Automate

- Follow instructions to upgrade to Chef Automate 4.x or higher. OpenSearch support is available on Chef Automate version 4.x or higher. [Click here to know how to upgrade to Chef Automate 4.x]({{< ref "major_upgrade.md" >}})
```

### Stop Chef Automate

After successful upgrade, stop Chef Automate Services.

```bash
sudo chef-automate stop
```

## Upgrading Elasticsearch to OpenSearch

Chef Automate 4.x supports external OpenSearch. Please follow the migration section below to migrate from external Elasticsearch to external OpenSearch.

Steps To Enable External OpenSearch
- Set the `external.opensearch` `enable` attribute to false.
- Set the `nodes` attribute to the external OpenSearch URL array.
- Set the auth .
- Set the `auth` attribute `scheme` to `basic_auth`.
- Set the `username` attribute to OpenSearch username.
- Set the `password` attribute to OpenSearch password.

For example:

```
[global.v1.external.opensearch]
  enable = true
  nodes = ["http://opensearch1.example:10168", "http://opensearch2.example:10168", "..." ]

# Uncomment and fill out if using external opensearch with SSL and/or basic auth
# [global.v1.external.opensearch.auth]
#   scheme = "basic_auth"
# [global.v1.external.opensearch.auth.basic_auth]
## Create this opensearch user before starting the Chef Automate deployment;
## Chef Automate assumes it exists.
#   username = "<admin username>"
#   password = "<admin password>"
# [global.v1.external.opensearch.ssl]
#  Specify either a root_cert or a root_cert_file
#  root_cert = """$(cat </path/to/cert_file.crt>)"""
#  server_name = "<opensearch server name>"

# Uncomment and fill out if using external opensearch that uses hostname-based routing/load balancing
# [esgateway.v1.sys.ngx.http]
#  proxy_set_header_host = "<your external es hostname>:1234"

# Uncomment and add to change the ssl_verify_depth for the root cert bundle
#  ssl_verify_depth = "2"
```

## Steps To Migrate from External Elasticsearch to External OpenSearch

Please refer the documentation for the Elasticsearch migration to OpenSearch.

[OpenSearch's documentation on upgrading to OpenSearch](https://opensearch.org/docs/latest/upgrade-to/upgrade-to/#upgrade-to-opensearch).

Once the migration of the data is complete, you can now configure Chef Automate to use OpenSearch clusters,
using `chef-automate config patch` command.

{{< note >}}
After upgrading to version 4.x, Chef Automate will have the configurations both for OpenSearch and Elasticsearch. We recommend removing the Elasticsearch configuration after upgrading to external OpenSearch.
{{< /note >}}
