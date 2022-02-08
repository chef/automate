+++
title = "Elasticsearch"
date = 2022-01-04T12:09:09-08:00
draft = false

[menu]
  [menu.automate]
    title = "Configure Elasticsearch"
    identifier = "automate/configuring_automate/elasticsearch/elasticsearch.md Elasticsearch"
    parent = "automate/configuring_automate/elasticsearch"
+++

You can configure Chef Automate to use Elasticsearch clusters that are not deployed via Chef Automate itself.

## Configure External Elasticsearch

These configuration directions are intended for in the initial deployment of Chef Automate.

{{< note >}}
Chef Automate supports the official Elasticsearch Service by Elastic. Chef Automate does not test or support alternative services, such as Amazon Elasticsearch Service (Amazon ES).
{{< /note >}}

Add the following to your config.toml:

```toml
[global.v1.external.elasticsearch]
  enable = true
  nodes = ["http://elastic1.example:9200", "http://elastic2.example:9200", "..." ]

# Uncomment and fill out if using external elasticsearch with SSL and/or basic auth
# [global.v1.external.elasticsearch.auth]
#   scheme = "basic_auth"
# [global.v1.external.elasticsearch.auth.basic_auth]
## Create this elasticsearch user before starting the Automate deployment;
## Automate assumes it exists.
#   username = "<admin username>"
#   password = "<admin password>"
# [global.v1.external.elasticsearch.ssl]
#  Specify either a root_cert or a root_cert_file
#  root_cert = """$(cat </path/to/cert_file.crt>)"""
#  server_name = "<elasticsearch server name>"

# Uncomment and fill out if using external elasticsearch that uses hostname-based routing/load balancing
# [esgateway.v1.sys.ngx.http]
#  proxy_set_header_host = "<your external es hostname>:1234"

# Uncomment and add to change the ssl_verify_depth for the root cert bundle
#  ssl_verify_depth = "2"
```

Because externally-deployed Elasticsearch nodes will not have access to Chef Automate's built-in backup storage services, you must configure Elasticsearch backup settings separately from Chef Automate's primary backup settings. You can configure backups to use either the local filesystem or S3.

### Add Resolvers for Elasticsearch

In case you want to resolve the Elasticsearch node IPs dynamically using DNS servers, you can add resolvers/nameservers to the configuration.

Name Servers can be added in two ways:

1. **Add nameserver IPs:** Add the nameservers to your `config.toml` file to resolve the Elasticsearch nodes.

    ```toml
    [esgateway.v1.sys.ngx.main.resolvers]
      # Multiple resolvers can be specified by adding the resolvers in the list.
      nameservers = ["192.0.2.0:24", "198.51.100.0:24"]
    ```

1. **Set system DNS entries:** To use existing system nameserver entries from `/etc/resolv.conf`, add the following setting to `config.toml`:

    ```toml
    [esgateway.v1.sys.ngx.main.resolvers]
      enable_system_nameservers = true
    ```

If both options are set, nameserver IPs takes precedence over the system nameserver entries.

Apply the changes:

```bash
sudo chef-automate config patch config.toml
````

If you wish to reset to the default configuration or to modify the configuration:

1. Run `chef-automate config show config.toml`.
1. Open `config.toml` and remove the `esgateway.v1.sys.ngx.main.resolvers` configuration or change the values.
1. Run `chef-automate config set config.toml` to apply your changes.

## Backup External Elasticsearch

### Backup External Elasticsearch to a Local Filesystem

To configure local filesystem backups of Chef Automate data stored in an externally-deployed Elasticsearch cluster:

1. Ensure that the filesystems you intend to use for backups are mounted to the same path on all Elasticsearch master and data nodes.
1. Configure the Elasticsearch `path.repo` setting on each node as described in the [Elasticsearch documentation](https://www.elastic.co/guide/en/elasticsearch/reference/6.8/modules-snapshots.html#_shared_file_system_repository).
1. Add the following to your `config.toml`:

```toml
[global.v1.external.elasticsearch.backup]
enable = true
location = "fs"

[global.v1.external.elasticsearch.backup.fs]
# The `path.repo` setting you've configured on your Elasticsearch nodes must be
# a parent directory of the setting you configure here:
path = "/var/opt/chef-automate/backups"
```

### Backup External Elasticsearch to AWS S3

To configure AWS S3 backups of Chef Automate data stored in an externally-deployed Elasticsearch cluster:

1. Install the [`repository-s3` plugin](https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-s3.html) on all nodes in your Elasticsearch cluster.
1. If you wish to use IAM authentication to provide your Elasticsearch nodes access to the S3 bucket, you must apply the appropriate IAM policy to each host system in the cluster.
1. Configure each Elasticsearch node with a S3 client configuration containing the proper S3 endpoint, credentials, and other settings as [described in the Elasticsearch documentation](https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-s3-client.html).
1. Enable S3 backups by adding the following settings to your `config.toml`:

    ```toml
    [global.v1.external.elasticsearch.backup]
    enable = true
    location = "s3"

    [global.v1.external.elasticsearch.backup.s3]

      # bucket (required): The name of the bucket
      bucket = "<bucket name>"

      # base_path (optional):  The path within the bucket where backups should be stored
      # If base_path is not set, backups will be stored at the root of the bucket.
      base_path = "<base path>"

      # name of an s3 client configuration you create in your elasticsearch.yml
      # see https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-s3-client.html
      # for full documentation on how to configure client settings on your
      # Elasticsearch nodes
      client = "<client name>"

    [global.v1.external.elasticsearch.backup.s3.settings]
    ## The meaning of these settings is documented in the S3 Repository Plugin
    ## documentation. See the following links:
    ## https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-s3-repository.html

    ## Backup repo settings
    # compress = false
    # server_side_encryption = false
    # buffer_size = "100mb"
    # canned_acl = "private"
    # storage_class = "standard"
    ## Snapshot settings
    # max_snapshot_bytes_per_sec = "40mb"
    # max_restore_bytes_per_sec = "40mb"
    # chunk_size = "null"
    ## S3 client settings
    # read_timeout = "50s"
    # max_retries = 3
    # use_throttle_retries = true
    # protocol = "https"
    ```

### Backup External Elasticsearch to GCS

To configure Google Cloud Storage Bucket (GCS) backups of Chef Automate data stored in an externally-deployed Elasticsearch cluster:

1. Install the [`repository-gcs` plugin](https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-gcs.html) on all nodes in your Elasticsearch cluster.
1. Create a storage bucket and configure a service account to access it per the steps [described in the Elasticsearch documentation](https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-gcs-usage.html).
1. Configure each Elasticsearch node with a GCS client configuration that contains the proper GCS settings as [described in the Elasticsearch documentation](https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-gcs-client.html).
1. Enable GCS backups by adding the following settings to your `config.toml`:

    ```toml
    [global.v1.external.elasticsearch]
      enable = true
      nodes = ["https://my-es.cluster"]
      ## If multiple
      # nodes = ["https://my-es.node-1", "https://my-es.node-2", "etc..."]

    ## The following settings are required if you have Elasticsearch setup with basic auth
    #[global.v1.external.elasticsearch.auth]
    #  scheme = "basic_auth"
    #
    #[global.v1.external.elasticsearch.auth.basic_auth]
    #  username = "everyuser"
    #  password = "pass123"

    [global.v1.external.elasticsearch.backup]
      enable = true
      location = "gcs"

    [global.v1.external.elasticsearch.backup.gcs]
      bucket = "<bucket name>"
      # Client name is normally default, but can be set here if you have generated service
      # account credentials with a different client name
      client = "default"

    ## GCS Bucket Settings:
    # type = nearline
    # access control = uniform
    ```
