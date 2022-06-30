+++
title = "OpenSearch"

date = 2022-01-04T12:09:09-08:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Configure OpenSearch"
    identifier = "automate/configuring_automate/opensearch/opensearch.md OpenSearch"
    parent = "automate/configuring_automate/opensearch"
    weight = 10
+++

You can configure Chef Automate to use OpenSearch clusters that are not deployed via Chef Automate itself.

## Configure External OpenSearch

These configuration directions are intended for the initial deployment of Chef Automate.

**Automate supports OpenSearch connection over HTTPS or HTTP**

Add the following to your `config.toml` for HTTPS connection:

```toml
[global.v1.external.opensearch]
  enable = true
  nodes = ["https://opensearch1.example:9200", "https://opensearch2.example:9200", "..." ]

# Uncomment and fill out if using external opensearch with SSL and/or basic auth
[global.v1.external.opensearch.auth]
  scheme = "basic_auth"
[global.v1.external.opensearch.auth.basic_auth]
## Create this opensearch user before starting the Chef Automate deployment;
## Chef Automate assumes it exists.
  username = "<admin username>"
  password = "<admin password>"
[global.v1.external.opensearch.ssl]
# Specify either a root_cert or a root_cert_file
  root_cert = """$(cat </path/to/cert_file.crt>)"""
# server_name = "<opensearch server name>"

# Uncomment and fill out if using external OpenSearch that uses hostname-based routing/load balancing
# [esgateway.v1.sys.ngx.http]
#  proxy_set_header_host = "<your external es hostname>:1234"

# Uncomment and add to change the ssl_verify_depth for the root cert bundle
#  ssl_verify_depth = "2"
```

Add the following to your `config.toml` for HTTP connection:
```toml
[global.v1.external.opensearch]
  enable = true
  nodes = ["http://opensearch1.example:9200", "http://opensearch2.example:9200", "..." ]

# Uncomment and fill out if using external opensearch with SSL and/or basic auth
[global.v1.external.opensearch.auth]
  scheme = "basic_auth"
[global.v1.external.opensearch.auth.basic_auth]
## Create this opensearch user before starting the Chef Automate deployment;
## Chef Automate assumes it exists.
  username = "<admin username>"
  password = "<admin password>"
```

Because externally-deployed OpenSearch nodes will not have access to Chef Automate's built-in backup storage services, you must configure OpenSearch backup settings separately from Chef Automate's primary backup settings. You can configure backups to use either the local filesystem or S3.

### Add Resolvers for OpenSearch

In case you want to resolve the OpenSearch node IPs dynamically using DNS servers, you can add resolvers/nameservers to the configuration.

Name Servers can be added in two ways:

1. **Add nameserver IPs:** Add the nameservers to your `config.toml` file to resolve the OpenSearch nodes.

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

## Backup External OpenSearch

### Backup External OpenSearch to a Local Filesystem

To configure local filesystem backups of Chef Automate data stored in an externally-deployed OpenSearch cluster:

1. Ensure that the filesystems you intend to use for backups are mounted to the same path on all OpenSearch master and data nodes.
1. Configure the OpenSearch `path.repo` setting on each node as described in the [OpenSearch documentation](https://opensearch.org/docs/latest/monitoring-plugins/pa/reference/).
1. Add the following to your `config.toml`:

```toml
[global.v1.external.opensearch.backup]
enable = true
location = "fs"

[global.v1.external.opensearch.backup.fs]
# The `path.repo` setting you've configured on your OpenSearch nodes must be
# a parent directory of the setting you configure here:
path = "/var/opt/chef-automate/backups"
```

### Backup External OpenSearch to AWS S3

To configure AWS S3 backups of Chef Automate data stored in an externally-deployed OpenSearch cluster:

1. Install the `repository-s3` on all nodes in your OpenSearch cluster.
1. If you wish to use IAM authentication to provide your OpenSearch nodes access to the S3 bucket, you must apply the appropriate IAM policy to each host system in the cluster.
1. Configure each OpenSearch node with a S3 client configuration containing the proper S3 endpoint, credentials, and other settings as described in the OpenSearch documentation.
1. Enable S3 backups by adding the following settings to your `config.toml`:

    ```toml
    [global.v1.external.opensearch.backup]
    enable = true
    location = "s3"

    [global.v1.external.opensearch.backup.s3]

      # bucket (required): The name of the bucket
      bucket = "<bucket name>"

      # base_path (optional):  The path within the bucket where backups should be stored
      # If base_path is not set, backups will be stored at the root of the bucket.
      base_path = "<base path>"

      # name of an s3 client configuration you create in your opensearch.yml
      # for full documentation on how to configure client settings on your
      # OpenSearch nodes
      client = "<client name>"

    [global.v1.external.opensearch.backup.s3.settings]
    ## The meaning of these settings is documented in the S3 Repository Plugin
    ## documentation.

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

### Backup External OpenSearch to GCS

To configure Google Cloud Storage Bucket (GCS) backups of Chef Automate data stored in an externally-deployed OpenSearch cluster:

1. Install the `repository-gcs` plugin on all nodes in your OpenSearch cluster.
1. Create a storage bucket and configure a service account to access it per the steps described in the OpenSearch documentation.
1. Configure each OpenSearch node with a GCS client configuration that contains the proper GCS settings as described in the OpenSearch documentation.
1. Enable GCS backups by adding the following settings to your `config.toml`:

    ```toml
    [global.v1.external.opensearch]
      enable = true
      nodes = ["https://my-es.cluster"]
      ## If multiple
      # nodes = ["https://my-es.node-1", "https://my-es.node-2", "etc..."]

    ## The following settings are required if you have OpenSearch setup with basic auth
    #[global.v1.external.opensearch.auth]
    #  scheme = "basic_auth"
    #
    #[global.v1.external.opensearch.auth.basic_auth]
    #  username = "everyuser"
    #  password = "pass123"

    [global.v1.external.opensearch.backup]
      enable = true
      location = "gcs"

    [global.v1.external.opensearch.backup.gcs]
      bucket = "<bucket name>"
      # Client name is normally default, but can be set here if you have generated service
      # account credentials with a different client name
      client = "default"

    ## GCS Bucket Settings:
    # type = nearline
    # access control = uniform
    ```
