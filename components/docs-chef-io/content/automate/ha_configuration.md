+++
title = "High Availability Configuration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Configuration"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_configuration.md High Availability Configuration"
    weight = 12
+++

A Chef Automate configuration file contains all the settings for deploying Automate in a high availability configuration. This includes settings for backing up data; connecting to OpenSeach, Chef Infra Server, and PostgreSQL nodes; and configuring Automate deployed on AWS.

## File format

Chef Automate uses a [TOML v0.4.0 file](https://toml.io/en/v0.4.0) for configuration.

## Create the configuration file

Use the `chef-automate` CLI to generate a new configuration file with default values.

```sh
chef-automate init-config-ha
```

To create a default configuration for deployment on AWS, use the `aws` argument.

```sh
chef-automate init-config-ha aws
```

To create a default configuration file on a bastion host that is part of an existing high availability deployment, use the `existing_infra` argument.

```sh
chef-automate init-config-ha existing_infra
```

## Use the configuration file

Use the `chef-automate CLI on view, patch, and reset a configuration file.

You can use the examples below to configure your TOML file.

## Infrastructure

The following sets the basic configuration of Chef Automate’s infrastructure in a high availability deployment.

### Parameters

`secrets_key_file`

: Type: string

  Default: /hab/a2_deploy_workspace/secrets.key

  Some words that describe this.

`secrets_store_file`

: Type: string

  Default: none

  Description text.

`architecture`

: Type: string

  Default: none

  Description text.

`workspace_path`

: Type: string

  Default: none

  Description text.

`ssh_user`

: Type: string

  Default: none

  Description text.

`ssh_port`

: Type: string

  Default: none

  Description text.

`ssh_key_file`

: Type: string

  Default: none

  Description text.

`sudo_password`

: Type: string

  Default: none

  Description text.

`backup_mount`

: Type: string

  Default: /mnt/automate_backups

  Description text.

Do not modify this value.

`backup_config`

: Type: string

  Default: None

  Allowed values: file_system, object_storage.

The type of storage for backing up Chef Automate data.

If this value is set to object_storage, set the object storage settings.

### Example

```ruby
###### Automate Infrastructure ##########################################################
# The following sets the basic configuration of Chef Automate's infrastructure.
# See docs.chef.io/automate/config_toml#infrastructure for a full description of each property.
# See https://docs.aws.amazon.com/s3/ for documentation on AWS S3.
#########################################################################################

###### Parameters #######################################################################
# secrets_key_file
#     The file path to the .....
#     Default: "/hab/a2_deploy_workspace/secrets.key"
# secrets_store_file
#
# architecture
#
# workspace_path
#
# ssh_user
#
# ssh_port
#
# ssh_key_file
#
# sudo_password
#     The configured sudo password for the account.
#     This is only required if the root user has a sudo password set.
# backup_mount
#     The file path ...
#     Default: "/mnt/automate_backups"
#     Do not modify this value.
# backup_config
#     Whether to back up data to file system storage or object storage.
#     Allowed values are: "file_system" and "object_storage".
#     If set to "object_storage", add the Object Storage configuration section to your
#     config file.
#########################################################################################

[architecture.existing_infra]
secrets_key_file = ""
secrets_store_file = ""
architecture = ""
workspace_path = ""
ssh_user = ""
ssh_port = ""
ssh_key_file = ""
sudo_password = ""
backup_mount = "/mnt/automate_backups"
backup_config = ""
```

## Object Storage

### Parameters

`bucket_name`

: Type: string

  Default: none

  The name of the AWS S3 bucket to back up data to.

`access_key`

: Type: string

  Default: none

  The AWS IAM access key for accessing the S3 bucket is used to back up data.

`secret_key`

: Type: string

  Default: none

  Other param description.

`endpoint`

: Type: string

  Default: none

  Other param description.

`region`

: Type: string

  Default: none

  Other param description.

### Example

```ruby
###### Object Storage ###################################################################
# The following settings configure Chef Automate to back up data on AWS S3.
# See docs.chef.io/automate/config_toml#object-storage for a full description of each property.
# See https://docs.aws.amazon.com/s3/ for documentation on AWS S3.
# You must set "backup_config" to "object_storage" in the infrastructure settings.
#########################################################################################

###### Parameters #######################################################################
# bucket_name
#      The name of the AWS S3 bucket to back up data to.
# access_key
#      The AWS IAM access key for accessing the S3 bucket used to back up data to.
# other_param
#      Other param description.
#########################################################################################

[object_storage.config]
bucket_name = ""
access_key = ""
secret_key = ""
endpoint = ""
region = ""
```

## Chef Automate

## Chef Infra Server

This section lists the Chef Infra Server configurations that can be configured in Standalone Automate and Automate HA.

### Keys

#### nginx

`client_max_body_size`

: Type: number

  Default: 250

  The maximum accepted body size for a client request, as indicated by the `Content-Length` request header.

`ssl_protocols`

: Type: string

  Default: TLSv1.2

  The SSL protocol versions that are enabled for the Chef Infra Server API. Starting with Chef Infra Server 14.3, this value defaults to `TLSv1.2` for enhanced security. Previous releases defaulted to `TLSv1 TLSv1.1 TLSv1.2`, which allowed for less secure SSL connections. TLS 1.2 is supported on Chef Infra Client 10.16.4 and later on Linux, Unix, and macOS, and on Chef Infra Client 12.8 and later on Windows. If it is necessary to support these older end-of-life Chef Infra Client releases, set this value to `TLSv1.1 TLSv1.2`.

`worker_connections`

: Type: number

  Default: 10240

  The maximum number of simultaneous clients. Use with nginx['worker_processes'] to determine the maximum number of allowed clients.

`worker_processes`

: Type: number

  Default: 4, 2

  The number of allowed worker processes. Use with nginx['worker_connections'] to determine the maximum number of allowed clients.

`gzip`

: Type: string

  Default: ON

  Enable gzip compression.

`gzip_comp_level`

: Type: number

  Default: 2

  The compression level used with gzip, from the least amount of compression (1, fastest) to the most (2, slowest).

`gzip_http_version`

: Type: number

  Default: 1.0

  Enable gzip depending on the version of the HTTP request.

`gzip_types`

: Type: string

  Default: none

  Enable compression for the specified MIME-types.

`keepalive_timeout`

: Type: number

  Default: 65

  The amount of time (in seconds) to wait for requests on HTTP keepalive connection.

`sendfile`

: Type: string

  Default: ON

  Copy data between file descriptors when sendfile() is used.

`ssl_ciphers`

: Type: string

  Default: none

  The list of supported cipher suites that are used to establish a secure connection. To favor AES256 with ECDHE forward security, drop the RC4-SHA:RC4-MD5:RC4:RSA prefix. See this link for more information. For example:

#### opscode_erchef

`s3_url_ttl`

: Type: number

  Default: 900, 28800

  The amount of time (in seconds) before connections to the server expire. If node bootstraps are timing out, increase this setting.

`auth_skew`

: Type: number

  Default: 900

  

`authz_fanout`

: Type: number

  Default: 20

  

`authz_timeout`

: Type: number

  Default: 2000

  The amount of time (in seconds) before a request to the **oc_bifrost** service times out.

`base_resource_url`

: Type: string

  Default: :host_header

  The base URL to which the service is to return links to API resources. Use `:host_header` to ensure the URL is derived from the host header of the incoming HTTP request.

`bulk_fetch_batch_size`

: Type: number

  Default: 5

  

`cleanup_batch_size`

: Type: number

  Default: 0

  The number of nodes that may be deserialized. Currently only applies to the /search endpoint in the Chef Infra Server API. The default value is the recommended value.

`depsolver_timeout`

: Type: number

  Default: 5000

  The amount of time (in milliseconds) to wait for cookbook dependency problems to be solved.

`depsolver_worker_count`

: Type: number

  Default: 5

  The number of Ruby processes for which cookbook dependency problems are unsolved. Use the `pgrep -fl depselector` command to verify the number of depsolver workers that are running. If you are seeing 503 service unavailable errors, increase this value.

`depsolver_pooler_timeout`

: Type: number

  Default: 100000, 0

  

`depsolver_pool_queue_max`

: Type: number

  Default: 10, 50

  

`db_pool_size`

: Type: number

  Default: 40, 20

  The number of open connections to PostgreSQL that are maintained by the service. This value should be increased if failures indicate that the **oc_bifrost** service ran out of connections. This value should be tuned in conjunction with the `postgresql['max_connections']` setting for PostgreSQL.

`db_pool_queue_max`

: Type: number

  Default: 40, 20

  

`ibrowse_max_pipeline_size`

: Type: number

  Default: 1

  Setting EOL in Chef Infra Server 14.

`ibrowse_max_sessions`

: Type: number

  Default: 256

  Setting EOL in Chef Infra Server 14.

`max_request_size`

: Type: number

  Default: 4000000

  When the request body size is greater than this value, a `413 Request Entity Too Large` error is returned.

`keygen_cache_size`

: Type: number

  Default: 1000, 10

  

`reindex_batch_size`

: Type: number

  Default: 10

  The number of items to fetch from the database and send to the search index at a time.

`reindex_sleep_min_ms`

: Type: number

  Default: 500

  The minimum number of milliseconds to sleep before retrying a failed attempt to index an item. Retries are delayed a random number of milliseconds between `reindex_sleep_min_ms` and `reindex_sleep_max_ms`. Set both this and `reindex_sleep_max_ms` to 0 to retry without delay.

`reindex_sleep_max_ms`

: Type: number

  Default: 2000

  The maximum number of milliseconds to sleep before retrying a failed attempt to index an item. Retries are delayed a random number of milliseconds between `reindex_sleep_min_ms` and `reindex_sleep_max_ms`. Set both this and `reindex_sleep_min_ms` to 0 to retry without delay.

`reindex_item_retries`

: Type: number

  Default: 3

  The number of times to retry sending an object for indexing in the case of failure.

`cbv_cache_enabled`

: Type: string

  Default: FALSE

  Whether to enable cookbook version response caching. If you frequently see very long response times from `cookbook_versions` when under load, this is worth enabling. Enabling this makes it possible for a client to receive stale results. When a cookbook is updated in place (without incrementing the version), and the old response has not expired from the cache, the Infra Server will give the old response to the client. Subsequent client runs will receive the updated response.

`search_queue_mode`

: Type: string

  Default: batch

  The search index queue mode.

`s3_enabled`

: Type: string

  Default: FALSE

  

`s3_bucket_name`

: Type: string

  Default: Bookshelf

  

`s3_external_url`

: Type: string

  Default: none

  

`strict_search_result_acls`

: Type: string

  Default: FALSE

  Use to specify that search results only return objects to which an actor (user, client, etc.) has read access, as determined by ACL settings. This affects all searches. When true, the performance of the Chef management console may increase because it enables the Chef management console to skip redundant ACL checks. To ensure the Chef management console is configured properly, after this setting has been applied with a `chef-server-ctl` reconfigure run `chef-manage-ctl` reconfigure to ensure the Chef management console also picks up the setting.

`enable_ibrowse_traces`

: Type: string

  Default: FALSE

  Use to configure ibrowse logging for the `opscode_erchef` service.

`s3_url_expiry_window_size`

: Type: string

  Default: [100, percent]

  The frequency at which unique URLs are generated. This value may be a specific amount of time, i.e. `15m` (fifteen minutes) or a percentage of the value of s3_url_ttl, i.e. `10%`.

#### oc_chef_authz

`http_queue_max`

: Type: number

  Default: 200

  

`http_max_count`

: Type: string

  Default: 100

  The maximum worker count for the HTTP connection pool that is used by the data collector.

`http_init_count`

: Type: string

  Default: 100

  

#### data_collector

`timeout`

: Type: number

  Default: 30000

  The amount of time (in milliseconds) before a request to the data collector API times out.

`http_init_count`

: Type: number

  Default: 25

  The initial worker count for the HTTP connection pool that is used by the data collector.

`http_max_count`

: Type: number

  Default: 100

  The maximum worker count for the HTTP connection pool that is used by the data collector.

`http_max_age`

: Type: string

  Default: {70, sec}

  The maximum connection worker age (in seconds) for the HTTP connection pool that is used by the data collector.

`http_cull_interval`

: Type: string

  Default: {1, min}

  The maximum cull interval (in minutes) for the HTTP connection pool that is used by the data collector.

`http_max_connection_duration`

: Type: string

  Default: {70, sec}

  The maximum connection duration (in seconds) for the HTTP connection pool that is used by the data collector.

`ibrowse_options`

: Type: string

  Default: [{connect_timeout, 10000}]

  An array of comma-separated key-value pairs of ibrowse options for the HTTP connection pool that is used by the data collector.

#### oc_bifrost

`db_pool_queue_max`

: Type: number

  Default: 50

  

`extended_perf_log`

: Type: string

  Default: TRUE

  

#### bookshelf

`stream_download`

: Type: string

  Default: TRUE

  Enable stream downloading of cookbooks. This setting (when `true`) typically results in improved cookbook download performance, especially with the memory usage of the bookshelf service and the **behavior** of load balancers and proxies in-between Chef Infra Client and the Chef Infra Server.

`aws_access_id`

: Type: string

  Default: none

  

`aws_secret_key`

: Type: string

  Default: none

  

`log_rotation`

: Type: string

  Default: (`file_maxbytes`: 104857600, `num_to_keep`: 10)

  The log rotation policy for this service. Log files are rotated when they exceed `file_maxbytes`. The maximum number of log files in the rotation is defined by num_to_keep.

`storage_type`

: Type: string

  Default: sql

  Determines where cookbooks are stored. In instances that require cookbooks to be stored within a SQL backend, such as in a high availability setup, you must set `storage_type` to `:sql:`.

`vip`

: Type: string

  Default: 127.0.0.1

  The virtual IP address. This may point to an external storage location, such as Amazon EC2.

#### oc_chef_wm

`health_ping_timeout`

: Type: number

  Default: 400

  ## PostgreSQL

The PostgreSQL node in Automate HA provides various configuration options you can configure to customize its behavior and meet specific requirements. This guide documents all the configurations that you can patch.

Patch the below configuration to PostgreSQL nodes. Please add the values you want to patch to a `config.toml` file and run the `chef-automate config patch config.toml --pg` from the bastion.

### Parameters

#### Logging

`log_level`

: Type: string

  Default: ERROR

  `log_level` controls which message levels are written to the server log. Valid values are DEBUG5, DEBUG4, DEBUG3, DEBUG2, DEBUG1, INFO, NOTICE, WARNING, ERROR, LOG, FATAL, and PANIC. The default is WARNING.

`log_line_prefix`

: Type: string

  Default: %t [%p]: [%l-1] user=%u,db=%d,client=%h %r (%x:%e)

  `log_line_prefix` is a print style string output at the beginning of each log line.

`logging_collector`

: Type: string

  Default: on

  `logging_collector` enables the logging collector, which is a background process that captures log messages sent to stderr and redirects them into log files.

#### Checkpoints

`checkpoint_timeout`

: Type: string

  Default: 5min

  `checkpoint_timeout` is the maximum time between automatic WAL checkpoints. The valid range is between 30 seconds and one day. The default is five minutes (5min). Increasing this parameter can increase the amount of time needed for crash recovery.

`max_wal_size`

: Type: string

  Default: 1GB

  `max_wal_size` is the maximum size to let the WAL grow during automatic checkpoints. The default is 1 GB. Increasing this parameter can increase the amount of time needed for crash recovery. This parameter can only be set in the PostgreSQL.conf file or the server command line.

`min_wal_size`

: Type: string

  Default: 80MB

  `min_wal_size` can ensure enough WAL space is reserved to handle spikes in WAL usage, for example, when running large batch jobs. If this value is specified without units, it is taken as megabytes. The default is 80 MB.

#### Wal Keep Size

`wal_keep_size`

: Type: number

  Default: 1600

  `wal_keep_size` specifies the minimum size of past log file segments kept in the pg_wal directory if a standby server needs to fetch them for streaming replication. If wal_keep_size is zero (the default), the system doesn’t keep extra segments for standby purposes. Hence, the number of old WAL segments available to standby servers is a function of the location of the previous checkpoint and the status of WAL archiving.

#### Lock Management

`max_locks_per_transaction`

: Type: number

  Default: 64

  The shared lock table tracks locks on max_locks_per_transaction * (max_connections + max_prepared_transactions) objects (e.g., tables); hence, no more than this many distinct objects can be locked at any time. This parameter controls the average number of object locks allocated for each transaction; individual transactions can lock more objects as long as the locks of all transactions fit in the lock table. This is not the number of rows that can be locked; that value is unlimited. The default is 64.

When running a standby server, you must set this parameter to the same or higher value than on the master server. Otherwise, queries will not be allowed on the standby server.

#### Max Connections

`max_connections`

: Type: number

  Default: 350

  In the above snippet, max_connections determines the maximum number of concurrent connections to the database server. The default for Automate is 350 connections.

When running a standby server, you must set this parameter to the same or higher value than on the master server. Otherwise, queries will not be allowed on the standby server.

#### Pg Dump

This section configures `pg_demp`, a PostgreSQL utility for performing database backups.

`enable`

: Type: string

  Default: true

  It enables `pg_demp`.

`path`

: Type: string

  Default: none

  It specified the path where the backups should be stored.

#### Replication

This section configures replication settings.

`lag_health_threshold`

: Type: number

  Default: 20480

  It sets the lag health threshold to 20480 bytes, i.e., the maximum allowed replication lag.

`max_replay_lag_before_restart_s`

: Type: number

  Default: 180

  It specifies the maximum replay lag before restarting the replication.

`name`

: Type: string

  Default: replication

  It specifies the name of the replication.

`password`

: Type: string

  Default: replication

  It specifies the password for the replication.

#### SSL

This section configures SSL/TLS settings.

`enable`

: Type: string

  Default: true

  It enables the SSL.

`issuer_cert`

: Type: string

  Default: none

  It specifies the root CA (issuer) certificate.

`ssl_cert`

: Type: string

  Default: none

  It specifies the public key certificate.

`ssl_key`

: Type: string

  Default: none

  It specifies the private key.

`tls_ciphers`

: Type: string

  Default: none

  It specifies the allowed TLS ciphers.

#### User

This section specified the username and password for the superuser (administrator) account.

`name`

: Type: string

  Default: none

  It specifies the name of the superuser.

`password`

: Type: string

  Default: none

  It specified the password of the superuser.

#### Wal Archive

This section configures WAL archiving.

`enable`

: Type: string

  Default: false

  It specifies whether WAL archiving is enabled (false in this case).

`path`

: Type: string

  Default: none

  It specifies the path where archived WAL files should be stored.

### Example

```ruby
checkpoint_timeout = "5min"
host = "0.0.0.0"
log_level = "ERROR"
log_line_prefix = "%t [%p]: [%l-1] user=%u,db=%d,client=%h %r (%x:%e)"
logging_collector = "on"
max_connections = 350
max_locks_per_transaction = 64
max_wal_size = "1GB"
min_wal_size = "80MB"
port = 5432
print_db_statistics = true
wal_keep_size = 1600
[pg_dump]
enable = true
path = "/mnt/automate_backups/postgresql/pg_dump"
[replication]
lag_health_threshold = 20480
max_replay_lag_before_restart_s = 180
name = "replication"
password = "replication"
[ssl]
enable = true
issuer_cert = "----Enter Root CA----"
ssl_cert = "----Enter Public Key----"
ssl_key = "----Enter Private Key----"
tls_ciphers = "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256"
[superuser]
name = "admin"
password = "admin"
[wal_archive]
enable = false
path = "/mnt/automate_backups/postgresql/archive"
```

## OpenSearch

The OpenSearch node in Chef Automate HA provides various configuration options that is patched to customize its behavior and meet specific requirements. Patch the below configuration to OpenSearch nodes. Please add the values you want to patch to a config.toml file and run the chef-automate config patch config.toml --os from the bastion node.

### Parameters

#### Action

It configures action settings.

`destructive_requires_name`

: Type: string

  Default: true

  Setting `destructive_requires_name` to **true** means that destructive actions, such as deleting indices or templates, require an explicit name to prevent accidental deletions.

#### Bootstrap

This section disables swapping (along with memlock).

`memory_lock`

: Type: string

  Default: false

  Swapping can dramatically decrease performance and stability, so you should ensure it is disabled on production clusters.

#### Cluster

This section configures cluster settings.

`name`

: Type: string

  Default: opensearch

  It sets the name of the OpenSearch cluster to “opensearch”.

`max_shards_per_node`

: Type: number

  Default: 20000

  We can use this setting to set the `max_shards_per_node` value for OpenSearch.

#### Discovery

This section configures discovery settings. It sets the minimum number of master-eligible nodes required to form a cluster, specifies the unicast hosts for node discovery, and sets the ping timeout.

`minimum_master_nodes`

: Type: number

  Default: 2

  Set `minimum_master_nodes` to prevent the split brain by configuring the majority of nodes (total number of nodes / 2 + 1):

`ping_unicast_hosts`

: Type: string

  Default: [“127.0.0.1”, “[::1]”]

  Set ping_unicast_hosts to pass an initial list of hosts to perform discovery when a new node starts.

#### Gateway

`recover_after_nodes`

: Type: string

  Default: none

  Set `recover_after_nodes` to block initial recovery after a full cluster restart until N nodes start.

#### Logger

This section configures logger settings.

`level`

: Type: string

  Default: info

  Allowed levels are trace, debug, info, warn, error, and fatal.

#### Node

`max_local_storage_nodes`

: Type: number

  Default: 1

  Use `max_local_storage_nodes` to disable starting multiple nodes on a single system.

`name`

: Type: string

  Default: name

  Use a descriptive name for the node by setting the `name` field.

#### OpenSearch Auth

This section configures OpenSearch authentication settings.

`admin_password`

: Type: string

  Default: none

  It sets the admin password.

`admin_username`

: Type: string

  Default: none

  It sets the admin username.

`hashed_password`

: Type: string

  Default: none

  It provides the hashed version of the password.

#### Path

`data`

: Type: string

  Default: none

  Use data to set the path to the directory where to store the data (separate multiple locations by comma).

`logs`

: Type: string

  Default: none

  Use logs to set the path to your log files.

`repo`

: Type: string

  Default: none

  Use the repo to register the snapshot repository using OpenSearch. It is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location in the path.repo setting on all master and data nodes.

#### Plugin Security

This section configures security plugin settings.

`allow_default_init_securityindex`

: Type: string

  Default: true

  Set the value to `true` to allow the default initialization of the security index.

`allow_unsafe_democertificates`

: Type: string

  Default: true

  Set the value to `true` to allow the default initialization of the unsafe demo certificates.

`check_snapshot_restore_write_privileges`

: Type: string

  Default: true

  Set the value to `true` to check the snapshot and restore write privileges.

`enable_snapshot_restore_privilege`

: Type: string

  Default: true

  Set the value to `true` to enable snapshot and restore privileges.

`nodes_dn`

: Type: string

  Default: none

  It specifies the nodes' distinguished name (DNs).

#### Plugin Security Audit

This section configures security audit settings.

`type`

: Type: string

  Default: internal_opensearch

  It specifies the type of audit logging as "internal_opensearch"

#### Plugin Security Authcz

This section specifies the distinguished name (DN) of the admin user.

`admin_dn`

: Type: string

  Default: none

  Specify the distinguished name (DN) of the admin user.

#### Plugin Security Restapi

This section configures security REST API settings.

`roles_enabled`

: Type: string

  Default: none

  It enables SSL/TLS, specifying the certificate’s file paths, private key, and trusted CA certificates.

#### Plugin Security SSL Transport

This section configures SSL/TLS settings for transport layer communication.

`enforce_hostname_verification`

: Type: string

  Default: false

  It disables the hostname verification.

`pemcert_filepath`

: Type: string

  Default: none

  It specifies the file paths for the certificate.

`pemkey_filepath`

: Type: string

  Default: none

  It specifies the private key.

`pemtrustedcas_filepath`

: Type: string

  Default: none

  It specifies the trusted CA certificates.

`resolve_hostname`

: Type: string

  Default: false

  It disables/enables the hostname resolution.

#### Plugin Security System Indices

This section configures system indices for the security plugin.

`cloud_aws_signer`

: Type: string

  Default: none

  It specifies the cloud aws signer.

`enabled`

: Type: string

  Default: true

  It enables/disables the security plugin.

`indices`

: Type: string

  Default: none

  It specifies the system indices that are enabled for various functionalities.

#### Runtime

This section configures runtime settings.

`es_java_opts`

: Type: string

  Default: none

  

`es_startup_sleep_time`

: Type: string

  Default: none

  

`g1ReservePercent`

: Type: number

  Default: 25

  

`initiatingHeapOccupancyPercent`

: Type: number

  Default: 15

  

`maxHeapsize`

: Type: string

  Default: 2g

  

`max_locked_memory`

: Type: string

  Default: unlimited

  

`max_open_files`

: Type: string

  Default: none

  

`minHeapsize`

: Type: string

  Default: 2g

  

#### S3 Client Default

This section configures the default S3 client settings.

`endpoint`

: Type: string

  Default: s3.amazonaws.com

  It specifies the S3 endpoint.

`max_retries`

: Type: string

  Default: 3

  It specifies the maximum number of retries.

`protocol`

: Type: string

  Default: https

  It specifies the protocol (HTTPS).

`read_timeout`

: Type: string

  Default: 60s

  It specifies the read timeout.

`use_throttle_retries`

: Type: string

  Default: true

  It sets whether to use throttle retries.

#### TLS

This section configures TLS settings.

`admin_cert`

: Type: string

  Default: none

  It specifies the admin public key.

`admin_key`

: Type: string

  Default: none

  It specifies the admin private key.

`rootCA`

: Type: string

  Default: none

  It specifies the root CA certificate.

`ssl_cert`

: Type: string

  Default: none

  It specifies the public key of the SSL certificate.

`ssl_key`

: Type: string

  Default: none

  It specifies the SSL private key.

### Example

```ruby
[action]
destructive_requires_name = "true"
[bootstrap]
memory_lock = false
[cluster]
name = "opensearch"
[cluster.routing.allocation]
awareness_attributes = ""
node_concurrent_recoveries = "2"
node_initial_primaries_recoveries = "4"
same_shard_host = "false"
[deprecated]
external_os = false
[discovery]
minimum_master_nodes = 2
# Example: ping_unicast_hosts = ["172.31.192.70", "172.31.192.132", "172.31.192.248"]
ping_unicast_hosts = ["Os Node IP 1", "Os Node IP 2"]
zen_fd_ping_timeout = "30s"
[gateway]
expected_data_nodes = "0"
expected_master_nodes = "0"
expected_nodes = "0"
recover_after_nodes = ""
recover_after_time = ""
[indices.breaker]
fielddata_limit = "60%"
fielddata_overhead = "1.03"
request_limit = "40%"
request_overhead = "1"
total_limit = "95%"
[indices.fielddata]
cache_size = ""
[indices.recovery]
max_bytes_per_sec = "20mb"
[logger]
level = "info"
[network]
host = "172.31.192.248"
port = 9200
[node]
data = true
master = true
max_local_storage_nodes = 1
name = ""
rack_id = ""
zone = ""
[opensearch_auth]
admin_password = "admin"
admin_username = "admin"
hashed_password = "$2a$12$yObdBmd8JFy2ar7nRjv46OYs3P3q5uB9llsrLRppWyAO/YOQ2JW3m"
[path]
data = ""
logs = "logs"
repo = ""
[plugins.security]
allow_default_init_securityindex = true
allow_unsafe_democertificates = true
check_snapshot_restore_write_privileges = true
enable_snapshot_restore_privilege = true
# Example: nodes_dn = "- CN=chefnode,O=Chef Software Inc,L=Seattle,ST=Washington,C=US"
nodes_dn = "- <Common Name of Public Key>"
[plugins.security.audit]
type = "internal_opensearch"
[plugins.security.authcz]
# Example: admin_dn = "- CN=chefadmin,O=Chef Software Inc,L=Seattle,ST=Washington,C=US"
admin_dn = "- <Common Name of Admin Public Key>"
[plugins.security.restapi]
roles_enabled = "[\"all_access\", \"security_rest_api_access\"]"
[plugins.security.ssl.http]
enabled = true
pemcert_filepath = "certificates/node1.pem"
pemkey_filepath = "certificates/node1-key.pem"
pemtrustedcas_filepath = "certificates/root-ca.pem"
[plugins.security.ssl.transport]
enforce_hostname_verification = false
pemcert_filepath = "certificates/node1.pem"
pemkey_filepath = "certificates/node1-key.pem"
pemtrustedcas_filepath = "certificates/root-ca.pem"
resolve_hostname = false
[plugins.security.system_indices]
cloud_aws_signer = ""
enabled = true
indices = "[\".opendistro-alerting-config\", \".opendistro-alerting-alert*\", \".opendistro-anomaly-results*\", \".opendistro-anomaly-detector*\", \".opendistro-anomaly-checkpoints\", \".opendistro-anomaly-detection-state\", \".opendistro-reports-*\", \".opendistro-notifications-*\", \".opendistro-notebooks\", \".opensearch-observability\", \".opendistro-asynchronous-search-response*\", \".replication-metadata-store\"]"
[runtime]
es_java_opts = ""
es_startup_sleep_time = ""
g1ReservePercent = "25"
initiatingHeapOccupancyPercent = "15"
maxHeapsize = "2g"
max_locked_memory = "unlimited"
max_open_files = ""
minHeapsize = "2g"
[s3.client.default]
endpoint = "s3.amazonaws.com"
max_retries = "3"
protocol = "https"
read_timeout = "60s"
use_throttle_retries = true
[tls]
admin_cert = "----Enter Admin Public Key----"
admin_key = "----Enter Admin Private Key----"
rootCA = "----Enter Root CA----"
ssl_cert = "----Enter Public Key----"
ssl_key = "Enter Private Key----"
[transport]
port = 9300
```


## AWS

## External database

## Event Gateway
