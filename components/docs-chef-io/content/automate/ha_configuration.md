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

Type: string

Default: /hab/a2_deploy_workspace/secrets.key

Some words that describe this.

`secrets_store_file`

Type: string

Default: none

Description text.

`architecture`

Type: string

Default: none

Description text.

`workspace_path`

Type: string

Default: none

Description text.

`ssh_user`

Type: string

Default: none

Description text.

`ssh_port`

Type: string

Default: none

Description text.

`ssh_key_file`

Type: string

Default: none

Description text.

`sudo_password`

Type: string

Default: none

Description text.

`backup_mount`

Type: string

Default: /mnt/automate_backups

Description text.

Do not modify this value.

`backup_config`

Type: string

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

#### bucket_name

Type: string

Default: none

The name of the AWS S3 bucket to back up data to.

#### access_key

Type: string

Default: none

The AWS IAM access key for accessing the S3 bucket is used to back up data.

#### secret_key

Type: string

Default: none

Other param description.

#### endpoint

Type: string

Default: none

Other param description.

#### region

Type: string

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

## PostgreSQL

The PostgreSQL node in Automate HA provides various configuration options you can configure to customize its behavior and meet specific requirements. This guide documents all the configurations that you can patch.

Patch the below configuration to PostgreSQL nodes. Please add the values you want to patch to a `config.toml` file and run the `chef-automate config patch config.toml --pg` from the bastion.

### Logging

#### log_level

Type: string

Default: ERROR

`log_level` controls which message levels are written to the server log. Valid values are DEBUG5, DEBUG4, DEBUG3, DEBUG2, DEBUG1, INFO, NOTICE, WARNING, ERROR, LOG, FATAL, and PANIC. The default is WARNING.

#### log_line_prefix

Type: string

Default: %t [%p]: [%l-1] user=%u,db=%d,client=%h %r (%x:%e)

`log_line_prefix` is a print style string output at the beginning of each log line.

#### logging_collector

Type: string

Default: on

`logging_collector` enables the logging collector, which is a background process that captures log messages sent to stderr and redirects them into log files.

### Checkpoints

#### checkpoint_timeout

Type: string

Default: 5min

`checkpoint_timeout` is the maximum time between automatic WAL checkpoints. The valid range is between 30 seconds and one day. The default is five minutes (5min). Increasing this parameter can increase the amount of time needed for crash recovery.

#### max_wal_size

Type: string

Default: 1GB

`max_wal_size` is the maximum size to let the WAL grow during automatic checkpoints. The default is 1 GB. Increasing this parameter can increase the amount of time needed for crash recovery. This parameter can only be set in the PostgreSQL.conf file or the server command line.

#### min_wal_size

Type: string

Default: 80MB

`min_wal_size` can ensure enough WAL space is reserved to handle spikes in WAL usage, for example, when running large batch jobs. If this value is specified without units, it is taken as megabytes. The default is 80 MB.

### Wal Keep Size

#### wal_keep_size

Type: number

Default: 1600

`wal_keep_size` specifies the minimum size of past log file segments kept in the pg_wal directory if a standby server needs to fetch them for streaming replication. If wal_keep_size is zero (the default), the system doesn’t keep extra segments for standby purposes. Hence, the number of old WAL segments available to standby servers is a function of the location of the previous checkpoint and the status of WAL archiving.

### Lock Management

### max_locks_per_transaction

Type: number

Default: 64

The shared lock table tracks locks on max_locks_per_transaction * (max_connections + max_prepared_transactions) objects (e.g., tables); hence, no more than this many distinct objects can be locked at any time. This parameter controls the average number of object locks allocated for each transaction; individual transactions can lock more objects as long as the locks of all transactions fit in the lock table. This is not the number of rows that can be locked; that value is unlimited. The default is 64.

When running a standby server, you must set this parameter to the same or higher value than on the master server. Otherwise, queries will not be allowed on the standby server.

### Max Connections

#### max_connections

Type: number

Default: 350

In the above snippet, max_connections determines the maximum number of concurrent connections to the database server. The default for Automate is 350 connections.

When running a standby server, you must set this parameter to the same or higher value than on the master server. Otherwise, queries will not be allowed on the standby server.

### Pg Dump

This section configures `pg_demp`, a PostgreSQL utility for performing database backups.

#### enable

Type: string

Default: true

It enables `pg_demp`.

#### path

Type: string

Default: none

It specified the path where the backups should be stored.

### Replication

This section configures replication settings.

#### lag_health_threshold

Type: number

Default: 20480

It sets the lag health threshold to 20480 bytes, i.e., the maximum allowed replication lag.

#### max_replay_lag_before_restart_s

Type: number

Default: 180

It specifies the maximum replay lag before restarting the replication.

#### name

Type: string

Default: replication

It specifies the name of the replication.

#### password

Type: string

Default: replication

It specifies the password for the replication.

### SSL

This section configures SSL/TLS settings.

#### enable

Type: string

Default: true

It enables the SSL.

#### issuer_cert

Type: string

Default: none

It specifies the root CA (issuer) certificate.

#### ssl_cert

Type: string

Default: none

It specifies the public key certificate.

#### ssl_key

Type: string

Default: none

It specifies the private key.

#### tls_ciphers

Type: string

Default: none

It specifies the allowed TLS ciphers.

### User

This section specified the username and password for the superuser (administrator) account.

#### name

Type: string

Default: none

It specifies the name of the superuser.

#### password

Type: string

Default: none

It specified the password of the superuser.

### Wal Archive

This section configures WAL archiving.

#### enable

Type: string

Default: false

It specifies whether WAL archiving is enabled (false in this case).

#### path

Type: string

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

### Action

It configures action settings.

#### destructive_requires_name

Type: string

Default: true

Setting `destructive_requires_name` to **true** means that destructive actions, such as deleting indices or templates, require an explicit name to prevent accidental deletions.

### Bootstrap

This section disables swapping (along with memlock).

#### memory_lock

Type: string

Default: false

Swapping can dramatically decrease performance and stability, so you should ensure it is disabled on production clusters.

### Cluster

This section configures cluster settings.

#### name

Type: string

Default: opensearch

It sets the name of the OpenSearch cluster to “opensearch”.

#### max_shards_per_node

Type: number

Default: 20000

We can use this setting to set the `max_shards_per_node` value for OpenSearch.

### Discovery

This section configures discovery settings. It sets the minimum number of master-eligible nodes required to form a cluster, specifies the unicast hosts for node discovery, and sets the ping timeout.

#### minimum_master_nodes

Type: number

Default: 2

Set `minimum_master_nodes` to prevent the split brain by configuring the majority of nodes (total number of nodes / 2 + 1):

#### ping_unicast_hosts

Type: string

Default: [“127.0.0.1”, “[::1]”]

Set ping_unicast_hosts to pass an initial list of hosts to perform discovery when a new node starts.

### Gateway

#### recover_after_nodes

Type: string

Default: none

Set `recover_after_nodes` to block initial recovery after a full cluster restart until N nodes start.

### Logger

This section configures logger settings.

#### level

Type: string

Default: info

Allowed levels are trace, debug, info, warn, error, and fatal.

### Node

#### max_local_storage_nodes

Type: number

Default: 1

Use `max_local_storage_nodes` to disable starting multiple nodes on a single system.

#### name

Type: string

Default: name

Use a descriptive name for the node by setting the `name` field.

### OpenSearch Auth

This section configures OpenSearch authentication settings.

#### admin_password

Type: string

Default: none

It sets the admin password.

#### admin_username

Type: string

Default: none

It sets the admin username.

#### hashed_password

Type: string

Default: none

It provides the hashed version of the password.

### Path

#### data

Type: string

Default: none

Use data to set the path to the directory where to store the data (separate multiple locations by comma).

#### logs

Type: string

Default: none

Use logs to set the path to your log files.

#### repo

Type: string

Default: none

Use the repo to register the snapshot repository using OpenSearch. It is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location in the path.repo setting on all master and data nodes.

### Plugin Security

This section configures security plugin settings.

#### allow_default_init_securityindex

Type: string

Default: true

Set the value to `true` to allow the default initialization of the security index.

#### allow_unsafe_democertificates

Type: string

Default: true

Set the value to `true` to allow the default initialization of the unsafe demo certificates.

#### check_snapshot_restore_write_privileges

Type: string

Default: true

Set the value to `true` to check the snapshot and restore write privileges.

#### enable_snapshot_restore_privilege

Type: string

Default: true

Set the value to `true` to enable snapshot and restore privileges.

#### nodes_dn

Type: string

Default: none

It specifies the nodes' distinguished name (DNs).

### Plugin Security Audit

This section configures security audit settings.

#### type

Type: string

Default: internal_opensearch

It specifies the type of audit logging as "internal_opensearch"

### Plugin Security Authcz

This section specifies the distinguished name (DN) of the admin user.

#### admin_dn

Type: string

Default: none

Specify the distinguished name (DN) of the admin user.

### Plugin Security Restapi

This section configures security REST API settings.

#### roles_enabled

Type: string

Default: none

It enables SSL/TLS, specifying the certificate’s file paths, private key, and trusted CA certificates.

### Plugin Security SSL Transport

This section configures SSL/TLS settings for transport layer communication.

#### enforce_hostname_verification

Type: string

Default: false

It disables the hostname verification.

#### pemcert_filepath

Type: string

Default: none

It specifies the file paths for the certificate.

#### pemkey_filepath

Type: string

Default: none

It specifies the private key.

#### pemtrustedcas_filepath

Type: string

Default: none

It specifies the trusted CA certificates.

#### resolve_hostname

Type: string

Default: false

It disables/enables the hostname resolution.

### Plugin Security System Indices

This section configures system indices for the security plugin.

#### cloud_aws_signer

Type: string

Default: none

It specifies the cloud aws signer.

#### enabled

Type: string

Default: true

It enables/disables the security plugin.

#### indices

Type: string

Default: none

It specifies the system indices that are enabled for various functionalities.

### Runtime

This section configures runtime settings.

#### es_java_opts

Type: string

Default: none



#### es_startup_sleep_time

Type: string

Default: none



#### g1ReservePercent

Type: number

Default: 25



#### initiatingHeapOccupancyPercent

Type: number

Default: 15



#### maxHeapsize

Type: string

Default: 2g



#### max_locked_memory

Type: string

Default: unlimited



#### max_open_files

Type: string

Default: none



#### minHeapsize

Type: string

Default: 2g



### S3 Client Default

This section configures the default S3 client settings.

#### endpoint

Type: string

Default: s3.amazonaws.com

It specifies the S3 endpoint.

#### max_retries

Type: string

Default: 3

It specifies the maximum number of retries.

#### protocol

Type: string

Default: https

It specifies the protocol (HTTPS).

#### read_timeout

Type: string

Default: 60s

It specifies the read timeout.

#### use_throttle_retries

Type: string

Default: true

It sets whether to use throttle retries.

### TLS

This section configures TLS settings.

#### admin_cert

Type: string

Default: none

It specifies the admin public key.

#### admin_key

Type: string

Default: none

It specifies the admin private key.

#### rootCA

Type: string

Default: none

It specifies the root CA certificate.

#### ssl_cert

Type: string

Default: none

It specifies the public key of the SSL certificate.

#### ssl_key

Type: string

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

Type: string

Default: none

Type: string

Default: none

Type: string

Default: none

Type: string

Default: none

Type: string

Default: none

Type: string

Default: none

Type: string

Default: none






## AWS

## External database

## Event Gateway
