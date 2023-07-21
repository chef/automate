+++
title = "HA OpenSearch Node Config"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA OpenSearch Node Config"
    parent = "automate/deploy_high_availability/configuration"
    identifier = "automate/deploy_high_availability/configuration/config_opensearch.md HA OpenSearch Node Config"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

## Configurations

The OpenSearch node in Automate HA provides various configuration options that is patched to customize its behavior and meet specific requirements. This guide documents all the configurations that you can patch.

The detailed document about how these individual properties affect the system is at [Official OpenSearch docs](https://opensearch.org/docs/1.3/)

Patch the below configuration to OpenSearch nodes. Please add the values you want to patch to a `config.toml` file and run the `chef-automate config patch config.toml --os` from the bastion node.

Certainly! Here's an explanation of each section in the OpenSearch TOML configuration file:

### Action

```toml
[action]
destructive_requires_name = "true"
```

- This section configures action settings. Setting `destructive_requires_name` to `true` means that destructive actions, such as deleting indices or templates, require an explicit name to prevent accidental deletions.

### Bootstrap

```toml
[bootstrap]
memory_lock = false
```

Disables swapping (along with memlock). Swapping can dramatically decrease performance and stability, so you should ensure it is disabled on production clusters.

### Cluster

```toml
[cluster]
name = "opensearch"
max_shards_per_node= "1000"
```

- This section configures cluster settings.
- It sets the name of the OpenSearch cluster to "opensearch".
- We can use these settings to set the `max_shards_per_node` value for OpenSearch. The default value is 2000.

### Discovery

```toml
[discovery]
minimum_master_nodes = 2
ping_unicast_hosts = ["Os Node IP 1", "Os Node IP 2"]
```

- This section configures discovery settings. It sets the minimum number of master-eligible nodes required to form a cluster, specifies the unicast hosts for node discovery, and sets the ping timeout.
- Set `ping_unicast_hosts` to pass an initial list of hosts to perform discovery when a new node start. The default list of hosts is ["127.0.0.1", "[::1]"]
- Set `minimum_master_nodes` to prevent the "split brain" by configuring the majority of nodes (total number of nodes / 2 + 1):

### Gateway

```toml
[gateway]
recover_after_nodes = ""
```

Set `recover_after_nodes` to block initial recovery after a full cluster restart until N nodes start.

### Logger

```toml
[logger]
level = "info"
```

This section configures logger settings. Allowed levels are trace, debug, info, warn, error, and fatal.

### Node

```toml
[node]
max_local_storage_nodes = 1
name = ""
```

- Use `max_local_storage_nodes` to disable starting multiple nodes on a single system:
- Use a descriptive name for the node by setting the `name` field

### OpenSearch Auth

```toml
[opensearch_auth]
admin_password = "admin"
admin_username = "admin"
hashed_password = "<your-hashed-password>"
```

This section configures OpenSearch authentication settings. It sets the admin username and password and provides a hashed version of the password.

### Path

```toml
[path]
data = ""
logs = "logs"
repo = ""
```

- Use `data` to set the path to the directory where to store the data (separate multiple locations by comma)
- Use `logs` to set the path to your log files
- Use `repo` to register the snapshot repository using OpenSearch. It is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location in the path.repo setting on all master and data nodes.

### Plugin Security

```toml
[plugins.security]
allow_default_init_securityindex = true
allow_unsafe_democertificates = true
check_snapshot_restore_write_privileges = true
enable_snapshot_restore_privilege = true
nodes_dn = "- <Common Name of Public Key>"
```

This section configures security plugin settings. It allows the default initialization of the security index, and unsafe demo certificates, checks snapshot and restore write privileges, enables snapshot and restore privileges, and specifies the nodes' distinguished names (DNs).

### Plugin Security Audit

```toml
[plugins.security.audit]
type = "internal_opensearch"
```

This section configures security audit settings. It specifies the type of audit logging as "internal_opensearch".

### Plugin Security Authcz

```toml
[plugins.security.authcz]
admin_dn = "- <Common Name of Admin Public Key>"
```

This section specifies the distinguished name (DN) of the admin user.

### Plugin Security Restapi

```toml
[plugins.security.restapi]
roles_enabled = "[\"all_access\", \"security_rest_api_access\"]"
```

This section configures security REST API settings. It enables certain roles, such as "all_access" and "security_rest_api_access".

### Plugin Security SSL HTTP

```toml
[plugins.security.ssl.http]
enabled = true
pemcert_filepath = "certificates/node1.pem"
pemkey_filepath = "certificates/node1-key.pem"
pemtrustedcas_filepath = "certificates/root-ca.pem"
```

This section configures SSL/TLS settings for HTTP. It enables SSL/TLS, specifying the certificate's file paths, private key, and trusted CA certificates.

### Plugin Security SSL Transport

```toml
[plugins.security.ssl.transport]
enforce_hostname_verification = false
pemcert_filepath = "certificates/node1.pem"
pemkey_filepath = "certificates/node1-key.pem"
pemtrustedcas_filepath = "certificates/root-ca.pem"
resolve_hostname = false
```

This section configures SSL/TLS settings for transport layer communication. It disables hostname verification, specifies the file paths for the certificate, private key, and trusted CA certificates, and disables hostname resolution.

### Plugin Security System Indices

```toml
[plugins.security.system_indices]
cloud_aws_signer = ""
enabled = true
indices = "[\".opendistro-alerting-config\", \".opendistro-alerting-alert*\", \".opendistro-anomaly-results*\", \".opendistro-anomaly-detector*\", \".
opendistro-anomaly-checkpoints\", \".opendistro-anomaly-detection-state\", \".opendistro-reports-*\", \".opendistro-notifications-*\", \".opendistro-notebooks\", \".opensearch-observability\", \".opendistro-asynchronous-search-response*\", \".replication-metadata-store\"]"
```

This section configures system indices for the security plugin. It specifies the system indices that are enabled for various functionalities.

### Runtime

```toml
[runtime]
es_java_opts = ""
es_startup_sleep_time = ""
g1ReservePercent = "25"
initiatingHeapOccupancyPercent = "15"
maxHeapsize = "2g"
max_locked_memory = "unlimited"
max_open_files = ""
minHeapsize = "2g"
```

This section configures runtime settings. It specifies various Java runtime options and heap sizes.

### S3 Client Default

```toml
[s3.client.default]
endpoint = "s3.amazonaws.com"
max_retries = "3"
protocol = "https"
read_timeout = "60s"
use_throttle_retries = true
```

This section configures the default S3 client settings. It specifies the S3 endpoint, the maximum number of retries, the protocol (HTTPS), the read timeout, and whether to use throttle retries.

### TLS

```toml
[tls]
admin_cert = "----Enter Admin Public Key----"
admin_key = "----Enter Admin Private Key----"
rootCA = "----Enter Root CA----"
ssl_cert = "----Enter Public Key----"
ssl_key = "Enter Private Key----"
```

This section configures TLS settings. It specifies the file paths for the admin certificate, admin private key, root CA certificate, SSL certificate, and SSL private key.

### Full config for OpenSearch node

```toml
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

#### Example

To increase max heap size:

- Create a heap.toml file with the below contents on bastion:

```toml
[runtime]
maxHeapsize = "2g"
```

- Run the patch command `chef-automate config patch log.toml --os` to apply the patch.

### Centralized Logs

Take a tour of the main page to know about [Centralized logs]((/automate/centralizing_log/)).
