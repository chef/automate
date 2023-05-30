+++
title = "HA OpenSearch Node config"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA OpenSearch Node config"
    parent = "automate/deploy_high_availability/configuration"
    identifier = "automate/deploy_high_availability/configuration/config_opensearch.md HA OpenSearch Node config"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

The below configurations can be patched to OpenSearch nodes. Please add the values you want to patch to a `config.toml` file and run `chef-automate config patch config.toml --os` from bastion node.

### Sample Config for OpenSearch node

```toml
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
ping_unicast_hosts = ["Os Node IP 1", "Os Node IP 1"]
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
- Create a heap.toml file with below contents on bastion:
  ```toml
  [runtime]
  maxHeapsize = "2g"
  ```
- Run patch command `chef-automate config patch log.toml --os` to apply the patch.

#### Centralised Logs

Click [here](/automate/centralizing_log/) for more information