+++
title = "HA PostgreSQL Node config"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA PostgreSQL Node config"
    parent = "automate/deploy_high_availability/configuration"
    identifier = "automate/deploy_high_availability/configuration/config_postgresql.md HA PostgreSQL Node config"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

The below configurations can be patched to PostgreSQL nodes. Please add the values you want to patch to a `config.toml` file and run `chef-automate config patch config.toml --pg` from bastion.


### Sample config for PostgreSQL node

```
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

#### Example

To increase log level to DEBUG:
- Create a log.toml file with below contents on bastion:
  ```toml
  log_level = "DEBUG"
  ```
- Run patch command `chef-automate config patch log.toml --pg` to apply the patch.

#### Centralised Logs

Click [here](/automate/centralizing_log/) for more information
