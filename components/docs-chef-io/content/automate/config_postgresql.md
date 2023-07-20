+++
title = "HA PostgreSQL Node Config"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA PostgreSQL Node Config"
    parent = "automate/deploy_high_availability/configuration"
    identifier = "automate/deploy_high_availability/configuration/config_postgresql.md HA PostgreSQL Node Config"
    weight = 220
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

## Configurations

The PostgreSQL node in Automate HA provides various configuration options you can configure to customize its behavior and meet specific requirements. This guide documents all the configurations that you can patch.

The detailed document about how these individual properties affect the system is at [Official PostgreSQL docs](https://www.postgresql.org/docs/13/)

Patch the below configuration to PostgreSQL nodes. Please add the values you want to patch to a `config.toml` file and run the `chef-automate config patch config.toml --pg` from the bastion.

### Logging

```bash
log_level = "ERROR"
log_line_prefix = "%t [%p]: [%l-1] user=%u,db=%d,client=%h %r (%x:%e)"
logging_collector = "on"
```

In the above snippet:

- `log_level` controls which message levels are written to the server log. Valid values are DEBUG5, DEBUG4, DEBUG3, DEBUG2, DEBUG1, INFO, NOTICE, WARNING, ERROR, LOG, FATAL, and PANIC. The default is WARNING.
- `log_line_prefix` is a printf-style string output at the beginning of each log line.
- `logging_collector` enables the logging collector, which is a background process that captures log messages sent to stderr and redirects them into log files

### Checkpoints

```bash
checkpoint_timeout = "5min"
max_wal_size = "1GB"
min_wal_size = "80MB"
```

In the above snippet:

- `checkpoint_timeout` is the maximum time between automatic WAL checkpoints. The valid range is between 30 seconds and one day. The default is five minutes (5min). Increasing this parameter can increase the amount of time needed for crash recovery.
- `max_wal_size` is the maximum size to let the WAL grow during automatic checkpoints. The default is 1 GB. Increasing this parameter can increase the amount of time needed for crash recovery. This parameter can only be set in the PostgreSQL.conf file or the server command line.
- `min_wal_size` can ensure enough WAL space is reserved to handle spikes in WAL usage, for example, when running large batch jobs. If this value is specified without units, it is taken as megabytes. The default is 80 MB.

### Wal Keep Size

```bash
wal_keep_size = 1600
```

`wal_keep_size` specifies the minimum size of past log file segments kept in the pg_wal directory if a standby server needs to fetch them for streaming replication. If wal_keep_size is zero (the default), the system doesn't keep extra segments for standby purposes. Hence, the number of old WAL segments available to standby servers is a function of the location of the previous checkpoint and the status of WAL archiving.

### Lock Management

```bash
max_locks_per_transaction = 64
```

The shared lock table tracks locks on `max_locks_per_transaction` * (max_connections + max_prepared_transactions) objects (e.g., tables); hence, no more than this many distinct objects can be locked at any time. This parameter controls the average number of object locks allocated for each transaction; individual transactions can lock more objects as long as the locks of all transactions fit in the lock table. This is not the number of rows that can be locked; that value is unlimited. The default is 64.

When running a standby server, you must set this parameter to the same or higher value than on the master server. Otherwise, queries will not be allowed on the standby server.

### Max Connections

```bash
max_connections = 350
```

In the above snippet, `max_connections` determines the maximum number of concurrent connections to the database server. The default for Automate is 350 connections.

When running a standby server, you must set this parameter to the same or higher value than on the master server. Otherwise, queries will not be allowed on the standby server.

### Pg Dump

```bash
[pg_dump]
enable = true
path = "/mnt/automate_backups/postgresql/pg_dump"
```

This section configures pg_dump, a PostgreSQL utility for performing database backups. It enables pg_dump and specifies the path where the backups should be stored.

### Replication

```bash
[replication]
lag_health_threshold = 20480
max_replay_lag_before_restart_s = 180
name = "replication"
password = "replication"
```

This section configures replication settings. It sets the lag health threshold to 20480 bytes, the maximum allowed replication lag. It also specifies the maximum replay lag before restarting replication and provides the replication name and password.

### SSL

```bash
[ssl]
enable = true
issuer_cert = "----Enter Root CA----"
ssl_cert = "----Enter Public Key----"
ssl_key = "----Enter Private Key----"
tls_ciphers = "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256"
```

This section configures SSL/TLS settings. It enables SSL and specifies the root CA (issuer) certificate, the public key certificate, the private key, and the allowed TLS ciphers.

### User

```bash
[superuser]
name = "admin"
password = "admin"
```

This section specifies the username and password for the superuser (administrator) account.

### Wal Archive

```bash
[wal_archive]
enable = false
path = "/mnt/automate_backups/postgresql/archive"
```

This section configures WAL archiving. It specifies whether WAL archiving is enabled (false in this case) and the path where archived WAL files should be stored.

### Full config for PostgreSQL node

```bash
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

To increase the log level to DEBUG1:

- Create a log.toml file with the below contents on bastion:

```toml
log_level = "DEBUG1"
```

- Run the patch command `chef-automate config patch log.toml --pg` to apply the patch.

### Centralized Logs

Take a tour of the main page to know about [Centralized logs]((/automate/centralizing_log/)).
