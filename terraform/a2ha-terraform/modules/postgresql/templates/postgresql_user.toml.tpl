port = ${listen_port}
[wal_archive]
enable = ${postgresql_wal_archive_enabled}
path = "${postgresql_wal_archive_fs_path}"
[pg_dump]
enable = ${postgresql_pg_dump_enabled}
path = "${postgresql_pg_dump_fs_path}"
[ssl]
enable = ${postgresql_ssl_enable}
