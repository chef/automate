+++
title = "Log Management"
description = "Chef Automate 2.0 sends all log events to `stdout` and `stderr` allowing the process supervisor to handle logs in a standard way."
draft = false
bref = "Chef Automate 2.0 sends all log events to `stdout` and `stderr` allowing the process supervisor to handle logs in a standard way."
[menu]
  [menu.docs]
    parent = "configuring_automate"
    weight = 40
+++

Chef Automate 2.0 uses `systemd`. Log management is performed according to the configuration defined for the system service `journald`.

### Viewing Logs

To view the logs you can run:

```shell
journalctl -u chef-automate
```

To follow the logs in realtime:

```shell
journalctl -u chef-automate -f
```

For information on changing the output, please refer to the man page or run:

```shell
journalctl --help
```

### Configuring Log Level

You can configure Chef Automate log level for all services by creating a `.toml` and configuring the log level. By default each service will initialize at the "info" level but can be any of 'debug, 'info', 'warning', 'panic', or 'fatal'.

```shell
[global.v1.log]
level = "debug"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

### Configuring Log Rotation and Retention

Log rotation and retention settings are managed at a system level using `journald`. At this point, `journald` does not support log retention policies at a granular level for units within itself. See the [man page](https://www.freedesktop.org/software/systemd/man/journald.conf.html) for more configuration options in `/etc/systemd/journald.conf`.
