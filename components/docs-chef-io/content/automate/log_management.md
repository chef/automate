+++
title = "Log Management"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Log Management"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/log_management.md Log Management"
    weight = 40
+++

Chef Automate uses `systemd`. Log management is performed according to the configuration defined for the system service `journald`.

## Viewing Logs

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

## Configuring Log Level

You can configure Chef Automate log level for all services by creating a TOML file and configuring the log level. By default each service will initialize at the "info" level but can be any of 'debug, 'info', 'warning', 'panic', or 'fatal'.

```shell
[global.v1.log]
level = "debug"
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.


## Configuring Log Rotation and Retention

Log rotation and retention settings are managed at a system level using `journald`. At this point, `journald` does not support log retention policies at a granular level for units within itself. See the [man page](https://www.freedesktop.org/software/systemd/man/journald.conf.html) for more configuration options in `/etc/systemd/journald.conf`.


## Configuring Rate Limiter

The rate limiter is used to control the volume of log messages that are written to the journal. You can configure Rate Limiter by creating a TOML file.

```shell
[global.v1.log]
rate_limit_interval = 30
rate_limit_burst = 10000
```

Then run `chef-automate config patch </path/to/your-file.toml>` to deploy your change.

Set the following values:

- `rate_limit_interval`: This defines the time interval for rate-limiting in seconds. For example, if it's set to 30s, journald will track messages within each 30-seconds window. Default value will be same as the `journald` default value, which is `30` [Journal Page](https://www.freedesktop.org/software/systemd/man/latest/journald.conf.html#RateLimitIntervalSec=).
- `rate_limit_burst`: This sets the maximum number of messages allowed within the interval defined by rate_limit_interval. If more messages are received within the interval, they will be temporarily suppressed to avoid spamming the journal. Default value will be same as the `journald` default value, which is `10000` [Journal Page](https://www.freedesktop.org/software/systemd/man/latest/journald.conf.html#RateLimitIntervalSec=).

{{< warning >}}
By enabling this configuration it may lead to increasing disk utilization.
{{< /warning >}}