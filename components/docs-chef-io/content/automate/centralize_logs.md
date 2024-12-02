+++
title = "Centralize Chef Automate Logs"

draft = false
aliases = ['/automate/centralizing_log/']

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Centralize Logs"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/Centralize Log"
    weight = 42
+++

You can centralize Chef Automate logs to a file and configure log rotation and retention. This is helpful if you want to transmit the Automate log to a third-party application like Splunk or Logstash.

## Prerequisites

The following utilities must be present in your environment:

- rsyslog
- logrotate

These come by default with all the operating systems we support.

## Configure log centralization

To centralize the log to a file, you must patch your Automate configuration.

1. Create a TOML file with the following content on the node running Chef Automate in a standalone deployment or on the bastion host in an Automate HA cluster:

    ```toml
    [global.v1.log]
    redirect_sys_log = true
    redirect_log_file_path = "<PATH/TO/LOG/DIRECTORY>"
    ```

    Set the following values:

    - `redirect_sys_log`: Whether to save the system logs to a file. Set to `true` to save to a file. Default value: `false`.
    - `redirect_log_file_path`: The path to the directory that you want to save the Automate log to. This value is required if `redirect_sys_log` is `true`.

1. Patch the Chef Automate configuration.

    To patch a standalone Chef Automate node or Chef Automate HA nodes in a cluster:

    ```bash
    sudo chef-automate config patch </PATH/TO/TOML/FILE>
    ```

    To patch OpenSearch nodes in Chef Automate HA cluster:

    ```bash
    chef-automate config patch --opensearch </PATH/TO/TOML/FILE>
    ```

    To patch PostgreSQL nodes in Chef Automate HA cluster:

    ```bash
    chef-automate config patch --postgresql </PATH/TO/TOML/FILE>
    ```

    After you patch the Automate configuration, Automate saves the log files to the location specified in `redirect_log_file_path`.

## Configure log rotation and retention

To configure log rotation and retention, you must patch your Automate configuration.

1. Create a TOML file with the following content on the node running Chef Automate in a standalone deployment or on the bastion host in an Automate HA cluster:

    ```toml
    [global.v1.log]
    redirect_sys_log = true
    redirect_log_file_path = "<PATH/TO/LOG/DIRECTORY>"
    compress_rotated_logs = true
    max_size_rotate_logs = "10M"
    max_number_rotated_logs = 10
    ```

    Set the following values:

    - `redirect_sys_log`: Whether to save the system logs to a file. Set to `true` to save to a file. Default value: `false`.
    - `redirect_log_file_path`: The path to the directory that you want to save the Automate log to. This value is required if `redirect_sys_log` is `true`.
    - `compress_rotated_logs`: Whether to compress log files. Valid values are `true` or `false`. Default value: `false`.
    - `max_size_rotate_logs`: The maximum size of rotated log files. The log files will rotate once the file size reaches the specified size. This accepts units in megabytes (`M`) and kilobytes (`K`). Default value: `10M`.
    - `max_number_rotated_logs`: The number of file logs you want to save. This limits the number of backup files in storage. The maximum number of log files you can store is 10 per day. Default value: `10`.

1. Patch the Chef Automate configuration.

    To patch a standalone Chef Automate node or Chef Automate HA nodes in a cluster:

    ```bash
    sudo chef-automate config patch </PATH/TO/TOML/FILE>
    ```

    To patch OpenSearch nodes in Chef Automate HA cluster:

    ```bash
    chef-automate config patch --opensearch </PATH/TO/TOML/FILE>
    ```

    To patch PostgreSQL nodes in Chef Automate HA cluster:

    ```bash
    chef-automate config patch --postgresql </PATH/TO/TOML/FILE>
    ```

    After you patch the Automate configuration, Automate saves and rotates the log files in the location specified in `redirect_log_file_path`.

## Configure Rate Limiter

To configure Rate Limiter, you must patch your Automate configuration.

1. Create a TOML file with the following content on the node running Chef Automate in a standalone deployment or on the bastion host in an Automate HA cluster:

    ```toml
    [global.v1.log]
    redirect_sys_log = true
    redirect_log_file_path = "<PATH/TO/LOG/DIRECTORY>"
    rate_limit_interval = 600
    rate_limit_burst = 20000
    ```

    Set the following values:

    - `redirect_sys_log`: Whether to save the system logs to a file. Set to `true` to save to a file. Default value: `false`.
    - `redirect_log_file_path`: The path to the directory that you want to save the Automate log to. This value is required if `redirect_sys_log` is `true`.
    - `rate_limit_interval`: This defines the time interval for rate-limiting in seconds. For example, if it's set to 600s, rsyslog will track messages within each 600-second window. The default value will be the same as the `rsyslog` default value, which is `600` [rsyslog Page](https://www.rsyslog.com/doc/configuration/modules/imjournal.html#ratelimit-interval).
    - `rate_limit_burst`: This sets the maximum number of messages allowed within the interval defined by rate_limit_interval. If more messages are received within the interval, they will be temporarily suppressed to avoid spamming the rsyslog. The default value will be the same as the `rsyslog` default value, which is `20000` [rsyslog Page](https://www.rsyslog.com/doc/configuration/modules/imjournal.html#ratelimit-burst).

    {{< note >}}

    - Changing the `rate_limit_burst` or `rate_limit_interval` value will configure both journald and rsyslog settings as well.
    - The default values for RateLimitInterval and RateLimitBurst in `journald` are 30 seconds and 10,000 messages, respectively.
    - In `rsyslog`, the default values for RateLimitInterval and RateLimitBurst are 600 seconds and 20,000 messages, respectively.

    {{< /note >}}

    {{< warning >}}
    Enabling this configuration may lead to increased disk utilization.
    {{< /warning >}}

1. Patch the Chef Automate configuration.

    To patch a standalone Chef Automate node or Chef Automate HA nodes in a cluster:

    ```bash
    sudo chef-automate config patch </PATH/TO/TOML/FILE>
    ```

    To patch OpenSearch nodes in Chef Automate HA cluster:

    ```bash
    chef-automate config patch --opensearch </PATH/TO/TOML/FILE>
    ```

    To patch PostgreSQL nodes in Chef Automate HA cluster:

    ```bash
    chef-automate config patch --postgresql </PATH/TO/TOML/FILE>
    ```

## Centralize all node logs to one location

You can configure all nodes in a Chef Automate HA cluster to save log files to one log location.
To do this, mount a network file system (NFS) or network-attached storage (NAS) to all the frontend and backend nodes in the Automate HA cluster, then patch the Automate configuration as described in the previous procedures.

## Consolidate logs using Splunk

Splunk consolidates all the log data and pushes it to a central, accessible, easy-to-use interface.

For information about configuring Splunk, see [Splunk's Universal Forwader documentation](https://docs.splunk.com/Documentation/Forwarder) and [Splunk's `inputs.conf` documentation](https://docs.splunk.com/Documentation/Splunk/latest/Admin/Inputsconf).

1. If have your Splunk forwarder installed, start the Splunk Universal Forwarder:

    ```bash
    cd splunkforwarder/bsh
    ./splunk start --accept-license
    ```

1. Add a monitor for the Automate log files to your Splunk forwarder. For example:

    ```bash
    ./splunk add monitor <PATH/TO/LOG/DIRECTORY>/automate.log
    ```

    Replace `<PATH/TO/LOG/DIRECTORY>` with the path to the directory where the automate logs are saved.
    The default path to the  file is `/var/tmp/`.

    Splunk returns:

    ```bash
    Added monitor of '<PATH/TO/LOG/DIRECTORY>/automate.log'
    ```

    This confirms that Splunk's `input.conf` file has been updated to monitor the `automate.log` file.
    Open the `input.conf` file to confirm that the path to the `automate.log` file is correct. It should look like this:

    ```plain
    [monitor://<PATH/TO/LOG/DIRECTORY>/automate.log]
    disabled = false
    ```

1. Restart the Splunk forwarder:

    ```bash
    ./splunk restart
    ```

   After restarting, Splunk monitors the `automate.log` file in your Splunk forwarder.
