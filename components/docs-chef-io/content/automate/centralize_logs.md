+++
title = "Centralize Chef Automate Logs to a File"

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

You can centralize Chef Automate logs to a file. This is helpful if you want to transmit the Automate log to a third-party application like Splunk or Logstash.

## Prerequisites

The following utilities must be present in your environment:

- rsyslog
- logrotate

These come by default with all the operating systems we support.

## Patch the Chef Automate configuration

To centralize the log to a file, you must patch your Automate configuration.

1. Create a TOML file with the following content on the node running Chef Automate in a standalone deployment or on the bastion host in an Automate HA cluster:

    ```toml
    [global.v1.log]
    redirect_sys_log = true
    redirect_log_file_path = "<PATH/TO/LOG/DIRECTORY>"
    compress_rotated_logs = false
    max_size_rotate_logs = "10M"
    max_number_rotated_logs = 10
    ```

    Set the following values:

    - `redirect_sys_log`: Whether to save the system logs to a file. Set to `true` to save to a file. Default value: `false`.
    - `redirect_log_file_path`: The path to the directory that you want to save the Automate log to. This value is required if `redirect_sys_log` is `true`.
    - `compress_rotated_logs`: Optional. Whether to compress log files. Valid values are `true` or `false`. Default value: `false`.
    - `max_size_rotate_logs`: Optional. The maximum size of rotated log files. The log files will rotate once the file size reaches the specified size. This accepts units in megabytes (`M`) and kilobytes (`K`). Default value: `10M`.
    - `max_number_rotated_logs`: Optional. The number of file logs you want to save. This limits the number of backup files in storage. The maximum number of log files you can store is 10 per day. Default value: `10`.

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

    After you patch the Automate configuration, Automate saves the `automate.log` files to the location specified in `redirect_log_file_path`.

### Centralize all node logs to one location

You can configure all nodes in a Chef Automate HA cluster to save log files to one log location. To do this, mount a network file system (NFS) or network-attached storage (NAS) to all the frontend and backend nodes in the Automate HA cluster, then patch the Automate configuration as described in the previous procedure.

## Consolidate logs using Splunk

Splunk consolidates all the log data and pushes it to a central, accessible, easy-to-use interface.

For information about configuring Splunk, see [Splunk's Universal Forwader documentation](https://docs.splunk.com/Documentation/Forwarder) and [Splunk's `inputs.conf` documentation](https://docs.splunk.com/Documentation/Splunk/latest/Admin/Inputsconf).

1. If have your Splunk forwarder installed, start the Splunk Universal Forwarder:

    ```bash
    cd splunkforwarder/bsh
    ./splunk start --accept-license
    ```

1. Add a monitor for the Automate log file to your Splunk forwarder:

    ```bash
    ./splunk add monitor <PATH/TO/LOG/DIRECTORY>/automate.log
    ```

    Replace `<PATH/TO/LOG/DIRECTORY>` with the path to the directory where the `automate.log` is saved.
    The default path to the `automate.log` file is `/var/tmp/automate.log`.

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
