+++
title = "Centralized Log"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Centralized Log"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/log_management.md Centralized Log"
    weight = 42
+++

Centralizing the log is a method to redirect your log to a separate file and save it. Centralizing the log in a file is always helpful if you want to transmit it to a third-party application like **Splunk** or **Logstash**.

To check the status of the Chef Automate from deployment with channel and type, run the following command:

```bash
chef-automate status
```

## Viewing Logs

To view the logs, run the following command:

```shell
journalctl -u chef-automate -f
```

Click [here](/automate/log_management) to learn more about Log Management.

## Prerequisites

The Operating system has two modules that should be present in your environment:

1. Rsyslog
1. Log Rotate

The above modules come by default with all the operating systems we support.

## Centralize the Log in a File

Once you centralize the log, you can run the above command sparingly to check the log. The configuration to centralize the log in a file is shown below:

```toml
[global.v1.log]
redirect_sys_log = true //centralize the log
redirect_log_file_path = "PATH OF THE LOG FILE" //set the location of the log file. syntax: /var/tmp/
```

In the above specifications:

- Setting the property `redirect_sys_log` to **true** will centralize the log in a document format.
- The default location of the log file is `/var/tmp/` as shown above. You can save the log file in your desired locations and mention the location in the above `redirect_log_file_path` property.

{{< note >}}

- The default value of `redirect_sys_log` is **false**.
- If the value of `redirect_sys_log` is set to **true**, it is necessary to mention the file's location in the `redirect_log_file_path` property, or it will throw an error: `Please specify a log path location using redirect_log_file_path`.

{{< /note >}}

### Patch the Log Configuration

Once you have created the **automate.log** file, patch the above configurations to chef automate. Patching the configuration will start transmitting the `automate.log` file wherever the location is specified in `redirect_log_file_path`.

```bash
sudo chef-automate config patch config.toml
```

## Configuring Log Rotation and Retention

The centralizing log also comes with log rotation with the following specifications:

```toml
[global.v1.log]
redirect_sys_log = true ##centralize the log
redirect_log_file_path = "/var/tmp/" ##set the location of the log file
compress_rotated_logs = true ##compress the log file
max_size_rotate_logs = "10M" ##set the max size of the file. syntax: 10M, 90k
max_number_rotated_logs = 10 ##number of backup files to be stored
```

In the above specifications:

- To compress the logs, set the value of `compress_rotated_logs` to **true**. The default value of `compress_rotated_logs` is **false**.
- The `max_size_rotate` property sets the limit after which you want to rotate the log. The default value of `max_size_rotate` is **10M**, i.e., the logs will rotate once the file size reaches the limit of 10Mb. You can also set the value of `max_size_rotate` in **KB**.
- The `max_number_rotated_logs` property sets the number of file logs you want to save in your backup. Using this property, you can limit the number of backup files in your storage. The maximum number of log files you can store is **10** per day.

{{< note >}} The default value of `max_number_rotated_logs` is **10M**. If you don't want to keep more than one log file in your storage, set the value of `max_number_rotated_logs` to **1**. {{< /note >}}

## Configure Logs for OpenSearch in Chef Automate HA

To configure the centralizing log feature for OpenSearch in Chef Automate HA, run the following command:

```toml
chef-automate config patch --opensearch <file/path/to/toml>
```

## Configure Logs for PostgreSQL in Chef Automate HA

To configure the centralizing log feature for PostgreSQL in Chef Automate HA, run the following command:

```toml
chef-automate config patch --postgresql <file/path/to/toml>
```

## Log Consolidation using Splunk

**Splunk** consolidates all the log data and pushes it to a central, accessible, easy-to-use interface. Here, the **Splunk forwarder** here will view the data outside the local environment. Assuming that you already have your Splunk forwarder installed, run the following command to start **Splunk Universal forwarder**:

```bash
[system@ABC]# cd splunkforwarder/bin
[system@ABC]# ./splunk start --accept-license
```

The above command will start your spunk forwarder.

Now, add a monitor for log file on your Splunk Forwarder as shown below:

```bash
[system@ABC]# ./splunk add monitor /var/tmp/automate.log
```

In the above command, **/var/tmp/** is the location of the `automate.log`, which you can change accordingly. Running the above command will show the following:

```bash
Added monitor of '/var/tmp/automate.log'
```

The above statement confirms that the value of the monitor in `input.conf` has been updated to `var/tmp/automate.log`. Open the `input.conf` file just to confirm whether the content in the file is as expected. The expected content is:

```bash
[monitor:///var/tmp/automate.log]
disabled = false
```

Restart your splunk forwarder using the following command:

```bash
[system@ABC]# ./splunk restart
```

The above step will let you view the `automate.log` file in your splunk forwarder. Click [here](https://statics.teams.cdn.office.net/evergreen-assets/safelinks/1/atp-safelinks.html) to know about the Splunk Forwarder.

## Log File

**NOTE:** `redirect_log_file_path` is a directory where **automate.log** file gets created.

## Frequently Asked Questions

### How can Automate HA be set up and configured to write the logs of all the nodes to a centralized log file?

This requires Automate HA to do the following steps to set up the log location and configure it.

- Mount and NFS or network-attached storage to all the nodes. Create a log location using the following steps:
  - Create an EFS.
  - Mount the EFS in all the Frontend and Backend nodes of Automate HA.
- Configure Automate HA nodes to redirect the journal logs to the same file inside the mounted location.
  - To set up `logrotate` on Automate `sudo chef-automate config patch <patch_conf.toml> --a`
  - To set up `logrotate` on Chef Server `sudo chef-automate config patch <patch_conf.toml> --cs`
  - To set up `logrotate` on OpenSearch `sudo chef-automate config patch <patch_conf.toml> --os`
  - To set up `logrotate` on Postgres `sudo chef-automate config patch <patch_conf.toml> --pg`
- Check if all the node logs are shown in a single log file.
