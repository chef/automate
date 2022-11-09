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

To check the status of the Chef Automate from deployment with channel amd type, run the following command:

```bash
chef-automate status
```

## Viewing Logs

To view the logs, run the following command:

```shell
journalctl -u chef-automate
```

Click here to know more on the Log Management.

## Centrailize the Log in a Document

Centralizing the log is method where you are redirecting your log to a file and saving it in a document format. Centrailizing the log in a document format is always helpful if you want to transmit it to a third party application like **Splunk** or **Logstack**. You don't have to run the above command again and again to check the log.

THe configuration to centralize the log in a document format is shown below:

```toml
redirect_sys_log = true //centralize the log
redirect_log_file_path = "/var/tmp/" //set the location of the log file
```

In the above specifications:

- Setting the property `redirect_sys_log` to **true** will centralize the log in a document format.
- The default location of the log file is `/var/tmp/` as shown above. You can save the log file in any of your desired location and mentioned the location in the above `redirect_log_file_path` property.

Patching the above configurations to chef automate will start transmitting the `automate.log` file wherever the location is mentioned in the above file.

{{< note >}}

- The default value of `redirect_sys_log` is **false**.
- If the value of `redirect_sys_log` is set to **true**, it is necessary to mention the location of the file in `redirect_log_file_path` property or it will throw an error: `Please specify a log path location using redirect_log_file_path`.

{{< /note >}}

## Log Rotation

Centralizing log also comes with log rotation with following specifications:

```toml
redirect_sys_log = true //centralize the log
redirect_log_file_path = "/var/tmp/" //set the location of the log file

//log rotation configurations
compress_rotated_logs = true //compress the log file
max_size_rotate = "10M" //set the max size of the file
max_number_rotated_logs = 10 //number of backup files to be stored
```

In the above specifications:

- To compress the logs, set the value of `compress_rotated_logs` to **true**. The default value of `compress_rotated_logs` is **false**.
- The `max_size_rotate` property sets the limit after which you want to rotate the log. The default value of `max_size_rotate` is **10M**, i.e., the logs will rotate once file size reaches the limit of 10Mb.
- The `max_number_rotated_logs` property sets the number of file logs you want to save in your backup. As soon as the file size reaches 10Mb it creates a new backup file each time. You can limit the number of backup files in your storage using this property.

{{< note >}} The default value of `max_number_rotated_logs` is **10M**. If you don't want to keep more than one log file in your storage, set the value of `max_number_rotated_logs` to **1**. {{< /note >}}
