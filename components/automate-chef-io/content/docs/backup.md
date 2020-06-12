+++
title = "Backup"
description = "Procedures for Backing Up Chef Automate Data"
date = 2018-03-26T15:27:52-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 70
+++

Backups are crucial for protecting your data from catastrophic loss and preparing a recovery procedure. The command `chef-automate backup create` creates a single backup that contains data for all products deployed with Chef Automate, including [Chef Infra Server]({{< ref "infra-server.md" >}}) and [Chef Habitat Builder on-prem]({{< ref "on-prem-builder.md" >}}). By default, Chef Automate stores backups to the filesystem in the directory `/var/opt/chef-automate/backups`. You can also configure Chef Automate to store backups in S3.

## Setting Up Backups

### Backup Space Requirements

This amount of space needed for a backup varies depending on your Chef Automate use. You need enough free space for:

* Complete copies of each Chef Automate service PostgreSQL database.
* Complete copies of your configuration files
* Elasticsearch snapshots of your Chef Automate configuration and data, such as converge, scan, and report data. You will need enough disk space for the each Elasticsearch snapshot and the delta--or the list of changes--for each successive snapshot.
* Chef Habitat Builder artifacts

### Backup to a Filesystem

Backups can be stored in a configurable backup directory. The backup directory should be on network-attached storage or synced periodically to a disk on another machine, to ensure that you can restore from your backup data during a hardware failure.

The default backup directory is `/var/opt/chef-automate/backups`. If it does not exist, the deployment process creates it.

To configure your Chef Automate installation's backup directory to another location:

1. Create a `backup_config.toml` file in your current directory with the following content, replacing `/path/to/backups` with the path to your backup directory:

    ```toml
    [global.v1.backups.filesystem]
      path = "/path/to/backups"
    ```

2. Run the following command to apply your configuration:

    ```shell
    chef-automate config patch backup_config.toml
    ```

    Once this command is applied, the `backup_config.toml` file is no longer necessary, and can be removed.

#### Store a Filesystem Backup in a Single-file Archive

Some users prefer to store backups offline in single-file archives. Such archives must
include both the configuration data and the reporting data contained in the standard
backup.

The [configured backup directory]({{< ref "backup.md#backup-to-a-filesystem" >}}) contains both the timestamp-based directory for the configuration, and the reporting data stored in the `automate-elasticsearch-data` directory.
You must archive both of these directory types in any single-file backups you create.

A timestamp-based directory can be distinguished from the `automate-elasticsearch-data` directory by its file name format, such as `20180518010336`.

Because externally-deployed Elasticsearch nodes will not have access to Automate's built-in backup storage services, you must configure Elasticsearch backup settings separately from Automate's primary backup settings. You can configure backups to use either the local filesystem or S3.

### Backup to S3

Backups can be stored in an existing S3 bucket.

The following settings are supported:

```toml
[global.v1.backups]
  location = "s3"
[global.v1.backups.s3.bucket]
  # name (required): The name of the bucket
  name = "<bucket name>"

  # endpoint (required): The endpoint for the region the bucket lives in.
  # See https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
  endpoint = "https://<region endpoint>"

  # base_path (optional):  The path within the bucket where backups should be stored
  # If base_path is not set, backups will be stored at the root of the bucket.
  base_path = "<base path>"

[global.v1.backups.s3.credentials]
  # Optionally, AWS credentials may be provided. If these are not provided, IAM instance
  # credentials will be used. It's also possible for these to be read through the standard
  # AWS environment variables or through the shared AWS config files.
  access_key = "<access_key>"
  secret_key = "<secret_key>"
  session_key = "<session_key>"

[global.v1.backups.s3.ssl]
  # root_cert (optional): The root certificate used for SSL validation.
  # For S3 compatible APIs, you can set the SSL root cert if needed
  root_cert = """
  -----BEGIN CERTIFICATE-----
  ...
  -----END CERTIFICATE-----
```

#### S3 Permissions

The following IAM policy describes the lowest permissions Chef Automate requires to run backup and restore operations.

```JSON
{
  "Statement": [
    {
      "Action": [
        "s3:ListBucket",
        "s3:GetBucketLocation",
        "s3:ListBucketMultipartUploads",
        "s3:ListBucketVersions"
      ],
      "Effect": "Allow",
      "Resource": [
        "arn:aws:s3:::automate-backups.example.com"
      ]
    },
    {
      "Action": [
        "s3:GetObject",
        "s3:PutObject",
        "s3:DeleteObject",
        "s3:AbortMultipartUpload",
        "s3:ListMultipartUploadParts"
      ],
      "Effect": "Allow",
      "Resource": [
        "arn:aws:s3:::automate-examples.example.com/*"
      ]
    }
  ],
  "Version": "2012-10-17"
}
```

## Backup Commands

### Create a Backup

Make a backup with the [`backup create`]({{< ref "cli-chef-automate/#chef-automate-backup-create" >}}) command:

```shell
chef-automate backup create
```

The command shows the backup progress for each service. A successful backup displays a success message containing the timestamp of the backup:

```shell
Success: Created backup 20180518010336
```

Incorrect directory permissions may cause filesystem backups to fail. Run the
[`fix-repo-permissions` command]({{< ref "cli-chef-automate/#chef-automate-backup-fix-repo-permissions" >}}) to address such issues:

```shell
sudo chef-automate backup fix-repo-permissions <path>
```

### List Backups

You can list existing backups with the [`backup list`]({{< ref "cli-chef-automate/#chef-automate-backup-list" >}}) command:

```shell
chef-automate backup list
```

The output shows each backup and its age:

```shell
        Backup        State  Age
20180508201548    completed  8 minutes old
20180508201643    completed  8 minutes old
20180508201952    completed  4 minutes old
```

By default, this command communicates with your running Chef Automate installation to list the backups. If the Chef Automate installation is down, you can still list the backups.

To list filesystem backups:

```shell
chef-automate backup list /var/opt/chef-automate/backups
Listing backups from local directory /var/opt/chef-automate/backups
        Backup        State  Age
20180508201548    completed  12 minutes old
20180508201643    completed  11 minutes old
20180508201952    completed  8 minutes old
```

For backups stored in an S3 bucket, use:

```shell
chef-automate backup list s3://bucket_name/base_path
```

where `bucket_name` is the name of the S3 bucket and `base_path` is an optional path within the bucket where the backups live.

### Delete Backups

To delete backups from a running instance of Chef Automate, first find the relevant backup ID with `chef-automate backup list` and then delete it using [`chef automate backup delete ID`]({{< ref "cli-chef-automate/#chef-automate-backup-delete" >}}).

```shell
chef-automate backup list
        Backup        State  Age
20181026183901    completed  1 minute old
20181026183954    completed  33 seconds old
20181026184012    completed  15 seconds old
```

You can delete a single backup with `chef-automate backup delete`:

```shell
chef-automate backup delete 20181026183901
The following backups will be permanently deleted:
20181026183901
Are you sure you want to continue? (y/n)
y
Success: Backups deleted
```

To delete multiple backups:

```shell
chef-automate backup delete 20181026183954 20181026184012
The following backups will be permanently deleted:
20181026183954
20181026184012
Are you sure you want to continue? (y/n)
y
Success: Backups deleted
```

Chef Automate does not provide a mechanism to prune all but a certain number of the most recent backups. You can manage this manually by parsing the output of `chef-automate backup list` and applying the command `chef-automate backup delete`. For example:

```bash
export KEEP=10; chef-automate backup list --result-json backup.json > /dev/null && jq "[.result.backups[].id] | sort | reverse | .[]" -rM backup.json | tail -n +$(($KEEP+1)) | xargs -L1 -i chef-automate backup delete --yes {}
```

## Troubleshooting
Set the log level to `debug` before re-running a failed backup to output debug info to
the Chef Automate log:

```shell
chef-automate debug set-log-level deployment-service debug
```

## References
[`chef-automate backup` command reference]({{< ref "cli-chef-automate/#chef-automate-backup" >}})
