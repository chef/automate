+++
title = "Backup & Restore"
description = "Procedures for Backing Up Chef Automate Data"
date = 2018-03-26T15:27:52-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 50
+++
## Configuring Backups

Backups are crucial for protecting your data from catastrophic loss and preparing a recovery procedure.
Back up Chef Automate either to a filesystem or an S3 bucket

### Backup to Filesystem

Backups can be stored in a configurable backup directory. The backup directory should be on network attached storage or synced periodically to a disk on another machine, to ensure that you can restore from your backup data during a hardware failure.

We recommend that you configure a backup directory on a disk with sufficient free space.

The the default backup directory is `/var/opt/chef-automate/backups`. If it does not exist it will be created during deployment.

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

The following IAM policy describes the minimum permissions Automate requires to successfully run backup and restore operations.

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

## Create a Backup

Make a backup with the following command:

```shell
chef-automate backup create
```

The command will show the backup progress for each service. If the backup is successful, you will see a success message and timestamp of the backup:

```shell
Success: Created backup 20180518010336
```

## Listing Backups

You can list existing backups with the `backup list` command:

```shell
chef-automate backup list
```

The output will show each backup, and its age:

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

where `bucket_name` is the name of the S3 bucket `base_path` an optional path within the bucket where the backups live.

## Restoring Chef Automate

Restore Chef Automate to an instance on which Automate is not already installed. The directory used for restoring Chef Automate must have access to the filesystem used for the backup. If you used a network-attached filesystem backup, mount the shared backup directory to the same mount point that was configured when the backup was created. If the backup directory is not a network-attached filesystem, copy the backup directory to the location that was configured when the backup was created.

If you have configured the backup directory to a directory that is not default (`/var/opt/chef-automate/backups`) you must supply the backup directory:

```shell
chef-automate backup restore /path/to/backups/BACKUP_ID
```

If backups are stored in S3:

```shell
chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID
```

To restore a backup of an [airgapped installation]({{< relref "airgapped-installation.md" >}}), you must specify the [Airgap Installation Bundle]({{< relref "airgapped-installation.md#create-an-airgap-installation-bundle" >}}) the installation is using:

```shell
chef-automate backup restore /path/to/backups/BACKUP_ID --airgap-bundle </path/to/bundle>
```

If the restore is successful, you will see the following message at the end of the status output:

```shell
Success: Restored backup 20180517223558
```

## Deleting Backups

To delete backups from a running instance of Chef Automate, first find the relevant backup ID with `chef-automate backup list` and then delete it using `chef automate backup delete ID`.

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
