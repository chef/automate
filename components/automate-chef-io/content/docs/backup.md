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
    weight = 70
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

### Storing a Filesystem Backup in a Single-file Archive
Some users prefer to store backups offline in single-file archives. Such archives must
include both the configuration data and the reporting data contained in the standard
backup.

The [configured]({{< ref "backup.md#backup-to-filesystem" >}}) backup directory contains both the timestamp-based directory for the configuration, and the reporting data stored in the `automate-elasticsearch-data` directory.
You will need to archive both of these directory types in any single-file backups you create.

A timestamp-based directory can be distinguished from the `automate-elasticsearch-data` directory by its file name format, such as `20180518010336`.

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
Restore Chef Automate to an instance on which Automate is not already installed.

### Restore From a Filesystem Backup

#### Prerequisites
1. The `chef-automate` command line utility must be installed on the host on which you
   plan to run the restore.
1. Chef Automate must have access to a backup directory in the [configured location]({{< ref
"backup.md#backup-to-filesystem" >}}):
  1. If you used a network-attached filesystem backup, mount the shared backup directory to the same mount point that was configured when the backup was created.
  1. If the backup directory is not a network-attached filesystem, copy the backup directory to the location that was [configured]({{< ref
"backup.md#backup-to-filesystem" >}}) when the backup was created.
  1. If you are restoring from a single-file backup archive, copy your archive to the machine on which you wish to restore and extract it to the [configured backup directory]({{< ref "backup.md#backup-to-filesystem" >}}).

#### Restore in an Internet-Connected Environment
If you have [configured the backup directory]({{< ref "backup.md#backup-to-filesystem" >}}) to a directory that is not default (`/var/opt/chef-automate/backups`) you must supply the backup directory; if you do not provide a backup ID, Chef Automate will use the most recent backup in the backup directory.

To restore on a fresh host:

```shell
chef-automate backup restore </path/to/backups/>BACKUP_ID
```

To restore on a host on which Automate has already been installed:

```shell
chef-automate backup restore </path/to/backups/>BACKUP_ID --skip-preflight
```

#### Restore in an Airgapped Environment
To restore a backup of an [airgapped installation]({{< relref "airgapped-installation.md" >}}), you must specify the [Airgap Installation Bundle]({{< relref "airgapped-installation.md#create-an-airgap-installation-bundle" >}}) used by the installation. If you have [configured the backup directory]({{< ref "backup.md#backup-to-filesystem" >}}) to a directory that is not default (`/var/opt/chef-automate/backups`) you must supply the backup directory; if you do not provide a backup ID, Chef Automate will use the most recent backup in the backup directory.

To restore on a fresh host:

```shell
chef-automate backup restore --airgap-bundle </path/to/bundle> </path/to/backups/>BACKUP_ID
```

To restore on a host on which Automate has already been installed:

```shell
chef-automate backup restore --airgap-bundle </path/to/bundle> </path/to/backups/>BACKUP_ID --skip-preflight
```

### Restore From an S3 Backup
To restore on a fresh host:

```shell
chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID
```

To restore on a host on which Automate has already been installed:

```shell
chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight
```

If the restore is successful, you will see the following message at the end of the status output:

```shell
Success: Restored backup 20180517223558
```

### Update the FQDN
Chef Automate's restore process expects to run on a host with the same FQDN as
the host on which the backup was created. If the host on which the restore has run has a
different FQDN, patch the FQDN and TLS certificate once the restore has completed:

1. On the host on which you are restoring Chef Automate, run

    ```shell
    chef-automate init-config
    ```
to generate a configuration file for that host.
1. Create a `patch.toml` file that contains the FQDN, TLS certificate, and TLS key entries from the
   generated configuration:
   ```toml
    [global.v1]
    fqdn = "<new-fqdn>"

    [[global.v1.frontend_tls]]
    # The TLS certificate for the load balancer frontend.
    cert = """-----BEGIN CERTIFICATE-----
    <certificate-for-new-fqdn>
    -----END CERTIFICATE-----
    """

    # The TLS RSA key for the load balancer frontend.
    key = """-----BEGIN RSA PRIVATE KEY-----
    <key-for-new-fqdn>
    -----END RSA PRIVATE KEY-----
    """
   ```
1. Run
   ```shell
   chef-automate config patch </path/to/patch.toml>
   ```
   to patch the restored Automate
   installation with the new FQDN, TLS certificate, and TLS key configuration entries.


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
